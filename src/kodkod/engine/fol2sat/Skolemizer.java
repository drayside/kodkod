package kodkod.engine.fol2sat;

import static kodkod.ast.BinaryFormula.Operator.AND;
import static kodkod.ast.BinaryFormula.Operator.IFF;
import static kodkod.ast.BinaryFormula.Operator.IMPLIES;
import static kodkod.ast.BinaryFormula.Operator.OR;
import static kodkod.ast.QuantifiedFormula.Quantifier.ALL;
import static kodkod.ast.QuantifiedFormula.Quantifier.SOME;

import java.util.IdentityHashMap;
import java.util.Map;
import java.util.Set;

import kodkod.ast.BinaryExpression;
import kodkod.ast.BinaryFormula;
import kodkod.ast.ComparisonFormula;
import kodkod.ast.Comprehension;
import kodkod.ast.ConstantExpression;
import kodkod.ast.ConstantFormula;
import kodkod.ast.Decl;
import kodkod.ast.Decls;
import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.IfExpression;
import kodkod.ast.LeafExpression;
import kodkod.ast.MultiplicityFormula;
import kodkod.ast.Node;
import kodkod.ast.NotFormula;
import kodkod.ast.QuantifiedFormula;
import kodkod.ast.Relation;
import kodkod.ast.RelationPredicate;
import kodkod.ast.UnaryExpression;
import kodkod.ast.Variable;
import kodkod.ast.visitor.DepthFirstReplacer;
import kodkod.ast.visitor.DepthFirstVoidVisitor;
import kodkod.ast.visitor.ReturnVisitor;
import kodkod.engine.bool.BooleanMatrix;
import kodkod.engine.bool.BooleanValue;
import kodkod.instance.Bounds;
import kodkod.instance.Universe;
import kodkod.util.IdentityHashSet;
import kodkod.util.IndexedEntry;
import kodkod.util.IntSet;
import kodkod.util.Ints;

/**
 * Skolemizes away existential quantifiers.  Specifically,
 * each top-level existentially quantified formula whose
 * declared variables do not depend on any other variables
 * is transformed as follows:  some x: E | F(x) becomes
 * r_x in E && some r_x && F(r_x), where r_x is a freshly
 * allocated relation (i.e. skolem constant). 
 * 
 * @author Emina Torlak
 */
final class Skolemizer {

	private Skolemizer() {}
	
	/**
	 * Skolemizes the given formula using the given bounds and the information about its
	 * structure.
	 * @requires sharedNodes = {n: Node | #(n.~children & formula.*children) > 1 }
	 * @requires Relation & formula.^children in bounds.relations
	 * @effects sharedNodes are modified to reflect the structure of the returned (skolemized) formula
	 * @effects upper bound mappings for skolem constants are added to the bounds
	 * @return skolemization of the given formula
	 * @throws NullPointerException - any of the arguments are null
	 * @throws IllegalArgumentException - some Relation & this.formula.^children - bounds.relations
	 * @throws UnsupportedOperationException - sharedNodes or bounds are unmodifiable
	 */
	static Formula skolemize(Formula formula, Set<Node> sharedNodes, Bounds bounds) {
		Formula ret;
		final EQFDetector detector = new EQFDetector(sharedNodes);
		formula.accept(detector);
//		System.out.println(detector.formulas);
		
		if (detector.formulas.isEmpty()) {
			ret = formula;
		} else {
			final EQFReplacer replacer = new EQFReplacer(detector.formulas, bounds, sharedNodes);
			ret = formula.accept(replacer);
			ret = ret.and(replacer.skolemFormula);
//			ret = formula;
//			System.out.println(formula);
//			System.out.println(ret);
//			System.out.println(bounds);
		}
		return ret;
	}
	
	/**
	 * Given a set of existentially quantified formulas, s, and a Bounds b,
	 * an EQFReplacer replaces the given formulas with their skolemizations and
	 * modifies b to include upper bounds for the skolem constants.  Each given formula 
	 * f is replaced with its body, in which all occurences
	 * of d.variable are replaced by the skolem constant for d.variable.
	 * 
	 * @specfield root: Formula // an EQFReplaces should only be applied to top once
	 * @specfield eqfs: set QuantifiedFormula 
	 * @specfield bounds: Bounds
	 * @invariant eqfs = { q: root.*children & QuantifiedFormula | 
	 *                      all path: children | q in root.*path => 
	 *                       all q': root.*path | 
	 *                         (q'.quantifier = SOME => #{root.*path & NotFormula} % 2 = 0) &&
	 *                         (q'.quantifier = ALL => #{root.*path & NotFormula} % 2 = 1) }
	 * @invariant Relation & root.^children in bounds.relations   
	 */
	private static final class EQFReplacer extends DepthFirstReplacer {
		private final Set<QuantifiedFormula> eqfs;
		private final Map<Node,Node> cache;
		private Environment<LeafExpression> env;
		/* the allocator used to determine the upper bounds for skolem constants;
		 * the upper bounds for skolem constants will be added to allocator.bounds */
		private final BooleanConstantAllocator.Overapproximating allocator;
		/* the conjunction that constrains all the skolem constants; i.e.
		 * for each decl->r in this.skolems, the conjunction contains the formula
		 * 'r in decl.expression and one r'  */
		Formula skolemFormula;
		
		/**
		 * Constructs a new EQFReplacer.  This replacer should only be applied to
		 * the top-level formula, root.  The given bounds will be modified to include
		 * upper bounds for the skolem constants generated during replacement.
		 * @requires sharedNodes = {n: Node | #(n.~children & this.root'.^children) > 1 }
		 * @requires root.*children & Relation in allocator.bounds.relations
		 * @effects this.eqfs' = eqfs && this.bounds' = bounds
		 */
		EQFReplacer(Set<QuantifiedFormula> eqfs, Bounds bounds, Set<Node> sharedNodes) {
			this.eqfs = eqfs;
			this.allocator = new BooleanConstantAllocator.Overapproximating(bounds);
			this.skolemFormula = Formula.TRUE;
			this.cache = new IdentityHashMap<Node,Node>(sharedNodes.size());
			for(Node n: sharedNodes) {
				cache.put(n, null);
			}
			this.env = new Environment<LeafExpression>();
		}
		
		/**
		 * If the given node is shared and its replacement
		 * cached, the cached value is returned.  Otherwise, null is returned.
		 * @return this.cache[node]
		 */
		@SuppressWarnings("unchecked")
		@Override
		protected <N extends Node> N lookup(N node) {
			return (N) cache.get(node);
		}
		
		/**
		 * Caches the given replacement for the specified node, if the node
		 * is shared.  Otherwise does nothing.  The method returns
		 * the replacement node. 
		 * @effects this.cache' = this.cache ++ node->replacement 
		 * @return replacement
		 */
		@Override
		protected <N extends Node> N cache(N node, N replacement) {
			if (cache.containsKey(node)) {
				cache.put(node, replacement);
			}
			return replacement;
		}
		
		/** 
		 * Visits the decl's expression.  Note that we must not visit variables 
		 * in case they are re-used.  For example, consider the formula
		 * some x: X | all x: Y | F(x).  Since x bound by the existential quantifier
		 * is going to be skolemized, if we visited the variable in the enclosed
		 * declaration, we would get the skolem constant as a return value and
		 * a ClassCastException would be thrown.
		 * @return { d: Declaration |  d.variable = declaration.variable && 
		 *                             d.expression = declaration.expression.accept(this) 
		 */
		@Override
		public Decl visit(Decl decl) {
			Decl ret = lookup(decl);
			if (ret==null) {
				final Expression expression = decl.expression().accept(this);
				ret = (expression==decl.expression()) ?
					  decl : decl.variable().oneOf(expression); 
			}
			return cache(decl,ret);
		}
		
		/** 
		 * Returns the binding for the variable in this.env.
		 * @return this.env.lookup(variable)
		 * @throws IllegalArgumentException - no this.env.lookup(variable)
		 */
		@Override
		public Expression visit(Variable variable) { 
			final Expression ret = env.lookup(variable);
			if (ret==null)
				throw new IllegalArgumentException("unbound variable: " + variable);
			return ret;
		}
		
		/**
		 * Returns a mapping of each variable in decls to itself.
		 * @return decls.variable->decls.variable & iden
		 */
		private Map<Variable,LeafExpression> identityMapping(Decls decls) {
			final Map<Variable,LeafExpression> iden = new IdentityHashMap<Variable,LeafExpression>(decls.size());
			for(Decl decl: decls) {
				iden.put(decl.variable(), decl.variable());
			}
			return iden;
		}
		
		/** 
		 * Calls lookup(comprehension) and returns the cached value, if any.  
		 * If a replacement has not been cached, visits the expression's 
		 * children.  If nothing changes, the argument is cached and
		 * returned, otherwise a replacement expression is cached and returned.
		 * @return { c: Comprehension | c.declarations = comprehension.declarations.accept(this) &&
		 *                              c.formula = comprehension.formula.accept(this) }
		 */
		@Override
		public Expression visit(Comprehension comprehension) {
			Expression ret = lookup(comprehension);
			if (ret==null) {
				env = env.extend(identityMapping(comprehension.declarations()));
				final Decls decls = (Decls)comprehension.declarations().accept(this);
				final Formula formula = comprehension.formula().accept(this);
				ret = (decls==comprehension.declarations() && formula==comprehension.formula()) ? 
					  comprehension : formula.comprehension(decls);
				env = env.parent();
			}
			return cache(comprehension,ret);
		}
		
		/**
		 * Adds the formula 'skolem in decl.expression && one skolem' to 
		 * this.skolemFormula, computes and adds the upper bound for skolem
		 * to this.allocator.bounds.
		 * @effects this.skolemFormula' = this.skolemFormula && skolem in decl.expression
		 *            && one skolem
		 * @effects this.allocator.bounds.upperBound' = 
		 *            this.allocator.bounds.upperBound + skolem->Translator.evaluate(decl.expression, allocator)
		 */
		private void updateSkolemInfo(Relation skolem, Decl decl) {
			final BooleanMatrix skolemBound = Translator.evaluate(decl.expression(), allocator);
			final Universe universe = allocator.universe();
			final IntSet tuples = Ints.bestSet(universe.size());
			for(IndexedEntry<BooleanValue> cell : skolemBound) {
				tuples.add(cell.index());
			}
			allocator.bounds().bound(skolem, universe.factory().setOf(1, tuples));
			skolemFormula = skolemFormula.and(skolem.in(decl.expression()).and(skolem.one()));
		}
		
		/** 
		 * Calls lookup(quantFormula) and returns the cached value, if any.  
		 * If a replacement has not been cached, visits the formula's 
		 * children.  If nothing changes, the argument is cached and
		 * returned, otherwise a replacement formula is cached and returned.
		 * @return { q: QuantifiedFormula | q.declarations = quantFormula.declarations.accept(this) &&
		 *                                  q.formula = quantFormula.formula.accept(this) }
		 */
		@Override
		public Formula visit(QuantifiedFormula quantFormula) {
			Formula ret = lookup(quantFormula);
			if (ret==null) {			
				if (eqfs.contains(quantFormula)) { // an existentially quantified formula
					final Map<Variable,LeafExpression> varMap = new IdentityHashMap<Variable,LeafExpression>(quantFormula.declarations().size());
					for(Decl decl : quantFormula.declarations()) {
						Relation skolem = Relation.unary(decl.variable().name());
						updateSkolemInfo(skolem, (Decl) decl.accept(this));
						varMap.put(decl.variable(), skolem);				
					}
					env = env.extend(varMap);
					ret = quantFormula.formula().accept(this);
				} else {
					env = env.extend(identityMapping(quantFormula.declarations()));
					final Decls decls = quantFormula.declarations().accept(this);
					final Formula formula = quantFormula.formula().accept(this);
					ret = ((decls==quantFormula.declarations() && formula==quantFormula.formula()) ? 
						    quantFormula : formula.quantify(quantFormula.quantifier(), decls));
				}			
				env = env.parent();
			}
			return cache(quantFormula,ret);
		}
		
	}
	
	/**
	 * Detects top-level formulas in a given formula that are,
	 * semantically, existentially quantified.  Specifically, given a formula f,
	 * the visitor collects the members of the following set (assuming that all
	 * implications are interpreted as disjunctions):
	 * 
	 * { q: f.*children & QuantifiedFormula | 
	 *     all path: children | q in f.*path => 
	 *       all q': f.*path | 
	 *         (q'.quantifier = SOME => #{f.*path & NotFormula} % 2 = 0) &&
	 *         (q'.quantifier = ALL => #{f.*path & NotFormula} % 2 = 1) }
	 */
	private static final class EQFDetector extends DepthFirstVoidVisitor {
		
		/* @invariant stores numbers [0..15] which represent the powerset 
		 * of the set {FF, FT, TF, TT}. The numbers 1, 2, 4, 
		 * and 8 represent the elements FF, FT, TF, and TT, respectively.  
		 * The members of the power set are, therefore, represented 
		 * as the bitwise OR of the elements they contain.
		 */
		private final Byte[] flagCombos;
		
		/* @invariant maps all nodes that have a quantified formula in their AST
		 * and that are in the reflexive transitive closure of a shared node to a byte, depending on 
		 * which combination of flags was active when the node was visited.
		 * For example, if, during each visit to a node n, the flags negated
		 * and topLevel were set to either FT or TT, visitedNodes
		 * would map n to flagCombos[2 | 8] = flagCombos[10].  In the beginning,
		 * all shared nodes are mapped to flagCombos[0].  
		 */
		private final Map<Node, Byte> visitedNodes;
		
		private final Set<Node> sharedNodes;
		
		final Set<QuantifiedFormula> formulas;
		
		private boolean negated, topLevel;
		
		EQFDetector(Set<Node> sharedNodes) {
		    this.negated = false;
			this.topLevel = true;
			this.sharedNodes = sharedNodes;
			this.formulas = new IdentityHashSet<QuantifiedFormula>();
			this.flagCombos = new Byte[16];
			for(int i = 0; i < 16; i++) {
				flagCombos[i] = new Byte((byte)i);
			}
			// determine which shared nodes and their descendents
			// have quantified formulas in their subtrees
			final QFDescendentDetector qfd = new QFDescendentDetector();
			for(Node n: sharedNodes) {
				n.accept(qfd);
			}
			// prune away the nodes that have no quantified formulas as descendents
			this.visitedNodes = new IdentityHashMap<Node,Byte>(qfd.numNodesWithQFDescendent);
			for(Map.Entry<Node,Boolean> e : qfd.visitedNodes.entrySet()) {
				if (e.getValue())
					visitedNodes.put(e.getKey(), flagCombos[0]);
			}
		}
		
		/**
		 * Translates the current value of the negated/topLevel flags
		 * into an integral representation that glues the two bits
		 * together, negated bit first.
		 */
		private final int flagCombo() {
			return 1 << ((negated ? 2 : 0) | (topLevel ? 1 : 0));
		}
		
		/**
		 * Returns true if the given node has already been visited with 
		 * the present combination of the flags.  If not, the present
		 * combination is recorded and false is returned.  
		 */
		private final boolean visited(Node node) {
			Byte status = visitedNodes.get(node);
			if (status != null) {
				final int current = flagCombo();
				if ((status & current) == 0) { // not seen yet
					// if the current value of the topLevel flag is false,
					// we don't have to visit this node ever again, so we
					// simply mark it as having been visited with all possible
					// flag combinations
					if (!topLevel)
						visitedNodes.put(node, flagCombos[15]);
					else
						visitedNodes.put(node, flagCombos[status|current]);
				} else { // combination seen
					return true;
				}
			} else if (sharedNodes.contains(node)) {
				// this is a shared node that has no quantified formulas as descendents;
				// we can therefore mark it as having been visited with all possible flag combinations
				visitedNodes.put(node, flagCombos[15]);
			}
			return false;
		}
		
		
		public void visit(QuantifiedFormula quantFormula) {
			if (!visited(quantFormula)) { 
				final boolean oldTop = topLevel;
				final QuantifiedFormula.Quantifier q = quantFormula.quantifier();
				if (topLevel && (q==SOME && !negated || q==ALL && negated)) {
					final Byte status = visitedNodes.get(quantFormula);
					if (status==null || status==flagCombo()) {
						formulas.add(quantFormula);
					}
				} else {
					formulas.remove(quantFormula);
					topLevel = false;
				}
				quantFormula.formula().accept(this);
				topLevel = oldTop;
			}
		}
		
		/**
		 * We have to visit the children of all binary formulas, regardless
		 * of whether they are at the top level or not, in order to properly
		 * handle potential sharing of quantified formula.  Specifically,
		 * an existentially quantified formula is skolemizable iff it is 
		 * at the top-level with respect to *all* of its parents.  Visiting
		 * the non-top level children ensures that we detect the case when 
		 * a formula is not at the top level with respect to some parent.
		 */
		public void visit(BinaryFormula binFormula) {
			if (!visited(binFormula)) {
				final boolean oldTop = topLevel;
				final BinaryFormula.Operator op = binFormula.op();
				if (op==IFF || negated && op==AND || !negated && (op==OR || op==IMPLIES)) {
					topLevel = false;
				}
				if (op==IMPLIES) { // a => b = !a || b one negation on the left side
					negated = !negated;
					binFormula.left().accept(this);
					negated = !negated;
					binFormula.right().accept(this);
				} else if (op==IFF) { // a<=>b = (!a || b) && (!b || a) both sides negated and not negated
					negated = !negated;
					super.visit(binFormula);
					negated = !negated;
					super.visit(binFormula);
				} else { // op==AND || op==OR
					super.visit(binFormula);
				}
				topLevel = oldTop;
			}
		}
		
		public void visit(NotFormula not) {
			if (!visited(not)) {
				negated = !negated;
				not.formula().accept(this);
				negated = !negated;
			}
		}
		
		public void visit(ComparisonFormula compFormula) {
			if (!visited(compFormula)) {
				final boolean oldTop = topLevel;
				topLevel = false;
				super.visit(compFormula);
				topLevel = oldTop;
			}
		}
		
		public void visit(MultiplicityFormula multFormula) {
			if (!visited(multFormula)) {
				final boolean oldTop = topLevel;
				topLevel = false;
				super.visit(multFormula);
				topLevel = oldTop;
			}
		}
		
		public void visit(RelationPredicate pred) {
			if (!visited(pred)) {
				final boolean oldTop = topLevel;
				topLevel = false;
				super.visit(pred);
				topLevel = oldTop;
			}
		}
		
	}
	
	
	/**
	 * Starting at a given root, a QFDescendentDetector determines which nodes in its
	 * reflexive transitive closure have quantified formulas in their subtrees.
	 */
	private static final class QFDescendentDetector implements ReturnVisitor<Boolean, Boolean, Boolean> {
		/**
		 * Maps each visited node to TRUE, if it has a quantified formula
		 * as a descendent, and to FALSE otherwise.
		 */
		final Map<Node, Boolean> visitedNodes;
		int numNodesWithQFDescendent = 0;
		/**
		 * Constructs a new QFDescendentDetector.
		 */
		QFDescendentDetector() {
			this.visitedNodes = new IdentityHashMap<Node,Boolean>();
		}
		
		/**
		 * Returns null if the node has not been visited before.
		 * If it has, returns a Boolean value that represents
		 * the presence or absence of a quantified formula in
		 * the node's subtree.
		 */
		private Boolean visited(Node node) {
			return visitedNodes.get(node);
		}
		
		/**
		 * Records the presence or absence of a quantified formula
		 * in the node's subtree as given by hasQFDescendent and returns it.
		 */
		private Boolean recordVisit(Node node, boolean hasQFDescendent) {
			final Boolean record = Boolean.valueOf(hasQFDescendent);
			visitedNodes.put(node, record);
			if (hasQFDescendent) numNodesWithQFDescendent++;
			return record;
		}
		
		public Boolean visit(Decls decls) {
			Boolean hasQFDescendent = visited(decls);
			if (hasQFDescendent!=null) 
				return hasQFDescendent;
			boolean qfd = false;
			for(Decl d: decls) {
				qfd = qfd | visit(d);
			}
			return recordVisit(decls, qfd);
		}

		public Boolean visit(Decl decl) {
			Boolean hasQFDescendent = visited(decl);
			if (hasQFDescendent!=null)
				return hasQFDescendent;
			return recordVisit(decl, decl.expression().accept(this));
		}

		public Boolean visit(Relation relation) {
			return Boolean.FALSE;
		}

		public Boolean visit(Variable variable) {
			return Boolean.FALSE;
		}

		public Boolean visit(ConstantExpression constExpr) {
			return Boolean.FALSE;
		}

		public Boolean visit(BinaryExpression binExpr) {
			Boolean hasQFDescendent = visited(binExpr);
			if (hasQFDescendent!=null)
				return hasQFDescendent;
			return recordVisit(binExpr, binExpr.left().accept(this) | binExpr.right().accept(this));
		}

		public Boolean visit(UnaryExpression unaryExpr) {
			Boolean hasQFDescendent = visited(unaryExpr);
			if (hasQFDescendent!=null)
				return hasQFDescendent;
			return recordVisit(unaryExpr, unaryExpr.expression().accept(this));
		}

		public Boolean visit(Comprehension comprehension) {
			Boolean hasQFDescendent = visited(comprehension);
			if (hasQFDescendent!=null)
				return hasQFDescendent;
			return recordVisit(comprehension, comprehension.declarations().accept(this) | comprehension.formula().accept(this));
		}

		public Boolean visit(IfExpression ifExpr) {
			Boolean hasQFDescendent = visited(ifExpr);
			if (hasQFDescendent!=null)
				return hasQFDescendent;
			return recordVisit(ifExpr, ifExpr.condition().accept(this) | 
					                   ifExpr.thenExpr().accept(this) | ifExpr.elseExpr().accept(this));
		}

		public Boolean visit(QuantifiedFormula quantFormula) {
			Boolean hasQFDescendent = visited(quantFormula);
			if (hasQFDescendent!=null)
				return hasQFDescendent;
			quantFormula.declarations().accept(this);
			quantFormula.formula().accept(this);
			return recordVisit(quantFormula, true);
		}

		public Boolean visit(BinaryFormula binFormula) {
			Boolean hasQFDescendent = visited(binFormula);
			if (hasQFDescendent!=null)
				return hasQFDescendent;
			return recordVisit(binFormula, binFormula.left().accept(this) | binFormula.right().accept(this));
		}

		public Boolean visit(NotFormula not) {
			Boolean hasQFDescendent = visited(not);
			if (hasQFDescendent!=null)
				return hasQFDescendent;
			return recordVisit(not, not.formula().accept(this));
		}

		public Boolean visit(ConstantFormula constant) {
			return Boolean.FALSE;
		}

		public Boolean visit(ComparisonFormula compFormula) {
			Boolean hasQFDescendent = visited(compFormula);
			if (hasQFDescendent!=null)
				return hasQFDescendent;
			return recordVisit(compFormula, compFormula.left().accept(this) | compFormula.right().accept(this));
		}

		public Boolean visit(MultiplicityFormula multFormula) {
			Boolean hasQFDescendent = visited(multFormula);
			if (hasQFDescendent!=null)
				return hasQFDescendent;
			return recordVisit(multFormula, multFormula.expression().accept(this));
		}

		public Boolean visit(RelationPredicate predicate) {
			Boolean hasQFDescendent = visited(predicate);
			if (hasQFDescendent!=null)
				return hasQFDescendent;
			if (predicate.name()==RelationPredicate.Name.FUNCTION) {
				final RelationPredicate.Function fun = (RelationPredicate.Function) predicate;
				return recordVisit(fun, fun.domain().accept(this) | fun.range().accept(this));
			}
			return recordVisit(predicate, false);
		}
		
	}
	
	
}
