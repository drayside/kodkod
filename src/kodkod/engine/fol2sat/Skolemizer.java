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

import kodkod.ast.BinaryFormula;
import kodkod.ast.ComparisonFormula;
import kodkod.ast.Comprehension;
import kodkod.ast.Decl;
import kodkod.ast.Decls;
import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.LeafExpression;
import kodkod.ast.MultiplicityFormula;
import kodkod.ast.Node;
import kodkod.ast.NotFormula;
import kodkod.ast.QuantifiedFormula;
import kodkod.ast.Relation;
import kodkod.ast.RelationPredicate;
import kodkod.ast.Variable;
import kodkod.ast.visitor.DepthFirstReplacer;
import kodkod.ast.visitor.DepthFirstVoidVisitor;
import kodkod.engine.bool.BooleanMatrix;
import kodkod.engine.bool.BooleanValue;
import kodkod.instance.Bounds;
import kodkod.instance.TupleFactory;
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
		final Map<Decl, Relation> skolemizableDecls = skolemizableDecls(detector.formulas);
		if (skolemizableDecls.isEmpty()) {
			ret = formula;
		} else {
			final EQFReplacer replacer = new EQFReplacer(sharedNodes, detector.formulas, skolemizableDecls);
			ret = formula.accept(replacer);
			final int usize = bounds.universe().size();
			final TupleFactory factory = bounds.universe().factory();
			for(Map.Entry<Decl, Relation> entry : skolemizableDecls.entrySet()) {
				Relation skolem = entry.getValue();
				Expression expression = entry.getKey().expression();
				BooleanMatrix skolemBound = Translator.evaluate(expression, new BooleanConstantAllocator.Overapproximating(bounds));
				IntSet tuples = Ints.bestSet(usize);
				for(IndexedEntry<BooleanValue> cell : skolemBound) {
					tuples.add(cell.index());
				}
				bounds.bound(skolem, factory.setOf(1, tuples));
				ret = ret.and(skolem.in(expression).and(skolem.one()));
			}
//			ret = formula;
//			System.out.println(formula);
//			System.out.println(ret);
//			System.out.println(bounds);
		}
		return ret;
	}
	
	/**
	 * Returns a map from all skolemizable declarations in formula.declarations to freshly
	 * allocated skolem constants (relations).  A Decl is skolemizable if it has no free variables.
	 * @return {m: Decl -> lone Relation | #m.Relation = #Decl.m &&  
	 *           all d: formulas.^children & Decl | 
	 *            some m[d] => no d.expression.^children & Variable }         
	 */
	private static Map<Decl, Relation> skolemizableDecls(Set<QuantifiedFormula> formulas) {
		final Map<Decl,Relation> allDecls = new IdentityHashMap<Decl,Relation>(formulas.size());
		for(QuantifiedFormula formula: formulas) {
			for (Decl decl: formula.declarations()) {
				allDecls.put(decl,null);
			}
		}
		FreeVariableDetector.retainBoundNodes(allDecls.keySet());
		for(Map.Entry<Decl,Relation> entry : allDecls.entrySet()) {
			entry.setValue(Relation.unary(entry.getKey().variable().name()));
		}
		return allDecls;
	}
	
	/**
	 * Given a set of existentially quantified formulas, s, and a mapping from
	 * declarations to skolem constants, m, an EQFReplacer replaces the given formulas with
	 * their skolemizations.  In particular, each given formula f is replaced with a formula f'
	 * such that f' contains no declaration d that is mapped by m, and all occurences
	 * of d.variable in f are replaced by m[d] in f'. 
	 * 
	 * @specfield root: Formula // an EQFReplaces should only be applied to top once
	 * @specfield eqfs: set QuantifiedFormula 
	 * @specfield skolems: Decl -> lone Relation
	 * @invariant eqfs = { q: root.*children & QuantifiedFormula | 
	 *                      all path: children | q in root.*path => 
	 *                       all q': root.*path | 
	 *                         (q'.quantifier = SOME => #{root.*path & NotFormula} % 2 = 0) &&
	 *                         (q'.quantifier = ALL => #{root.*path & NotFormula} % 2 = 1) }
	 * @invariant (all d: eqfs.^children & Decl | 
	 *             some skolems[d] <=> no d.expression.^children & Variable) &&
	 *            #skolems.Relation = #Decl.skolems && 
	 *            Decl.skolems in Relation - root.^children   
	 */
	private static final class EQFReplacer extends DepthFirstReplacer {
		final Set<QuantifiedFormula> eqfs;
		final Map<Decl, Relation> skolems;
		final Map<Node,Node> cache;
		Environment<LeafExpression> env;
		
		/**
		 * Constructs a new EQFReplacer.
		 * @effects this.eqfs' = eqfs && this.skolems' = skolems && 
		 *          this.sharedNodes' = {n: Node | #(n.~children & this.root'.*children) > 1 }
		 */
		EQFReplacer(Set<Node> sharedNodes, Set<QuantifiedFormula> eqfs, Map<Decl, Relation> skolems) {
			this.eqfs = eqfs;
			this.skolems = skolems;
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
				Decls decls = null;
				
				if (eqfs.contains(quantFormula)) { // an existentially quantified formula
					final Map<Variable,LeafExpression> varMap = new IdentityHashMap<Variable,LeafExpression>(quantFormula.declarations().size());
					for(Decl decl : quantFormula.declarations()) {
						Relation skolem = skolems.get(decl);
						if (skolem == null) { // not skolemizable 
							decls = (decls==null ? decl : decls.and(decl));
							varMap.put(decl.variable(), decl.variable());
						} else { // skolemizable
							varMap.put(decl.variable(), skolem);
						}
					}
					env = env.extend(varMap);
					decls = decls==null ? null : decls.accept(this);
				} else {
					env = env.extend(identityMapping(quantFormula.declarations()));
					decls = quantFormula.declarations().accept(this);
				}
				
				final Formula formula = quantFormula.formula().accept(this);
				ret = decls==null ? formula : 
					  ((decls==quantFormula.declarations() && formula==quantFormula.formula()) ? 
					    quantFormula : formula.quantify(quantFormula.quantifier(), decls));
				
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
		final Byte[] flagCombos;
		
		/* @invariant maps each shared node to a byte, depending on 
		 * which combination of flags was active when the node was visited.
		 * For example, if, during each visit to a node n, the flags negated
		 * and topLevel were set to either FT or TT, visitedNodes
		 * would map n to flagCombos[2 | 8] = flagCombos[10].  In the beginning,
		 * all shared nodes are mapped to flagCombos[0].  
		 */
		final Map<Node, Byte> visitedNodes;
		
		final Set<QuantifiedFormula> formulas;
		
		boolean negated, topLevel;
		
		EQFDetector(Set<Node> sharedNodes) {
		    this.negated = false;
			this.topLevel = true;
			this.formulas = new IdentityHashSet<QuantifiedFormula>();
			this.flagCombos = new Byte[16];
			for(int i = 0; i < 16; i++) {
				flagCombos[i] = new Byte((byte)i);
			}
			this.visitedNodes = new IdentityHashMap<Node,Byte>(sharedNodes.size());
			for(Node n : sharedNodes) {
				visitedNodes.put(n, flagCombos[0]);
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
					visitedNodes.put(node, flagCombos[status|current]);
				} else { // combination seen
					return true;
				}
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
}
