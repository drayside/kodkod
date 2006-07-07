package kodkod.engine.fol2sat;

import java.util.Collections;
import java.util.IdentityHashMap;
import java.util.Map;
import java.util.Set;

import kodkod.ast.Comprehension;
import kodkod.ast.Decl;
import kodkod.ast.Decls;
import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.LeafExpression;
import kodkod.ast.Multiplicity;
import kodkod.ast.Node;
import kodkod.ast.QuantifiedFormula;
import kodkod.ast.Relation;
import kodkod.ast.Variable;
import kodkod.ast.visitor.DepthFirstReplacer;
import kodkod.engine.bool.BooleanFactory;
import kodkod.engine.bool.BooleanMatrix;
import kodkod.engine.bool.BooleanValue;
import kodkod.instance.Bounds;
import kodkod.instance.Universe;
import kodkod.util.collections.IdentityHashSet;
import kodkod.util.ints.IndexedEntry;
import kodkod.util.ints.IntSet;
import kodkod.util.ints.Ints;

/**
 * Skolemizes away existential quantifiers.  Specifically,
 * each top-level existentially quantified formula whose
 * declared variables do not depend on any other variables
 * is transformed as follows:  some x: E | F(x) becomes
 * r_x in E && some r_x && F(r_x), where r_x is a freshly
 * allocated relation (i.e. skolem constant). 
 * 
 * @specfield original: AnnotatedNode<Formula>
 * @specfield skolemized: AnnotatedNode<Formula>
 * @specfield skolems: original.^children & Decl -> lone Relation
 * 
 * @author Emina Torlak
 */
final class Skolemizer {
	private final AnnotatedNode<Formula> skolemized;
	private final Map<Decl, Relation> skolems;
	
	/**
	 * Constructs a new skolemizer with the given values.
	 * @effects this.skolemized' = annotated && this.skolems' = skolems
	 */
	private Skolemizer(AnnotatedNode<Formula> annotated, Map<Decl, Relation> skolems) {
		this.skolemized = annotated;
		this.skolems = skolems;
	}
	
	/**
	 * Returns the skolemized version of this.original.
	 * @return this.skolemized
	 */
	AnnotatedNode<Formula> skolemized() {
		return skolemized;
	}
	
	/**
	 * Returns a map from the existentially quantified declarations
	 * in this.original to their corresponding skolem constants  in this.skolemized.
	 * @return this.skolems
	 */
	Map<Decl, Relation> skolems() {
		return skolems;
	}
	
	/**
	 * Skolemizes the given annotated formula using the given bounds and factory.
	 * @effects upper bound mappings for skolem constants, if any, are added to the bounds
	 * @return a Skolemizer whose skolemized field is a skolemized version of the given formula,
	 * and whose skolem field contains the generated skolem constants
	 * @throws NullPointerException - any of the arguments are null
	 * @throws IllegalArgumentException - some Relation & annotated.node.^children - bounds.relations
	 * @throws UnsupportedOperationException - bounds is unmodifiable
	 */
	@SuppressWarnings("unchecked")
	static Skolemizer skolemize(AnnotatedNode<Formula> annotated, Bounds bounds, BooleanFactory factory) {
		final Set<QuantifiedFormula> formulas = AnnotatedNode.existentials(annotated);
		if (formulas.isEmpty()) {
			return new Skolemizer(annotated, Collections.EMPTY_MAP);
		} else {
			final EQFReplacer replacer = new EQFReplacer(formulas, annotated.sharedNodes(), new BoundsInterpreter.Overapproximating(bounds, factory));
			final Formula f = annotated.node().accept(replacer).and(replacer.skolemFormula);
			if (identityMapping(replacer.cache)) {
				return new Skolemizer(new AnnotatedNode<Formula>(f, annotated.sharedNodes()), replacer.skolems);
			} else {
				final Set<Node> newSharedNodes = new IdentityHashSet<Node>(annotated.sharedNodes().size());
				for (Map.Entry<Node, Node> e : replacer.cache.entrySet()) {
					newSharedNodes.add(e.getValue());
				}
				return new Skolemizer(new AnnotatedNode<Formula>(f, newSharedNodes), replacer.skolems);
			}
		}
	}
	
	/**
	 * Returns true if the given map is an identity mapping.
	 * @return all o: Object | map.containsKey(o) => map.get(o) = o
	 */
	private static boolean identityMapping(Map<?, ?> map) {
		for (Map.Entry<?,?> e : map.entrySet()) {
			if (e.getKey()!=e.getValue())
				return false;
		}
		return true;
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
		private Environment<LeafExpression> env;
		/* the interpreter used to determine the upper bounds for skolem constants;
		 * the upper bounds for skolem constants will be added to interpreter.bounds */
		private final BoundsInterpreter.Overapproximating manager;
		/* the cache used for storing the replacements for shared nodes */
		final Map<Node,Node> cache;
		/* the conjunction that constrains all the skolem constants; i.e.
		 * for each decl->r in this.skolems, the conjunction contains the formula
		 * 'r in decl.expression and one r'  */
		Formula skolemFormula;
		/* the mapping from skolemized declarations to their corresponding
		 * skolem constants */
		final Map<Decl, Relation> skolems;
		final Bounds bounds;
		/**
		 * Constructs a new EQFReplacer.  This replacer should only be applied to
		 * the top-level formula, root.  The bounds backing the given interpreter will be modified to include
		 * upper bounds for the skolem constants generated during replacement.
		 * @requires sharedNodes = {n: Node | #(n.~children & this.root'.^children) > 1 }
		 * @requires root.*children & Relation in interpreter.bounds.relations
		 * @effects this.eqfs' = eqfs && this.bounds' = interpreter.bounds
		 */
		EQFReplacer(Set<QuantifiedFormula> eqfs, Set<Node> sharedNodes, BoundsInterpreter.Overapproximating manager) {
			this.eqfs = eqfs;
			this.bounds = manager.boundingObject();
			this.manager = manager;
			this.skolems = new IdentityHashMap<Decl, Relation>(eqfs.size());
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
		 * @return { d: Declaration |  d.variable = decl.variable && d.multiplicity = decl.multiplicity &&
		 *                             d.expression = decl.expression.accept(this) } 
		 */
		@Override
		public Decl visit(Decl decl) {
			Decl ret = lookup(decl);
			if (ret==null) {
				final Expression expression = decl.expression().accept(this);
				ret = (expression==decl.expression()) ?
					  decl : decl.variable().declare(decl.multiplicity(), expression); 
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
		 * Adds the formula 'skolem in decl.expression && decl.multiplicity skolem' to 
		 * this.skolemFormula, computes and adds the upper bound for skolem
		 * to this.allocator.bounds.
		 * @effects this.skolemFormula' = this.skolemFormula && skolem in decl.expression
		 *            && decl.multiplicity skolem
		 * @effects this.allocator.bounds.upperBound' = 
		 *            this.allocator.bounds.upperBound + skolem->Translator.evaluate(decl.expression, interpreter)
		 */
		private void updateSkolemInfo(Relation skolem, Decl decl) {
			final BooleanMatrix skolemBound = (BooleanMatrix) 
			  FOL2BoolTranslator.translate(new AnnotatedNode<Expression>(decl.expression()), manager);
			final Universe universe = bounds.universe();
			final int arity = decl.variable().arity();
			final IntSet tuples = Ints.bestSet((int)StrictMath.pow(universe.size(), arity));
			for(IndexedEntry<BooleanValue> cell : skolemBound) {
				tuples.add(cell.index());
			}
			bounds.bound(skolem, universe.factory().setOf(arity, tuples));
			skolemFormula = skolemFormula.and(skolem.in(decl.expression()));
			if (decl.multiplicity()!=Multiplicity.SET) 
				skolemFormula = skolemFormula.and(skolem.apply(decl.multiplicity()));
			skolems.put(decl, skolem);
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
						Relation skolem = Relation.nary(decl.variable().name(), decl.variable().arity());
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
}
