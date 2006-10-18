package kodkod.engine.fol2sat;

import java.util.ArrayList;
import java.util.Collections;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import kodkod.ast.BinaryExpression;
import kodkod.ast.Comprehension;
import kodkod.ast.Decl;
import kodkod.ast.Decls;
import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.Multiplicity;
import kodkod.ast.Node;
import kodkod.ast.QuantifiedFormula;
import kodkod.ast.Relation;
import kodkod.ast.Variable;
import kodkod.ast.visitor.DepthFirstReplacer;
import kodkod.engine.Options;
import kodkod.engine.bool.BooleanFactory;
import kodkod.engine.bool.BooleanMatrix;
import kodkod.engine.bool.BooleanValue;
import kodkod.instance.Bounds;
import kodkod.instance.Universe;
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
	 * Skolemizes the given annotated formula using the given bounds and options.
	 * @effects upper bound mappings for skolem constants, if any, are added to the bounds
	 * @return a Skolemizer whose skolemized field is a skolemized version of the given formula,
	 * and whose skolem field contains the generated skolem constants
	 * @throws NullPointerException - any of the arguments are null
	 * @throws IllegalArgumentException - some Relation & annotated.node.^children - bounds.relations
	 * @throws UnsupportedOperationException - bounds is unmodifiable
	 */
	@SuppressWarnings("unchecked")
	static Skolemizer skolemize(AnnotatedNode<Formula> annotated, Bounds bounds, Options options) {
		final Set<QuantifiedFormula> formulas = AnnotatedNode.skolemizables(annotated, options.skolemDepth());
		
		if (formulas.isEmpty()) {
			return new Skolemizer(annotated, Collections.EMPTY_MAP);
		} else {
			final EQFReplacer replacer = new EQFReplacer(formulas, annotated.sharedNodes(), bounds, options);
			final Formula f = annotated.node().accept(replacer).and(replacer.skolemFormula);
//			System.out.println(f);
			return new Skolemizer(new AnnotatedNode<Formula>(f), replacer.skolems);
		}
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
		/* replacement environment; maps skolemized variables to their skolem expressions,
		 * and non-skolemized variables to themselves */
		private Environment<Expression> env;
		private final int skolemDepth;
		/* a list of declInfo records for non-skolemizable decls, in the order of
		 * occurrence; i.e. the first info corresponds to the outermost decl, the
		 * second to the second outermost decl, etc.
		 */
		private final List<DeclInfo> nonSkolems;
		/* when computing the upper bounds for skolems, all
		 * expressions must be replaced with sound approximation.
		 */
		private final Approximator approximator ;
		/* the interpreter used to determine the upper bounds for skolem constants;
		 * the upper bounds for skolem constants will be added to interpreter.bounds */
		private final BoundsInterpreter.Overapproximating interpreter;
		/* the cache used for storing the replacements for shared nodes */
		private final Map<Node,Node> cache;
		
		/* the formula that constrains all the skolem constants */
		Formula skolemFormula;
		/* the mapping from skolemized declarations to their corresponding
		 * skolem constants */
		final Map<Decl, Relation> skolems;
		
		/**
		 * Constructs a new EQFReplacer.  This replacer should only be applied to
		 * the top-level formula, root.  The bounds will be modified to include
		 * upper bounds for the skolem constants generated during replacement.
		 * @requires sharedNodes = {n: Node | #(n.~children & this.root'.^children) > 1 }
		 * @requires root.*children & Relation in interpreter.bounds.relations
		 * @effects this.eqfs' = eqfs && this.bounds' = interpreter.bounds
		 */
		EQFReplacer(Set<QuantifiedFormula> eqfs, Set<Node> sharedNodes, Bounds bounds, Options options) {
			this.eqfs = eqfs;
			this.interpreter = new BoundsInterpreter.Overapproximating(bounds, BooleanFactory.constantFactory(options));
			this.skolems = new IdentityHashMap<Decl, Relation>(eqfs.size());
			this.skolemFormula = Formula.TRUE;
			this.cache = new IdentityHashMap<Node,Node>(sharedNodes.size());
			for(Node n: sharedNodes) {
				cache.put(n, null);
			}
			this.env = new Environment<Expression>();
			this.skolemDepth = options.skolemDepth();
			this.nonSkolems = new ArrayList<DeclInfo>(skolemDepth);
			this.approximator = new Approximator(sharedNodes, nonSkolems);
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
		 * Extends this.env with the mapping of declared
		 * variables to themselves, in the order in which they
		 * are declared.
		 * @effects extends this.env with the mapping of declared
		 * variables to themselves, in the order in which they
		 * are declared.
		 */
		private void extendEnv(Decls decls) {
			for(Decl decl: decls) {
				env = env.extend(decl.variable(), decl.variable());
			}
		}
		
		/**
		 * Sets this.env to the ancestor reached in num steps.
		 * @requires #(this.env.^parent-null) >= num
		 * @effects sets this.env to the ancestor reached in num steps.
		 */
		private void shrinkEnv(int num) {
			for(int i = 0; i < num; i++)
				env = env.parent();
		}
		
		/**
		 * Extends this.nonSkolems with DeclInfos for the given Decls.
		 * @effects extends this.nonSkolems with DeclInfos for the given Decls.
		 */
		private void extendNonSkolems(Decls decls) {
			for(Decl decl: decls) {
				nonSkolems.add(new DeclInfo(decl, decl.expression().accept(approximator)));
			}
			assert nonSkolems.size() <= skolemDepth;
		}
		
		/**
		 * Removes the last num elements from the tail of this.nonSkolems.
		 * @requires num <= this.nonSkolems.size()
		 * @effects removes the last num elements from the tail of this.nonSkolems.
		 */
		private void shrinkNonSkolems(int num) {
			assert num <= nonSkolems.size();
			for(int depth = nonSkolems.size(), i = depth-1; i >= depth-num; i--) {
				nonSkolems.remove(i);
			}
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
				extendEnv(comprehension.declarations());
				final Decls decls = (Decls)comprehension.declarations().accept(this);
				final Formula formula = comprehension.formula().accept(this);
				ret = (decls==comprehension.declarations() && formula==comprehension.formula()) ? 
					  comprehension : formula.comprehension(decls);
				shrinkEnv(decls.size());
			}
			return cache(comprehension,ret);
		}
		
		/**
		 * Adds a skolem constant for the given declaration to this.skolems,
		 * and returns the expression that should replace decl.variable in 
		 * the final formula.
		 * @effects adds a skolem constant for the given declaration to this.skolems
		 * @return the expression that should replace decl.variable in 
		 * the final formula
		 */
		private Expression extendSkolems(Decl decl) {
			final int depth = nonSkolems.size();
			final int arity = depth + decl.variable().arity();
			final Relation skolem = Relation.nary("$"+decl.variable().name(), arity);
			Expression exprBound = decl.expression().accept(approximator);
			for(int i = depth-1; i >= 0; i--) {
				exprBound = nonSkolems.get(i).upperBound.product(exprBound);
			}
			final BooleanMatrix tupleBound = (BooleanMatrix) 
			  FOL2BoolTranslator.translate(new AnnotatedNode<Expression>(exprBound), interpreter);
			final Universe universe = interpreter.universe();
			final IntSet tuples = Ints.bestSet((int)StrictMath.pow(universe.size(), arity));
			for(IndexedEntry<BooleanValue> cell : tupleBound) {
				tuples.add(cell.index());
			}
			interpreter.boundingObject().bound(skolem, universe.factory().setOf(arity, tuples));
			Expression skolemExpr = skolem;
			for(DeclInfo d: nonSkolems) {
				skolemExpr = d.decl.variable().join(skolemExpr);
			}
			Formula f = skolemExpr.in(decl.expression());
			if (decl.multiplicity()!=Multiplicity.SET)
				f = f.and(skolemExpr.apply(decl.multiplicity()));
			if (depth>0) {
				Decls decls = nonSkolems.get(0).decl;
				for(int i = 1; i < depth; i++) {
					decls = decls.and(nonSkolems.get(i).decl);
				}
				skolemFormula = skolemFormula.and(f.forAll(decls));
			} else {
				skolemFormula = skolemFormula.and(f);
			}
			return skolemExpr;
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
				final Decls decls = quantFormula.declarations();
				if (eqfs.contains(quantFormula)) { // skolemizable formula
					for(Decl decl : decls) {
						env = env.extend(decl.variable(), extendSkolems((Decl)decl.accept(this)));				
					}
					ret = quantFormula.formula().accept(this);
				} else { // non-skolemizable formula
					extendEnv(decls);
					final Decls newDecls = decls.accept(this);
					
					final boolean updateNonSkolems = nonSkolems.size() < skolemDepth;
//					System.out.println(updateNonSkolems + " " + nonSkolems);
					if (updateNonSkolems) extendNonSkolems(newDecls);
					final Formula formula = quantFormula.formula().accept(this);
//					System.out.println(nonSkolems + " " + decls.size());
					if (updateNonSkolems) shrinkNonSkolems(decls.size());
					
					ret = ((newDecls==decls && formula==quantFormula.formula()) ? 
						    quantFormula : formula.quantify(quantFormula.quantifier(), newDecls));
				}			
				shrinkEnv(decls.size());
			}
			return cache(quantFormula,ret);
		}
	}
	
	/**
	 * Replaces all subexpressions in a node with sound approximations; 
	 * in particular, a difference expression is replaced with
	 * its left child, and a free variable with the approximation
	 * of its declared expression.  
	 */
	private static final class Approximator extends DepthFirstReplacer {
		/* the cache used for storing the replacements for shared nodes */
		private final Map<Node,Node> cache;
		private final Set<Node> cached;
		/* the upper bounds (approximations) for free variables */
		private final List<DeclInfo> infos;
		/**
		 * Creates a difference remover which will cache replacement values
		 * for the given nodes, and it will replace free variables with 
		 * the upperBound expressions from the infos list.
		 */
		Approximator(Set<Node> sharedNodes, List<DeclInfo> infos) {
			this.cache = new IdentityHashMap<Node,Node>();
			this.cached = sharedNodes;
			this.infos = infos;
		}
		
		@SuppressWarnings("unchecked")
		@Override
		protected <N extends Node> N lookup(N node) {
			return (N)cache.get(node);
		}

		@Override
		protected <N extends Node> N cache(N node, N replacement) {
			if (cached.contains(node)) {
				cache.put(node, replacement);
			}
			return replacement;
		}
		
		/**
		 * Returns super.visit(binExpr) if binExpr is not a difference
		 * expression, otherwise return binExpr.left().accept(this). 
		 * @return super.visit(binExpr) if binExpr is not a difference
		 * expression, otherwise return binExpr.left().accept(this). 
		 */
		public Expression visit(BinaryExpression binExpr) {
			if (binExpr.op()==BinaryExpression.Operator.DIFFERENCE)
				return cache(binExpr, binExpr.left().accept(this));
			else
				return super.visit(binExpr);
		}	
		
		/**
		 * Returns the upper bound (approximation) of the expression 
		 * to which the given variable was most recently bound in 
		 * this.infos list.  If no such Expression exists, the variable
		 * must have been unbound, so an UnboundLeafException is thrown.
		 * @return the upper bound (approximation) of the expression 
		 * to which the given variable was most recently bound in 
		 * this.infos list
		 * @throws UnboundLeafException - this.infos contains no DeclInfo
		 * for the given variable
		 */
		public Expression visit(Variable variable) {
			for(int i = infos.size()-1; i >= 0; i--) {
				DeclInfo d = infos.get(i);
				if (d.decl.variable()==variable)
					return d.upperBound;
			}
			throw new UnboundLeafException("Unbound variable",variable);
		}
	}
	
	/**
	 * Stores info about non-skolemizable decls.
	 * @specfield decl: Decl
	 * @specfield upperBound: TupleSet
	 * @invariant decl.expression in upperBound
	 */
	private static final class DeclInfo {
		final Decl decl;
		final Expression upperBound;
		/**
		 * Constructs a new NonSkolemInfo for the given 
		 */
		DeclInfo(Decl decl, Expression upperBound) {
			this.decl = decl;
			this.upperBound = upperBound;
		}
		
		public String toString() {
			return "(" +decl + " < " + upperBound + ")";
		}
	}
}
