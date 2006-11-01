/**
 * 
 */
package kodkod.engine.fol2sat;

import static kodkod.ast.BinaryFormula.Operator.AND;
import static kodkod.ast.BinaryFormula.Operator.IFF;
import static kodkod.ast.BinaryFormula.Operator.IMPLIES;
import static kodkod.ast.BinaryFormula.Operator.OR;
import static kodkod.ast.QuantifiedFormula.Quantifier.ALL;
import static kodkod.ast.QuantifiedFormula.Quantifier.SOME;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import kodkod.ast.BinaryExpression;
import kodkod.ast.BinaryFormula;
import kodkod.ast.ComparisonFormula;
import kodkod.ast.Comprehension;
import kodkod.ast.Decl;
import kodkod.ast.Decls;
import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.IntComparisonFormula;
import kodkod.ast.IntExpression;
import kodkod.ast.Multiplicity;
import kodkod.ast.MultiplicityFormula;
import kodkod.ast.Node;
import kodkod.ast.NotFormula;
import kodkod.ast.QuantifiedFormula;
import kodkod.ast.Relation;
import kodkod.ast.RelationPredicate;
import kodkod.ast.SumExpression;
import kodkod.ast.Variable;
import kodkod.ast.visitor.DepthFirstDetector;
import kodkod.ast.visitor.DepthFirstReplacer;
import kodkod.engine.bool.BooleanMatrix;
import kodkod.engine.settings.Options;
import kodkod.engine.settings.Reporter;
import kodkod.instance.Bounds;
import kodkod.instance.TupleSet;
import kodkod.util.collections.ArrayStack;
import kodkod.util.collections.Stack;

/**
 * Skolemizes existential quantifiers, up to a given
 * number of nestings (within universal quantifiers). 
 * @author Emina Torlak
 */
final class Skolemizer {

	/**
	 * Skolemizes the given annotated formula using the given bounds and options.
	 * @effects upper bound mappings for skolem constants, if any, are added to the bounds
	 * @return the skolemized version of the given formula
	 * @throws NullPointerException - any of the arguments are null
	 * @throws IllegalArgumentException - some Relation & annotated.node.^children - bounds.relations
	 * @throws UnsupportedOperationException - bounds is unmodifiable
	 */
	@SuppressWarnings("unchecked")
	static AnnotatedNode<Formula> skolemize(AnnotatedNode<Formula> annotated, Bounds bounds, Options options) {
		final SkolemReplacer r = new SkolemReplacer(annotated.sharedNodes(), bounds, options);
		final Formula f = annotated.node().accept(r).and(r.skolemConstraints);
//		System.out.println(annotated.node());
//		System.out.println(f);
		return f==annotated.node() ? annotated : new AnnotatedNode<Formula>(f);
	}

	/**
	 * Given a formula, bounds and depth, 
	 * detects skolemizable formulas, replaces them
	 * with their skolemized equivalents, and collects the necessary 
	 * skolemization constraints. 
	 * @author Emina Torlak
	 */
	private static final class SkolemReplacer extends DepthFirstReplacer {	
		/* replacement environment; maps skolemized variables to their skolem expressions,
		 * and non-skolemized variables to themselves */
		private Environment<Expression> repEnv;
		/* when computing the upper bounds for skolems, all
		 * expressions must be replaced with sound approximations;
		 * specifically all difference expressions must replaced with 
		 * their left children. */
		private final DifferenceRemover diffRemover ;
		/* the interpreter used to determine the upper bounds for skolem constants;
		 * the upper bounds for skolem constants will be added to interpreter.bounds */
		private final LeafInterpreter interpreter;
		/* bounds on which the interpreter is based */
		private final Bounds bounds;
		/* reporter */
		private final Reporter reporter;
		/* non-skolemizable quantified declarations in the current scope, in the order of declaration
		 * (most recent decl is last in the list) */
		private final List<DeclInfo> nonSkolems;
		/* true if the polarity of the currently visited node is negative, otherwise false */
		private boolean negated;
		/* depth to which to skolemize; negative depth indicates that no skolemization can be done at that point */
		private int skolemDepth;
		
		/** holds the skolemization constraints at the end of the visit */
		Formula skolemConstraints;
		
		/**
		 * Constructs a skolem replacer from the given arguments. 
		 */
		SkolemReplacer(Set<Node> sharedNodes, Bounds bounds, Options options) {
			super(sharedNodes);
			
			// only cache intermediate computations for expressions with no free variables
			// and formula with no free variables and no quantified descendents
			final FreeVarDetector fvdetect = new FreeVarDetector(sharedNodes);
			final DepthFirstDetector qdetect = new DepthFirstDetector(sharedNodes) {
				public Boolean visit(QuantifiedFormula quantFormula) {
					return cache(quantFormula, true);
				}
			};
			for(Node n: sharedNodes) {
				if (!(Boolean)n.accept(fvdetect)) {
					if (!(n instanceof Formula) || !((Boolean)n.accept(qdetect)))
						this.cache.put(n, null);
				}
			}
			this.reporter = options.reporter();
			this.diffRemover = new DifferenceRemover(this.cache.keySet());
			this.bounds = bounds;
			this.interpreter = LeafInterpreter.overapproximating(bounds, options);
			this.skolemConstraints = Formula.TRUE;
			this.repEnv = Environment.empty();
			this.nonSkolems = new ArrayList<DeclInfo>();
			this.negated = false;
			this.skolemDepth = options.skolemDepth();
		}
		
		/**
		 * Caches the given replacement for the specified node, if this is 
		 * a caching visitor and the node is a syntactically shared expression with
		 * no free variables or a syntactically shared formula with no free variables
		 * and no quantified descendents.  Otherwise does nothing.  The method returns
		 * the replacement node.  
		 * @return replacement
		 */
		@Override
		protected <N extends Node> N cache(N node, N replacement) {
			if (cache.containsKey(node)) {
				cache.put(node, replacement);
			}
			return replacement;
		}
		
		/*-------declarations---------*/
		/** 
		 * Visits the given decl's expression.  Note that we must not visit variables 
		 * in case they are re-used.  For example, consider the formula
		 * some x: X | all x: Y | F(x).  Since x bound by the existential quantifier
		 * is going to be skolemized, if we visited the variable in the enclosed
		 * declaration, we would get the skolem constant as a return value and
		 * a ClassCastException would be thrown.
		 * 
		 * @return { d: Declaration |  d.variable = decl.variable && d.multiplicity = decl.multiplicity &&
		 *                             d.expression = decl.expression.accept(this) } 
		 */
		@Override
		public Decl visit(Decl decl) {
			Decl ret = lookup(decl);
			if (ret!=null) return ret;
			final int oldDepth = skolemDepth;
			skolemDepth = -1; // can't skolemize inside a decl
			final Expression expression = decl.expression().accept(this);
			skolemDepth = oldDepth;
			ret = (expression==decl.expression()) ? decl : decl.variable().declare(decl.multiplicity(), expression); 	
			return cache(decl,ret);
		}
		
		/** 
		 * This method should be accessed only from the context of a non-skolemizable
		 * node, because it  extends the replacement environment
		 * with  identity mappings for the variables declared in the given decls.  To ensure
		 * that the environment is always extended, the method should be called using the
		 * visit((Decls) node.declarations()) syntax, since the accept syntax may dynamically
		 * dispatch the call to the {@link #visit(Decl)} method, producing UnboundLeafExceptions.
		 * @effects this.repEnv in this.repEnv'.^parent &&
		 * #(this.repEnv'.*parent - this.repEnv.*parent) = decls.size() &&
		 * all v: decls.variable | this.repEnv'.lookup(v) = v
		 * @requires this.skolemDepth < 0
		 * @return { d: Decls | d.size = decls.size && 
		 *                      all i: [0..d.size) | d.declarations[i] = decls.declarations[i].accept(this) } 
		 */
		public Decls visit(Decls decls) { 
			Decls ret = lookup(decls);
			if (ret==null) {
				Decls visitedDecls = null;
				boolean allSame = true;
				for(Decl decl : decls) {
					Decls newDecl = visit(decl);
					if (newDecl != decl) 
						allSame = false;
					visitedDecls = (visitedDecls==null) ? newDecl : visitedDecls.and(newDecl);
					repEnv = repEnv.extend(decl.variable(), decl.variable());
				}
				ret = allSame ? decls : visitedDecls;
				return cache(decls, ret);
			} else { // just extend the replacement environment
				for(Decl decl: decls) {
					repEnv = repEnv.extend(decl.variable(), decl.variable());
				}
				return ret;
			}
		}
		
		/*-------expressions and intexpressions---------*/
		/* INVARIANT:  whenever an expression or intexpression is visited, skolemDepth < 0 */
		/** 
		 * Returns the binding for the variable in this.repEnv.
		 * @return this.repEnv.lookup(variable)
		 * @throws IllegalArgumentException - no this.repEnv.lookup(variable)
		 */
		@Override
		public Expression visit(Variable variable) { 
			final Expression ret = repEnv.lookup(variable);
			if (ret==null)
				throw new UnboundLeafException("Unbound variable", variable);
			return ret;
		}	
		@Override
		public Expression visit(Comprehension expr) {
			Expression ret = lookup(expr);
			if (ret!=null) return ret;
			final Environment<Expression> oldRepEnv = repEnv; // skolemDepth < 0 at this point
			final Decls decls = visit((Decls)expr.declarations());
			final Formula formula = expr.formula().accept(this);
			ret = (decls==expr.declarations() && formula==expr.formula()) ? expr : formula.comprehension(decls);
			repEnv = oldRepEnv;		
			return cache(expr,ret);
		}
		@Override
	    public IntExpression visit(SumExpression intExpr) {
			IntExpression ret = lookup(intExpr);
			if (ret!=null) return ret;	
			final Environment<Expression> oldRepEnv = repEnv; // skolemDepth < 0 at this point
			final Decls decls  = visit((Decls)intExpr.declarations());
			final IntExpression expr = intExpr.intExpr().accept(this);
			ret =  (decls==intExpr.declarations() && expr==intExpr.intExpr()) ? intExpr : expr.sum(decls);
			repEnv = oldRepEnv;
			return cache(intExpr,ret);
	    }
	    
		/*-------formulas---------*/
		/**
		 * Removes all difference subexpression from expr and evaluates it using
		 * this.intepreter and the given environment.
		 * @return the least sound upper bound on the value of expr
		 */
		private final BooleanMatrix upperBound(Expression expr, Environment<BooleanMatrix> env) {
			return (BooleanMatrix) 
			  FOL2BoolTranslator.translate(new AnnotatedNode<Expression>(expr.accept(diffRemover)), interpreter, env);
		}
		/**
		 * Creates a skolem relation for decl.variable, bounds it in 
		 * this.interpreter.boundingObject, and returns the expression 
		 * that should replace decl.variable in the final formula.
		 * @requires decl has been visited by this 
		 * @effects bounds the skolem relation for decl in this.interpreter.boundingObject
		 * @return the expression that should replace decl.variable in 
		 * the final formula
		 */
		private Expression skolemExpr(Decl decl) {
			final int depth = nonSkolems.size();
			final int arity = depth + decl.variable().arity();
			
			final Relation skolem = Relation.nary("$"+decl.variable().name(), arity);
			reporter.skolemizing(decl, skolem);
			
			Expression skolemExpr = skolem;
			Environment<BooleanMatrix> skolemEnv = Environment.empty();
			
			for(DeclInfo info : nonSkolems) {
				if (info.upperBound==null) {
					info.upperBound = upperBound(info.decl.expression(), skolemEnv);
				}
				skolemEnv = skolemEnv.extend(info.decl.variable(), info.upperBound);
				skolemExpr = info.decl.variable().join(skolemExpr);
			}
			
			BooleanMatrix matrixBound = upperBound(decl.expression(), skolemEnv);
			for(int i = depth-1; i >= 0; i--) {
				matrixBound = nonSkolems.get(i).upperBound.cross(matrixBound);
			}
			
			final TupleSet skolemBound = bounds.universe().factory().setOf(arity, matrixBound.denseIndices());
			bounds.bound(skolem, skolemBound);
			
			return skolemExpr;
		}	
		/**
		 * Returns the skolemization constraints for the given 
		 * declaration and its corresponding skolemization expression.
		 * @requires decl has been visited by this
		 * @requires skolemExpr = skolemExpr(decl)
		 * @return skolemization constraints for the given 
		 * declaration and its corresponding skolemization expression.
		 */
		private Formula skolemFormula(Decl decl, Expression skolemExpr) {
			Formula f = skolemExpr.in(decl.expression());
			
			if (decl.multiplicity()!=Multiplicity.SET)
				f = f.and(skolemExpr.apply(decl.multiplicity()));
			
			final Iterator<DeclInfo> infos = nonSkolems.iterator();
			if (infos.hasNext()) {
				Decls decls = infos.next().decl;
				while(infos.hasNext()) {
					decls = decls.and(infos.next().decl);
				}
				f = f.forAll(decls);
			} 
			
			return f;
		}	
		
		/**
		 * Skolemizes the given formula, if possible, otherwise returns the result
		 * of replacing its free variables according to the current repEnv.
		 * @see kodkod.ast.visitor.DepthFirstReplacer#visit(kodkod.ast.QuantifiedFormula)
		 */
		public Formula visit(QuantifiedFormula qf) {
			Formula ret = lookup(qf);
			if (ret!=null) return ret;
			final Environment<Expression> oldRepEnv = repEnv;	
			final QuantifiedFormula.Quantifier quant = qf.quantifier();
			if (skolemDepth>=0 && (negated && quant==ALL || !negated && quant==SOME)) { // skolemizable formula
				for(Decl decl : qf.declarations()) {
					Decl newDecl = visit(decl);
					Expression skolemExpr = skolemExpr(newDecl);
					repEnv = repEnv.extend(decl.variable(), skolemExpr);
					skolemConstraints = skolemConstraints.and(skolemFormula(newDecl, skolemExpr));
				}
				ret = qf.formula().accept(this);
			} else { // non-skolemizable formula
				final Decls decls = visit((Decls)qf.declarations());
				if (skolemDepth>=nonSkolems.size()+decls.size()) { // can skolemize below
					for(Decl d: decls) { nonSkolems.add(new DeclInfo(d)); }
					final Formula formula = qf.formula().accept(this);
					ret = ((decls==qf.declarations() && formula==qf.formula()) ? qf : formula.quantify(quant, decls));
					for(int i = decls.size(); i > 0; i--) { nonSkolems.remove(nonSkolems.size()-1); }
				} else { // can't skolemize below
					final int oldDepth = skolemDepth;
					skolemDepth = -1; 
					final Formula formula = qf.formula().accept(this);
					ret = ((decls==qf.declarations() && formula==qf.formula()) ? qf : formula.quantify(quant, decls));
					skolemDepth = oldDepth;
				}				
			}			
			repEnv = oldRepEnv;
			return cache(qf,ret);
		}
		
		/** 
		 * Calls not.formula.accept(this) after flipping the negation flag and returns the result. 
		 * @see kodkod.ast.visitor.DepthFirstReplacer#visit(kodkod.ast.NotFormula)
		 **/
		public Formula visit(NotFormula not) {
			Formula ret = lookup(not);
			if (ret!=null) return ret;
			negated = !negated; // flip the negation flag
			final Formula retChild = not.formula().accept(this);
			negated = !negated;
			return retChild==not.formula() ? cache(not,not) : cache(not, retChild.not());			
		}
		
		/**
		 * If not cached, visits the formula's children with appropriate settings
		 * for the negated flag and the skolemDepth parameter.
		 * @see kodkod.ast.visitor.DepthFirstReplacer#visit(kodkod.ast.BinaryFormula)
		 */
		public Formula visit(BinaryFormula bf) {
			Formula ret = lookup(bf);
			if (ret!=null) return ret;			
			final BinaryFormula.Operator op = bf.op();
			final int oldDepth = skolemDepth;
			if (op==IFF || (negated && op==AND) || (!negated && (op==OR || op==IMPLIES))) { // cannot skolemize in these cases
				skolemDepth = -1;
			}
			final Formula left, right;
			if (negated && op==IMPLIES) { // !(a => b) = !(!a || b) = a && !b
				negated = !negated;
				left = bf.left().accept(this);
				negated = !negated;
				right = bf.right().accept(this);
			} else {
				left = bf.left().accept(this);
				right = bf.right().accept(this);
			}
			skolemDepth = oldDepth;
			ret = (left==bf.left()&&right==bf.right()) ? bf : left.compose(op, right);
			return cache(bf,ret);
		}
		

		/** 
		 * Calls super.visit(icf) after disabling skolemization and returns the result. 
		 * @return super.visit(icf) 
		 **/
		public Formula visit(IntComparisonFormula icf) {
			final int oldDepth = skolemDepth;
			skolemDepth = -1; // cannot skolemize inside an int comparison formula
			final Formula ret = super.visit(icf);
			skolemDepth = oldDepth;
			return ret;
		}
		
		/** 
		 * Calls super.visit(cf) after disabling skolemization and returns the result. 
		 * @return super.visit(cf) 
		 **/
		public Formula visit(ComparisonFormula cf) {
			final int oldDepth = skolemDepth;
			skolemDepth = -1; // cannot skolemize inside a comparison formula
			final Formula ret = super.visit(cf);
			skolemDepth = oldDepth;
			return ret;
		}
		
		/** 
		 * Calls super.visit(mf) after disabling skolemization and returns the result. 
		 * @return super.visit(mf) 
		 **/
		public Formula visit(MultiplicityFormula mf) {
			final int oldDepth = skolemDepth;
			skolemDepth = -1; // cannot skolemize inside a multiplicity formula
			final Formula ret = super.visit(mf);
			skolemDepth = oldDepth;
			return ret;
		}
		
		/** 
		 * Calls super.visit(pred) after disabling skolemization and returns the result. 
		 * @return super.visit(pred) 
		 **/
		public Formula visit(RelationPredicate pred) {
			final int oldDepth = skolemDepth;
			skolemDepth = -1; // cannot skolemize inside a relation predicate
			final Formula ret = super.visit(pred);
			skolemDepth = oldDepth;
			return ret;
		}
	}
	
	/**
	 * Contains info about an approximate bound for a 
	 * non-skolemizable decl.
	 * @specfield decl: Decl
	 * @specfield upperBound: lone BooleanMatrix
	 * @invariant decl.expression in upperBound
	 * @author Emina Torlak
	 */
	private static final class DeclInfo {
		final Decl decl;
		BooleanMatrix upperBound;
		/**
		 * Constructs a DeclInfo for the given decl.
		 * @effects this.decl' = decl && this.upperBound' = null
		 */
		DeclInfo(Decl decl) {
			this.decl = decl;
			this.upperBound =  null;
		}
	}
	
	/**
	 * Replaces all difference subexpressions in a node
	 * with their left children.   
	 */
	private static final class DifferenceRemover extends DepthFirstReplacer {
		/**
		 * Creates a difference remover which will cache replacement values
		 * for the given nodes.
		 */
		DifferenceRemover(Set<Node> sharedNodes) {
			super(sharedNodes);
		}
		/**
		 * Returns super.visit(binExpr) if binExpr is not a difference
		 * expression, otherwise return binExpr.left().accept(this). 
		 * @return super.visit(binExpr) if binExpr is not a difference
		 * expression, otherwise return binExpr.left().accept(this). 
		 */
		public Expression visit(BinaryExpression binExpr) {
			Expression ret = lookup(binExpr);
			if (ret!=null) return ret;
			if (binExpr.op()==BinaryExpression.Operator.DIFFERENCE)
				ret = binExpr.left().accept(this);
			else {
				final Expression left = binExpr.left().accept(this);
				final Expression right = binExpr.right().accept(this);
				ret = left==binExpr.left()&&right==binExpr.right() ? binExpr : 
					left.compose(binExpr.op(), right);
			}
			return cache(binExpr, ret);
		}	
	}
	
	/**
	 * Detects if a given node contains free variables.
	 */
	private static final class FreeVarDetector extends DepthFirstDetector {
		/* Holds the variables that are currently in scope, with the
		 * variable at the top of the stack being the last declared variable. */
		private final Stack<Variable> varsInScope;
		
		protected FreeVarDetector(Set<Node> cached) {
			super(cached);
			this.varsInScope = new ArrayStack<Variable>();
		}	
		/**
		 * Visits the given comprehension, quantified formula, or sum expression.  
		 * The method returns TRUE if the creator body contains any 
		 * variable not bound by the decls; otherwise returns FALSE.  
		 */
		@SuppressWarnings("unchecked")
		private Boolean visit(Node creator, Decls decls, Node body) {
			Boolean ret = lookup(creator);
			if (ret!=null) return ret;
			boolean retVal = false;
			for(Decl decl : decls) {
				retVal = decl.expression().accept(this) || retVal;
				varsInScope.push(decl.variable());
			}
			retVal = ((Boolean)body.accept(this)) || retVal;
			for(int i = decls.size(); i > 0; i--) {
				varsInScope.pop();
			}
			return cache(creator, retVal);
		}
		/**
		 * Returns TRUE if the given variable is free in its parent, otherwise returns FALSE.
		 * @return TRUE if the given variable is free in its parent, otherwise returns FALSE.
		 */
		public Boolean visit(Variable variable) {
			return Boolean.valueOf(varsInScope.search(variable)<0);
		}	
		
		public Boolean visit(Decl decl) {
			Boolean ret = lookup(decl);
			if (ret!=null) return ret;
			return cache(decl, decl.expression().accept(this));
		}	
		
		public Boolean visit(Comprehension comprehension) {
			return visit(comprehension, comprehension.declarations(), comprehension.formula());
		}		
		
		public Boolean visit(SumExpression intExpr) {
			return visit(intExpr, intExpr.declarations(), intExpr.intExpr());
		}
		
		public Boolean visit(QuantifiedFormula qformula) {
			return visit(qformula, qformula.declarations(), qformula.formula());
		}
	}
}
