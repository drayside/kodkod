package kodkod.engine.fol2sat;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
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
import kodkod.instance.Bounds;
import kodkod.instance.TupleSet;

/**
 * Skolemizes away existential quantifiers, up to a given
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
		final Set<QuantifiedFormula> formulas = AnnotatedNode.skolemizables(annotated, options.skolemDepth());
		
		if (formulas.isEmpty()) {
			return annotated;
		} else {
			final EQFReplacer replacer = new EQFReplacer(formulas, annotated.sharedNodes(), bounds, options);
			final Formula f = annotated.node().accept(replacer).and(replacer.skolemFormula);
//			System.out.println(annotated.node());
//			System.out.println(f);
//			System.out.println(bounds);
			return new AnnotatedNode<Formula>(f);
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
		private Environment<Expression> repEnv;
		/* when computing the upper bounds for skolems, all
		 * expressions must be replaced with sound approximations;
		 * specifically all difference expressions must replaced with 
		 * their left children.
		 */
		private final DifferenceRemover diffRemover ;
		/* the interpreter used to determine the upper bounds for skolem constants;
		 * the upper bounds for skolem constants will be added to interpreter.bounds */
		private final BoundsInterpreter.Overapproximating interpreter;
		/* non-skolemizable quantified declarations in the current scope, in the order of declaration
		 * (most recent decl is last in the list) 
		 */
		private final List<DeclInfo> nonSkolems;
		
		/* the formula that constrains all the skolem constants */
		Formula skolemFormula;
		
		/**
		 * Constructs a new EQFReplacer.  This replacer should only be applied to
		 * the top-level formula, root.  The bounds will be modified to include
		 * upper bounds for the skolem constants generated during replacement.
		 * @requires sharedNodes = {n: Node | #(n.~children & this.root'.^children) > 1 }
		 * @requires root.*children & Relation in interpreter.bounds.relations
		 * @effects this.eqfs' = eqfs && this.bounds' = interpreter.bounds
		 */
		EQFReplacer(Set<QuantifiedFormula> eqfs, Set<Node> sharedNodes, Bounds bounds, Options options) {
			super(sharedNodes);
			this.eqfs = eqfs;
			this.interpreter = new BoundsInterpreter.Overapproximating(bounds, BooleanFactory.constantFactory(options));
			this.skolemFormula = Formula.TRUE;
			this.repEnv = new Environment<Expression>();
			this.diffRemover = new DifferenceRemover(sharedNodes);
			this.nonSkolems = new ArrayList<DeclInfo>();
		}
		
		
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
			
			final Expression expression = decl.expression().accept(this);
			ret = (expression==decl.expression()) ?
				  decl : decl.variable().declare(decl.multiplicity(), expression); 	
			return cache(decl,ret);
		}
		
		/** 
		 * Returns the binding for the variable in this.env.
		 * @return this.env.lookup(variable)
		 * @throws IllegalArgumentException - no this.env.lookup(variable)
		 */
		@Override
		public Expression visit(Variable variable) { 
			final Expression ret = repEnv.lookup(variable);
			if (ret==null)
				throw new UnboundLeafException("Unbound variable", variable);
			return ret;
		}
		
		/** 
		 * This method should be accessed only from the context of a non-skolemizable
		 * formula or a comprehension, because it  extends the replacement environment
		 * with  identity mappings for the variables declared in the given decls.  To ensure
		 * that the environment is always extended, the method should be called using the
		 * visit((Decls) node.declarations()) syntax, since the accept syntax may dynamically
		 * dispatch the call to the {@link #visit(Decl)} method, producing UnboundLeafExceptions.
		 * 
		 * <p>Calls lookup(decls) and returns the cached value, if any.  
		 * If a replacement has not been cached, visits each of the children's 
		 * variable and expression.  If nothing changes, the argument is cached and
		 * returned, otherwise a replacement Decls object is cached and returned.</p>
		 * @effects this.repEnv in this.repEnv'.^parent &&
		 * #(this.repEnv'.*parent - this.repEnv.*parent) = decls.size() &&
		 * all v: decls.variable | this.repEnv'.lookup(v) = v
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
			if (ret!=null) return ret;
			
			final Environment<Expression> oldRepEnv = repEnv;
			
			final Decls decls = visit((Decls)comprehension.declarations());
			final Formula formula = comprehension.formula().accept(this);
			ret = (decls==comprehension.declarations() && formula==comprehension.formula()) ? 
				  comprehension : formula.comprehension(decls);
			
			repEnv = oldRepEnv;
			
			return cache(comprehension,ret);
		}
		
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
			Expression skolemExpr = skolem;
			Environment<BooleanMatrix> skolemEnv = new Environment<BooleanMatrix>();
			
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
			
			final TupleSet skolemBound = interpreter.universe().factory().setOf(arity, matrixBound.denseIndices());
			interpreter.boundingObject().bound(skolem, skolemBound);
			
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
			if (ret!=null) return ret;
			
			final Environment<Expression> oldRepEnv = repEnv;
			
			if (eqfs.contains(quantFormula)) { // skolemizable formula
				for(Decl decl : quantFormula.declarations()) {
					Decl newDecl = visit(decl);
					Expression skolemExpr = skolemExpr(newDecl);
					repEnv = repEnv.extend(decl.variable(), skolemExpr);
					skolemFormula = skolemFormula.and(skolemFormula(newDecl, skolemExpr));
				}
				ret = quantFormula.formula().accept(this);
				
			} else { // non-skolemizable formula
				
				final Decls decls = visit((Decls)quantFormula.declarations());
			
				for(Decl d: decls) {
					nonSkolems.add(new DeclInfo(d));
				}
			
				final Formula formula = quantFormula.formula().accept(this);
				ret = ((decls==quantFormula.declarations() && formula==quantFormula.formula()) ? 
					    quantFormula : formula.quantify(quantFormula.quantifier(), decls));
				
				for(int i = decls.size(); i > 0; i--) { 
					nonSkolems.remove(nonSkolems.size()-1); 
				}
			}			
			
			repEnv = oldRepEnv;
			return cache(quantFormula,ret);
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
				return cache(binExpr, binExpr.left().accept(this));
			else
				return super.visit(binExpr);
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
}
