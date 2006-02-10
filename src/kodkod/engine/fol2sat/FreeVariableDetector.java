package kodkod.engine.fol2sat;

import java.util.Iterator;
import java.util.Set;

import kodkod.ast.BinaryExpression;
import kodkod.ast.BinaryFormula;
import kodkod.ast.ComparisonFormula;
import kodkod.ast.Comprehension;
import kodkod.ast.ConstantExpression;
import kodkod.ast.ConstantFormula;
import kodkod.ast.Decl;
import kodkod.ast.Decls;
import kodkod.ast.Formula;
import kodkod.ast.IfExpression;
import kodkod.ast.MultiplicityFormula;
import kodkod.ast.Node;
import kodkod.ast.NotFormula;
import kodkod.ast.QuantifiedFormula;
import kodkod.ast.Relation;
import kodkod.ast.RelationPredicate;
import kodkod.ast.UnaryExpression;
import kodkod.ast.Variable;
import kodkod.ast.visitor.ReturnVisitor;
import kodkod.util.LinkedStack;
import kodkod.util.Stack;


/**
 * Detects free variables at the leaves of 
 * an abstract syntax tree / dag. 
 * @author Emina Torlak
 */
final class FreeVariableDetector {

	private FreeVariableDetector() { }
	
	/**
	 * Removes from the given set all nodes that contain free variables.
	 * @effects nodes' = nodes - 
	 *           {n: nodes | some Variable & n.*children - (n.*children & Decls).variable}
	 * @throws NullPointerException - nodes = null
	 * @throws UnsupportedOperationException - nodes is unmodifiable
	 */
	static final void retainBoundNodes(Set<? extends Node> nodes) {
		final Detector detector = new Detector();
		for(Iterator<? extends Node> nIter = nodes.iterator(); nIter.hasNext(); ) {
			if ((Boolean)nIter.next().accept(detector))
				nIter.remove();
		}
	}
	
	/**
	 * The helper visitor that detects free variables.
	 * @specfield freeVarMap: Node -> lone Boolean
	 */
	private static final class Detector implements ReturnVisitor<Boolean,Boolean,Boolean> {
//		private final Map<N, Boolean> freeVarMap;
		/* Holds the variables that are currently in scope, with the
		 * variable at the top of the stack being the last declared variable. */
		private final Stack<Variable> varsInScope;
		
		/**
		 * Constructs a new detector.
		 */
		Detector(/*Set<N> nodes*/) {
//			this.freeVarMap = new IdentityHashMap<N,Boolean>(nodes.size());
//			for(N n : nodes) {
//				freeVarMap.put(n, null);
//			}
			varsInScope = new LinkedStack<Variable>();
		}
		
		/**
		 * If the given node has already been visited, 
		 * and it is a node for which we are detecting free 
		 * variables, the previously computed value is returned.
		 * Otherwise, null is returned.
		 * @return freeVarMap[node]
		 */
//		private final Boolean cachedValue(Node node) {
//			return freeVarMap.get(node);
//		}
		
		/**
		 * If we are detecting free variables for the given node,
		 * the node and the value is cached.  Otherwise, nothing
		 * happens.  The value itself is returned.
		 * @return value
		 */
		@SuppressWarnings("unchecked")
//		private final Boolean cache(Node node, Boolean value) {
//			if (freeVarMap.containsKey(node)) {
//				freeVarMap.put((N)node, value);
//			}
//			return value;
//		}
		
		public Boolean visit(Decls decls) {
//			Boolean hasFreeVars = cachedValue(decls);
//			if (hasFreeVars != null) return hasFreeVars;
//			for(Decl decl : decls) {
//				if ((hasFreeVars = visit(decl)).booleanValue()) 
//					break;
//			}
//			return cache(decls, hasFreeVars);
			Boolean hasFreeVars = null;
			for(Decl decl : decls) {
				if ((hasFreeVars = visit(decl)).booleanValue()) 
					break;
			}
			return hasFreeVars;
		}

		public Boolean visit(Decl decl) {
//			final Boolean hasFreeVars = cachedValue(decl);
//			return (hasFreeVars != null) ? hasFreeVars : 
//				   cache(decl, decl.expression().accept(this));
			return decl.expression().accept(this);
		}

		public Boolean visit(Relation relation) {
			return Boolean.FALSE;
		}

		public Boolean visit(Variable variable) {
			return (varsInScope.search(variable) > 0) ? 
				   Boolean.FALSE : Boolean.TRUE;
		}

		public Boolean visit(ConstantExpression constExpr) {
			return Boolean.FALSE;
		}

		public Boolean visit(BinaryExpression binExpr) {
//			final Boolean hasFreeVars = cachedValue(binExpr);
//			if (hasFreeVars != null) return hasFreeVars;
//			return binExpr.left().accept(this).booleanValue() ?
//				   cache(binExpr, Boolean.TRUE) : 
//				   cache(binExpr, binExpr.right().accept(this));
			return binExpr.left().accept(this).booleanValue() ?
				   Boolean.TRUE : binExpr.right().accept(this);
		}

		public Boolean visit(UnaryExpression unaryExpr) {
//			final Boolean hasFreeVars = cachedValue(unaryExpr);
//			if (hasFreeVars != null) return hasFreeVars;
//			return cache(unaryExpr, unaryExpr.expression().accept(this));
			return unaryExpr.expression().accept(this);
		}

		public Boolean visit(IfExpression ifExpr) {
//			final Boolean hasFreeVars = cachedValue(ifExpr);
//			if (hasFreeVars != null) return hasFreeVars;
//			if (ifExpr.condition().accept(this) || 
//				    ifExpr.thenExpr().accept(this) ||
//				    ifExpr.elseExpr().accept(this)) {
//				return cache(ifExpr, Boolean.TRUE);
//			} else {
//				return cache(ifExpr, Boolean.FALSE);
//			}
			if (ifExpr.condition().accept(this) || 
				    ifExpr.thenExpr().accept(this) ||
				    ifExpr.elseExpr().accept(this)) {
				return Boolean.TRUE;
			} else {
				return Boolean.FALSE;
			}
		}
		
		/**
		 * Visits the given comprehension or quantified formula.  The method returns 
		 * TRUE if either a declaration of the body of the formula contains free
		 * variables; otherwise it returns false;
		 */
		@SuppressWarnings("unchecked")
		private Boolean visit(Node creator, Decls decls, Formula formula) {
//			Boolean hasFreeVars = cachedValue(creator);
//			if (hasFreeVars != null) return hasFreeVars;
//			for(Decl decl : decls) {
//				varsInScope.push(decl.variable());
//			}
//			hasFreeVars = decls.accept(this).booleanValue() ? 
//					      Boolean.TRUE : formula.accept(this);
//			for(int i = decls.size(); i > 0; i--) {
//				varsInScope.pop();
//			}
//			return cache(creator, hasFreeVars);
			for(Decl decl : decls) {
				varsInScope.push(decl.variable());
			}
			Boolean hasFreeVars = decls.accept(this).booleanValue() ? Boolean.TRUE : formula.accept(this);
			for(int i = decls.size(); i > 0; i--) {
				varsInScope.pop();
			}
			return hasFreeVars;
		}
		
		public Boolean visit(Comprehension comprehension) {
			return visit(comprehension, comprehension.declarations(), comprehension.formula());
		}

		public Boolean visit(QuantifiedFormula quantFormula) {
			return visit(quantFormula, quantFormula.declarations(), quantFormula.formula());
		}

		public Boolean visit(BinaryFormula binFormula) {
//			final Boolean hasFreeVars = cachedValue(binFormula);
//			if (hasFreeVars != null) return hasFreeVars;
//			return binFormula.left().accept(this).booleanValue() ?
//				   cache(binFormula, Boolean.TRUE) : 
//				   cache(binFormula, binFormula.right().accept(this));
			return binFormula.left().accept(this).booleanValue() ?
				   Boolean.TRUE : binFormula.right().accept(this);
		}

		public Boolean visit(NotFormula not) {
//			final Boolean hasFreeVars = cachedValue(not);
//			if (hasFreeVars != null) return hasFreeVars;
//			return cache(not, not.formula().accept(this));
			return not.formula().accept(this);
		}

		public Boolean visit(ConstantFormula constant) {
			return Boolean.FALSE;
		}

		public Boolean visit(ComparisonFormula compFormula) {
//			final Boolean hasFreeVars = cachedValue(compFormula);
//			if (hasFreeVars != null) return hasFreeVars;
//			return compFormula.left().accept(this).booleanValue() ?
//				   cache(compFormula, Boolean.TRUE) : 
//				   cache(compFormula, compFormula.right().accept(this));
			return compFormula.left().accept(this).booleanValue() ?
				   Boolean.TRUE : compFormula.right().accept(this);
		}

		public Boolean visit(MultiplicityFormula multFormula) {
//			final Boolean hasFreeVars = cachedValue(multFormula);
//			if (hasFreeVars != null) return hasFreeVars;
//			return cache(multFormula, multFormula.expression().accept(this));
			return multFormula.expression().accept(this);
		}

		public Boolean visit(RelationPredicate predicate) {
//			final Boolean hasFreeVars = cachedValue(predicate);
//			if (hasFreeVars != null) return hasFreeVars;
//			if (predicate.name() != RelationPredicate.Name.FUNCTION)
//				return cache(predicate, Boolean.FALSE);
//			final RelationPredicate.Function fun = (RelationPredicate.Function) predicate;
//			return fun.domain().accept(this).booleanValue() ? 
//				   cache(predicate, Boolean.TRUE) : 
//				   cache(predicate, fun.range().accept(this));
			if (predicate.name() != RelationPredicate.Name.FUNCTION)
				return Boolean.FALSE;
			final RelationPredicate.Function fun = (RelationPredicate.Function) predicate;
			return fun.domain().accept(this).booleanValue() ? 
				   Boolean.TRUE : fun.range().accept(this);
		}
		
	}
}
