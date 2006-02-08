package kodkod.engine.fol2sat;

import java.util.Collections;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.LinkedHashSet;
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
import kodkod.util.ArrayStack;
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
	 * Collects the free variables in each of the subtrees rooted 
	 * at the given nodes.  The returned map 
	 * binds each node to a set 
	 * comprised of the node's free variables.  
	 * The free variable sets' iterators
	 * are guaranteed to return the variables in the reverse order
	 * of declaration; i.e. the last declared variable will be 
	 * returned first, and the first one will be returned last.
	 * The map tests for key presence using reference equality.
	 * @return { m: Map | all n: nodes | 
	 *           m[n] = Variable & n.*children - (n.*children & Decls).variable }
	 * @throws NullPointerException - nodes = null
	 */
	static final Map<Node, Set<Variable>> collectFreeVars(Set<Node> nodes) {
		final Collector detector = new Collector(nodes);
		for(Node n : nodes) {
			n.accept(detector);
		}
		return detector.freeVarMap;
	}
	
	/**
	 * The helper visitor that collects free variables.
	 * @specfield freeVarMap: Node -> lone Set<Variable>
	 */
	private static final class Collector implements ReturnVisitor<Set<Variable>,Set<Variable>,Set<Variable>> {
		private final Map<Node, Set<Variable>> freeVarMap;
		/* Holds the variables that are currently in scope, with the
		 * variable at the top of the stack being the last declared variable. */
		private final ArrayStack<Variable> varsInScope;
				
		/**
		 * Constructs a new collector for the given set of nodes.
		 * The visitor should only be applied to the nodes in the
		 * given set.
		 */
		Collector(Set<Node> nodes) {
			this.freeVarMap = new IdentityHashMap<Node,Set<Variable>>(nodes.size());
			for(Node n : nodes) {
				freeVarMap.put(n, null);
			}
			varsInScope = new ArrayStack<Variable>();
		}

		/**
		 * If the given node has already been visited and
		 * it is a node for which we are collecting free 
		 * variables, the previously computed set is returned.
		 * Otherwise, null is returned.
		 * @return freeVarMap[node]
		 */
		private final Set<Variable> cachedVars(Node node) {
			return freeVarMap.get(node);
		}
		
		/**
		 * If the given node is one for which we are collecting
		 * free variable information, we add a binding from it
		 * to a set with the same contents as the specified set, 
		 * but whose iterator returns the variables in the reverse
		 * order of declaration.  Otherwise,
		 * freeVarMap is not changed.  The specified set is returned.
		 * @effects node in freeVarMap.keySet() => freeVarMap' = freeVarMap ++ node->varsInScope,
		 *             freeVarMap' = freeVarMap
		 * @return freeVars
		 */
		private final Set<Variable> cache(Node node, Set<Variable> freeVars) {
			if (freeVarMap.containsKey(node)) {
				final int numVars = freeVars.size();
				if (numVars < 2)
					freeVarMap.put(node, freeVars);
				else {
					final Set<Variable> orderedVars = new LinkedHashSet<Variable>((numVars * 4) / 3 + 1);
					for(Variable var : varsInScope) {
						if (freeVars.contains(var)) {
							orderedVars.add(var);
							if (orderedVars.size()==numVars) 
								break;
						}
					}
					freeVarMap.put(node,orderedVars);
				}
			}
			return freeVars;
		}
		
		/**
		 * Returns an empty set that can grow up to 
		 * the given size.  The returned set may or 
		 * may not throw an UnsupportedOperationException
		 * if required to accomodate more than the specified
		 * number of elements.
		 */
		@SuppressWarnings("unchecked")
		private final Set<Variable> setOfSize(int size) {
			return size == 0 ? Collections.EMPTY_SET : 
				   new HashSet<Variable>((size * 4) / 3 + 1);
		}
		
		@SuppressWarnings("unchecked")
		public Set<Variable> visit(Decls decls) {
			Set<Variable> vars = cachedVars(decls);
			if (vars != null) return vars;
			
			vars = setOfSize(decls.size());
			for(Decl decl : decls) {
				vars.addAll(visit(decl));
			}
			
			return cache(decls, vars.isEmpty() ? Collections.EMPTY_SET : vars);
		}

		public Set<Variable> visit(Decl decl) {
			final Set<Variable> vars = cachedVars(decl);
			return vars != null ? vars : 
				   cache(decl, decl.expression().accept(this));
		}

		public Set<Variable> visit(Relation relation) {
			return Collections.emptySet();
		}

		public Set<Variable> visit(Variable variable) {
			return Collections.singleton(variable);
		}

		public Set<Variable> visit(ConstantExpression constExpr) {
			return Collections.emptySet();
		}
		
		public Set<Variable> visit(BinaryExpression binExpr) {
			Set<Variable> vars = cachedVars(binExpr);
			if (vars != null) return vars;
			
			final Set<Variable> left = binExpr.left().accept(this);
			final Set<Variable> right = binExpr.right().accept(this);
			vars = setOfSize(left.size() + right.size());
			vars.addAll(left);
			vars.addAll(right);
			return cache(binExpr, vars);
		}

		public Set<Variable> visit(UnaryExpression unaryExpr) {
			final Set<Variable> vars = cachedVars(unaryExpr);
			return vars != null ? vars : 
				   cache(unaryExpr, unaryExpr.expression().accept(this));
		}
		
		/**
		 * Visits the given comprehension or quantified formula.  The method returns a set that contains all 
		 * the free variables in the declarations and the formula, minus the variables that are 
		 * actually bound in the declarations.  
		 */
		@SuppressWarnings("unchecked")
		private Set<Variable> visit(Node creator, Decls decls, Formula formula) {
			Set<Variable> vars = cachedVars(creator);
			if (vars!=null) return vars;
			
			// collect the free variables in the declaration expressions;
			// note that we don't need to add variables to the in-scope stack
			// as they are being collected, as we assume that there are no 
			// variable dependencies among variables declared within the same 
			// Decls object.
			final Set<Variable> declVars = decls.accept(this);
			
			// add the declared variables to the scoped variables stack 
			varsInScope.ensureCapacity(varsInScope.size() + decls.size());
			for(Decl decl : decls) {
				varsInScope.push(decl.variable());
			}
			
			// collect the free variables in the formula
			final Set<Variable> formulaVars = formula.accept(this);
			
			// add the free variables in the decls and the formula to the creator's free vars set
			vars = setOfSize(declVars.size() + formulaVars.size());
			vars.addAll(declVars);
			vars.addAll(formulaVars);
			
			// remove the variables that are actually declared by this creator
			// from the creator's free variable set, as well as from the in-scope stack
			for(int i = decls.size(); i > 0; i--) {
				vars.remove(varsInScope.pop());
			}
			
			return cache(creator, vars.isEmpty() ? Collections.EMPTY_SET : vars);
		}
		
		public Set<Variable> visit(Comprehension comprehension) {
			return visit(comprehension, comprehension.declarations(), comprehension.formula());
		}

		public Set<Variable> visit(IfExpression ifExpr) {
			Set<Variable> vars = cachedVars(ifExpr);
			if (vars != null) return vars;
			
			final Set<Variable> condition = ifExpr.condition().accept(this);
			final Set<Variable> thenExpr = ifExpr.thenExpr().accept(this);
			final Set<Variable> elseExpr = ifExpr.elseExpr().accept(this);
			vars = setOfSize(condition.size() + thenExpr.size() + elseExpr.size());
			vars.addAll(condition);
			vars.addAll(thenExpr);
			vars.addAll(elseExpr);
			return cache(ifExpr, vars);
		}
		
		public Set<Variable> visit(QuantifiedFormula quantFormula) {
			return visit(quantFormula, quantFormula.declarations(), quantFormula.formula());
		}

		public Set<Variable> visit(BinaryFormula binFormula) {
			Set<Variable> vars = cachedVars(binFormula);
			if (vars != null) return vars;
			
			final Set<Variable> left = binFormula.left().accept(this);
			final Set<Variable> right = binFormula.right().accept(this);
			vars = setOfSize(left.size() + right.size());
			vars.addAll(left);
			vars.addAll(right);
			return cache(binFormula, vars);
		}

		public Set<Variable> visit(NotFormula not) {
			final Set<Variable> vars = cachedVars(not);
			return vars != null ? vars :  
				   cache(not, not.formula().accept(this));
		}

		public Set<Variable> visit(ConstantFormula constant) {
			return Collections.emptySet();
		}

		public Set<Variable> visit(ComparisonFormula compFormula) {
			Set<Variable> vars = cachedVars(compFormula);
			if (vars != null) return vars;
			
			final Set<Variable> left = compFormula.left().accept(this);
			final Set<Variable> right = compFormula.right().accept(this);
			vars = setOfSize(left.size() + right.size());
			vars.addAll(left);
			vars.addAll(right);
			return cache(compFormula, vars);
		}

		public Set<Variable> visit(MultiplicityFormula multFormula) {
			final Set<Variable> vars = cachedVars(multFormula);
			return vars != null ? vars : 
				   cache(multFormula, multFormula.expression().accept(this));
		}
		
		public Set<Variable> visit(RelationPredicate pred) {
			if (pred.name() != RelationPredicate.Name.FUNCTION) 
				return Collections.emptySet();
			Set<Variable> vars = cachedVars(pred);
			if (vars != null) return vars;
			
			final RelationPredicate.Function fp = (RelationPredicate.Function) pred;
			final Set<Variable> domain = fp.domain().accept(this);
			final Set<Variable> range = fp.range().accept(this);
			vars = setOfSize(domain.size() + range.size());
			vars.addAll(domain);
			vars.addAll(range);
			return cache(pred, vars);
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
