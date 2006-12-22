package kodkod.engine.fol2sat;


import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import kodkod.ast.Comprehension;
import kodkod.ast.Decl;
import kodkod.ast.Decls;
import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.IntExpression;
import kodkod.ast.Node;
import kodkod.ast.QuantifiedFormula;
import kodkod.ast.SumExpression;
import kodkod.ast.Variable;
import kodkod.ast.visitor.DepthFirstCollector;
import kodkod.engine.bool.BooleanConstant;
import kodkod.engine.bool.BooleanMatrix;
import kodkod.engine.bool.BooleanValue;
import kodkod.engine.bool.Int;
import kodkod.util.collections.ArrayStack;
import kodkod.util.collections.Stack;


/**
 * Caches translations for particular nodes, computed during
 * the FOL to Bool translation phase.  It determines which translations
 * to cache, when to throw them out of the cache, etc. 
 * 
 * @specfield node: Node // node being translated
 * @specfield cached: node.*children  // the nodes whose translations we'll cache
 * @specfield cache: cached -> (BooleanMatrix + BooleanValue + List<BooleanMatrix> + Int) -> Environment
 * @invariant all n: cached | 
 *             n in Expression + Decl => cache[n] in BooleanMatrix -> Environment,
 *             n in Formula => cache[n] in BooleanValue -> Environment,
 *             n in IntExpression => cache[n] in Int -> Environment
 * @invariant all e: Environment | some cache.e => 
 *             let n = cache.e.Object | 
 *               e.map.BooleanMatrix = Variable & n.*children - (n.*children & Decls).variable 
 * @author Emina Torlak
 */
class TranslationCache {
	private final Map<Node,TranslationInfo> cache;
	
	/**
	 * Constructs a new translation cache for the given annotated node.
	 * @effects this.node' = annotated.node 
	 */
	@SuppressWarnings("unchecked") 
	TranslationCache(AnnotatedNode<? extends Node> annotated) {
		final VarCollector collector = new VarCollector(annotated.sharedNodes());
		annotated.node().accept(collector);
//		System.out.println(collector.cachingInfo());
		for(Map.Entry<Node, Object> e :  ((Map<Node, Object>)((Map)collector.cachingInfo())).entrySet()) {
			Set<Variable> freeVars = (Set<Variable>)e.getValue();
			if (freeVars.isEmpty())
				e.setValue(new NoVarTranslationInfo());
			else 
				e.setValue(new MultiVarTranslationInfo(freeVars));
		}
		this.cache = (Map<Node, TranslationInfo>)((Map) collector.cachingInfo());
//		System.out.println(cache);
	}

	/**
	 * If the translation of the given node, with its free variables
	 * bound as they are in the given environment, has been cached, 
	 * the cached value is returned.  Otherwise, null is returned.
	 * @return this.cache[node][Object] in env.map =>
	 *         this.cache[node].map, null
	 */
	@SuppressWarnings("unchecked")
	<T> T lookup(Node node, Environment<BooleanMatrix> env) {
		final TranslationInfo info = cache.get(node);
		return info==null ? null : (T) info.get(env);
	}
	
	/**
	 * Caches the given translation for the specified node, if the node is
	 * one for which caching is performed.  Otherwise does nothing.  
	 * The method returns the specified translation. 
	 * @requires freeVariables(node) in env.map.BooleanMatrix && 
	 *           (node in Expression + Decl => translation in BooleanMatrix,
	 *            node in Formula => translation in BooleanValue, 
	 *            node in IntExpression => translation in Int) 
	 * @effects node in this.cached => 
	 *           this.cache' = this.cache ++ 
	 *            node->translation->{e: Environment | e.map = freeVariables(node)<:env.map }, 
	 *           this.cache' = this.cache
	 * @return translation
	 */
	private final <T> T cache(Node node, T translation, Environment<BooleanMatrix> env) {
		final TranslationInfo info = cache.get(node);
		if (info != null) {
			info.set(translation, env);
		}
		return translation;
	}
	
	/**
	 * Caches the given translation for the specified node, if the node is
	 * one for which caching is performed.  Otherwise does nothing.  
	 * The method returns the specified translation. 
	 * @requires freeVariables(expr) in env.map.BooleanMatrix
	 * @effects decl in this.cached => 
	 *           this.cache' = this.cache ++ 
	 *            decl->translation->{e: Environment | e.map = freeVariables(expr)<:env.map }, 
	 *           this.cache' = this.cache
	 * @return translation
	 */
	BooleanMatrix cache(Decl decl, BooleanMatrix translation, Environment<BooleanMatrix> env) {
		return cache((Node)decl, translation, env);
	}
	
	/**
	 * Caches the given translation for the specified node, if the node is
	 * one for which caching is performed.  Otherwise does nothing.  
	 * The method returns the specified translation. 
	 * @requires freeVariables(expr) in env.map.BooleanMatrix
	 * @effects expr in this.cached => 
	 *           this.cache' = this.cache ++ 
	 *            expr->translation->{e: Environment | e.map = freeVariables(expr)<:env.map }, 
	 *           this.cache' = this.cache
	 * @return translation
	 */
	BooleanMatrix cache(Expression expr, BooleanMatrix translation, Environment<BooleanMatrix> env) {
		return cache((Node)expr, translation, env);
	}
	
	/**
	 * Caches the given translation for the specified node, if the node is
	 * one for which caching is performed.  Otherwise does nothing.  
	 * The method returns the specified translation. 
	 * @requires freeVariables(formula) in env.map.BooleanMatrix
	 * @effects formula in this.cached => 
	 *           this.cache' = this.cache ++ 
	 *            formula->translation->{e: Environment | e.map = freeVariables(formula)<:env.map }, 
	 *           this.cache' = this.cache
	 * @return translation
	 */
	BooleanValue cache(Formula formula, BooleanValue translation, Environment<BooleanMatrix> env) {
		return cache((Node)formula, translation, env);
	}
	
	/**
	 * Caches the given translation for the specified node, if the node is
	 * one for which caching is performed.  Otherwise does nothing.  
	 * The method returns the specified translation. 
	 * @requires freeVariables(expr) in env.map.BooleanMatrix
	 * @effects intexpr in this.cached => 
	 *           this.cache' = this.cache ++ 
	 *            intexpr->translation->{e: Environment | e.map = freeVariables(expr)<:env.map }, 
	 *           this.cache' = this.cache
	 * @return translation
	 */
	Int cache(IntExpression intexpr, Int translation, Environment<BooleanMatrix> env) {
		return cache((Node)intexpr, translation, env);
	}
	
	/**
	 * A container class that stores the translation of a shared node
	 * (BooleanValue for formulas and BooleanMatrix for expressions)
	 * and bindings for the node's free variables which were used to 
	 * generate the translation.
	 * Storing the bindings is necessary for proper handling of 
	 * sharing within quantified formulas and comprehensions.
	 * This implementation assumes that each free variable is 
	 * mapped to a BooleanMatrix of density one, whose sole entry
	 * is the BooleanConstant TRUE.
	 * @specfield varBinding: Variable -> lone int
	 * @specfield translation: lone Object
	 */
	private static abstract class TranslationInfo {
		Object translation;
		/**
		 * Returns this.translation if the given environment
		 * has the same mappings for the free variables of 
		 * the translated node as the ones used to generate
		 * this.translation.  Otherwise returns null.  
		 * @requires all v: varBinding.int | some e.lookup(v)
		 * @return all v: varBinding.int | e.lookup(v).get(varBinding[v])=TRUE => this.translation, null
		 * @throws NullPointerException - e = null
		 */
		abstract Object get(Environment<BooleanMatrix> e);
		
		/**
		 * Sets this.translation to the given translation
		 * and sets the free variable bindings to those 
		 * given by the specified environment.
		 * @requires all v: varBinding.int | some env.lookup(v)
		 * @effects this.translation' = translation && 
		 *          this.varBinding' = 
		 *           {v: this.varBinding.int, tupleIndex: int | 
		 *             tupleIndex = env.lookup(v).iterator().next().index() }
		 */
		abstract void set(Object transl, Environment<BooleanMatrix> env);
	}
	
	/**
	 * A TranslationInfo for a node with one or more free variables. 
	 */
	private static final class MultiVarTranslationInfo extends TranslationInfo {
		final Variable[] vars;
		final int[] tuples;
		
		/**
		 * Constructs a translation unit for a node which
		 * has the given set of free variables.
		 * @effects this.freeVariables' = vars &&
		 *          no this.translation' 
		 */
		MultiVarTranslationInfo(Set<Variable> freeVariables) {
			this.vars = freeVariables.toArray(new Variable[freeVariables.size()]);
			this.tuples = new int[freeVariables.size()];
		}
		
		@Override
		Object get(Environment<BooleanMatrix> e) {
			if (translation==null) return null;
			for(int i = 0; i < vars.length; i++) {
				if (e.lookup(vars[i]).get(tuples[i])!=BooleanConstant.TRUE)
					return null;
			}
			return translation;
		}
		
		@Override
		void set(Object transl, Environment<BooleanMatrix> env) {
			translation = transl;
			for(int i = 0; i < vars.length; i++) {
				tuples[i] = env.lookup(vars[i]).iterator().next().index();
			}
		}
		
		public String toString() {
			final StringBuilder b = new StringBuilder("{");
			b.append(String.valueOf(translation));
			for(int i = 0; i < vars.length; i++)
			{			
				b.append(" (");
				b.append(vars[i]);
				b.append(", ");
				b.append(tuples[i]);
				b.append(")");
			}
			b.append("}");
			return b.toString();
		}
	}
	
	/**
	 * A TranslationInfo for a node with no free variables. 
	 */
	private static final class NoVarTranslationInfo extends TranslationInfo {
		
		@Override
		Object get(Environment<BooleanMatrix> e) {
			return translation;
		}
		
		@Override
		void set(Object transl, Environment<BooleanMatrix> env) {
			translation = transl;
		}
		
		public String toString() {
			return "{" + translation+ "}";
		}
		
	}	
	
	/**
	 * Examines the free variables of semantically shared nodes
	 * to determine which ones should be cached.  A node is considered 'semantically shared'
	 * if it is syntactically shared or if it is a descendent of a quantifed formula
	 * or comprehension. 
	 * @specfield cached: set Node 
	 * @specfield cache: Node -> lone Set<Variable>
	 * @specfield varsInScope: Stack<Variable> // variables currently in scope
	 */
	private static final class VarCollector extends DepthFirstCollector<Variable> {
		/* Holds the variables that are currently in scope, with the
		 * variable at the top of the stack being the last declared variable. */
		protected final Stack<Variable> varsInScope;
		
		/**
		 * Constructs a new collector using the given structural information.
		 * The given set is required to contain the syntactically shared subtrees of the
		 * node for which we are computing caching information.
		 */
		protected VarCollector(Set<Node> cached) {
			super(cached);
			this.varsInScope = new ArrayStack<Variable>();
		}
		/**
		 * Returns the caching information collected by this collector.
		 * This method should be called *after* this collector has been
		 * applied to this.node.
		 * @return a mapping from semantically shared subtrees of this.nodes to
		 * their free variables.
		 */
		final Map<Node, Set<Variable>> cachingInfo() {
			return cache;
		}

		/**
		 * @see kodkod.ast.visitor.DepthFirstCollector#newSet()
		 */
		@Override
		protected Set<Variable> newSet() {
			return new LinkedHashSet<Variable>(2);
		}
		
		/**
		 * If the given node is one for which we are collecting
		 * free variable information, we add a binding from it
		 * to a set with the same contents as the specified set, 
		 * but whose iterator returns the variables in the reverse
		 * order of declaration.  Otherwise,
		 * freeVarMap is not changed.  The specified set is returned.
		 * @effects node in sharedNodes || 
		 *          ((node.^(~children) in (QuantifiedFormula + Comprehension)) &&
		 *           (some varsInScope.top() => !freeVars.contains(varsInScope.top()))) => 
		 *            this.cache' = this.cache ++ node->varsInScope,
		 *            this.cache' = this.cache
		 * @return freeVars
		 */
		@SuppressWarnings("unchecked")
		@Override
		protected final Set<Variable> cache(Node node, Set<Variable> freeVars) {
			if (cached.contains(node) || !varsInScope.empty() && !freeVars.contains(varsInScope.peek())) {
				cache.put(node, reduce(freeVars));
			}
			return freeVars;
		}
		
		/**
		 * Visits the given comprehension, quantified formula, or sum expression.  
		 * The method returns a set that contains all 
		 * the free variables in the declarations and the body, minus the variables that are 
		 * actually bound in the declarations.  
		 */
		@SuppressWarnings("unchecked")
		private Set<Variable> visit(Node creator, Decls decls, Node body) {
			Set<Variable> ret = lookup(creator);
			if (ret!=null) return ret;
			
			ret = newSet();
			final Set<Variable> boundVars = newSet();
			
			// add the declared variables to the scoped variables stack;
			// compute free vars for each decl, and the difference of the
			// computed set and previously bound variables to ret
			for(Decl decl : decls) {
				for(Variable v : visit(decl)) {
					if (!boundVars.contains(v))
						ret.add(v);
				}
				varsInScope.push(decl.variable());
				boundVars.add(decl.variable());
			}

			// add to ret the free variables in the body, minus the bound variables
			for(Variable v: (Set<Variable>) body.accept(this)) {
				if (!boundVars.contains(v))
					ret.add(v);
			}
			
			// remove the declared variables from the in-scope stack
			for(int i = decls.size(); i > 0; i--) {
				varsInScope.pop();
			}
			
			return cache(creator, ret);
		}
		
		/**
		 * Returns the free variables in the given declaration.
		 * @return freeVars(decl.expression)
		 */
		public Set<Variable> visit(Decl decl) {
			final Set<Variable> ret = lookup(decl);
			return ret != null ? ret : cache(decl, decl.expression().accept(this));
		}
		
		/**
		 * Returns the singleton set containing the given variable.
		 * @return {variable}
		 */
		@Override
		public Set<Variable> visit(Variable variable) {
			return Collections.singleton(variable);
		}
		
		/** 
		 * Calls lookup(comprehension) and returns the cached value, if any.  
		 * If no cached value exists, computes, caches and returns the set
		 * of free variables in comprehension.
		 * @return let x = lookup(comprehension), d = comprehension.declarations, f = comprehension.formula | 
		 *          x != null => x,  
		 *          cache(comprehension, 
		 *            (f.accept(this) - d.children.variable) + 
		 *            {v: Variable | some i: [0..d.size) | 
		 *             v in d.declarations[i].accept(this) - d.declarations[0..i).variable } ) 
		 */
		@Override
		public Set<Variable> visit(Comprehension comprehension) {
			return visit(comprehension, comprehension.declarations(), comprehension.formula());
		}
		
		/** 
		 * Calls lookup(intExpr) and returns the cached value, if any.  
		 * If no cached value exists, computes, caches and returns the set
		 * of free variables in intExpr.  
		 * @return let x = lookup(intExpr), d = intExpr.declarations, e = intExpr.intExpr | 
		 *          x != null => x,  
		 *          cache(intExpr, 
		 *            (e.accept(this) - d.children.variable) + 
		 *            {v: Variable | some i: [0..d.size) | 
		 *             v in d.declarations[i].accept(this) - d.declarations[0..i).variable } ) 
		 */
		@Override
		public Set<Variable> visit(SumExpression intExpr) {
			return visit(intExpr, intExpr.declarations(), intExpr.intExpr());
		}
		
		/** 
		 * Calls lookup(quantFormula) and returns the cached value, if any.  
		 * If no cached value exists, computes, caches and returns the set
		 * of free variables in quantFormula.  
		 * @return let x = lookup(quantFormula), d = quantFormula.declarations, f = quantFormula.formula | 
		 *          x != null => x,  
		 *          cache(quantFormula, 
		 *            (f.accept(this) - d.children.variable) + 
		 *            {v: Variable | some i: [0..d.size) | 
		 *             v in d.declarations[i].accept(this) - d.declarations[0..i).variable } ) 
		 */
		@Override
		public Set<Variable> visit(QuantifiedFormula quantFormula) {
			return visit(quantFormula, quantFormula.declarations(), quantFormula.formula());
		}
	}
}
