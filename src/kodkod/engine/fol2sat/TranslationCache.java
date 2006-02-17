package kodkod.engine.fol2sat;

import java.util.Collections;
import java.util.HashSet;
import java.util.IdentityHashMap;
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
import kodkod.engine.bool.BooleanConstant;
import kodkod.engine.bool.BooleanMatrix;
import kodkod.util.ArrayStack;


/**
 * This class manages translation caching during
 * the FOL to SAT phase.  It determines which translations
 * to cache, when to throw them out of the cache, etc. 
 * 
 * @specfield node: Node // node being translated
 * @specfield cached: node.*children - LeafExpression // the nodes whose translations we'll cache
 * @specfield cache: cached -> (BooleanMatrix + BooleanValue + List<BooleanMatrix>) -> Environment
 * @invariant all n: cached | 
 *             n in Expression + Decl => cache[n] in BooleanMatrix -> Environment,
 *             n in Decls => cache[n] in List<BooleanMatrix> -> Environment,
 *             n in Formula => cache[n] in BooleanValue -> Environment
 * @invariant all e: Environment | some cache.e => 
 *             let n = cache.e.Object | 
 *               e.map.BooleanMatrix = Variable & n.*children - (n.*children & Decls).variable 
 * @author Emina Torlak
 */
final class TranslationCache {
	private final Map<Node,TranslationInfo> cache;
	
	/**
	 * Constructs a new translation cache for the given node,
	 * using the provided structural information.
	 * @requires sharedInternalNodes = {n: Node | #(n.~children & node.*children) > 1 }
	 * @effects this.node' = node 
	 */
	@SuppressWarnings("unchecked") TranslationCache(Node node, Set<Node> sharedInternalNodes) {
		final Collector collector = new Collector(sharedInternalNodes);
		node.accept(collector);
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
	<T> T get(Node node, Environment<BooleanMatrix> env) {
		final TranslationInfo info = cache.get(node);
		return info==null ? null : (T) info.get(env);
	}
	
	/**
	 * Caches the given translation for the specified node, if the node is
	 * one for which caching is performed.  Otherwise does nothing.  
	 * The method returns the specified translation. 
	 * @requires freeVariables(node) in env.map.BooleanMatrix && 
	 *           (node in Expression + Decl => translation in BooleanMatrix,
	 *            node in Decls => translation in List<BooleanMatrix>,
	 *            node in Formula => translation in BooleanValue) 
	 * @effects node in this.cached => 
	 *           this.cache' = this.cache ++ 
	 *            node->translation->{e: Environment | e.map = freeVariables(node)<:env.map }, 
	 *           this.cache' = this.cache
	 * @return translation
	 */
	<T> T cache(Node node, T translation, Environment<BooleanMatrix> env) {
		final TranslationInfo info = cache.get(node);
		if (info != null) {
			info.set(translation, env);
		}
		return translation;
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
	 * The helper visitor that examines the free variables of semantically shared nodes
	 * to determine which ones should be cached.  A node is considered 'semantically shared'
	 * if it is syntactically shared or if it is a descendent of a quantifed formula
	 * or comprehension. 
	 * @specfield: node: Node // node for which we are collecting caching information
	 * @specfield freeVarMap: Node -> lone Set<Variable>
	 */
	private static final class Collector implements ReturnVisitor<Set<Variable>,Set<Variable>,Set<Variable>> {
		/* Holds the variables that are currently in scope, with the
		 * variable at the top of the stack being the last declared variable. */
		private final ArrayStack<Variable> varsInScope;
		private final Set<Node> sharedNodes;
		
		private final Map<Node, Set<Variable>> freeVarMap;
				
		/**
		 * Constructs a new collector using the given structural information.
		 * The given set is required to contain the syntactically shared subtrees of the
		 * node for which we are computing caching information.
		 */
		Collector(Set<Node> sharedNodes) {
			this.freeVarMap = new IdentityHashMap<Node,Set<Variable>>(sharedNodes.size());
			this.sharedNodes = sharedNodes;
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
			return freeVarMap;
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
		 * @effects node in sharedNodes || 
		 *          ((node.^(~children) in (QuantifiedFormula + Comprehension)) &&
		 *           (some varsInScope.top() => !freeVars.contains(varsInScope.top()))) => 
		 *            freeVarMap' = freeVarMap ++ node->varsInScope,
		 *            freeVarMap' = freeVarMap
		 * @return freeVars
		 */
		private final Set<Variable> cache(Node node, Set<Variable> freeVars) {
			if (/*varsInScope.empty() && */sharedNodes.contains(node) || 
				!varsInScope.empty() && !freeVars.contains(varsInScope.peek())) {
				
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
		
		/**
		 * Returns free variables in the given declarations.
		 * @return freeVars(decls.declarations)
		 */
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

		/**
		 * Returns the free variables in the given declaration.
		 * @return freeVars(decl.expression)
		 */
		public Set<Variable> visit(Decl decl) {
			final Set<Variable> vars = cachedVars(decl);
			return vars != null ? vars : 
				   cache(decl, decl.expression().accept(this));
		}

		/**
		 * Returns the empty set.
		 * @return {}
		 */
		@SuppressWarnings("unchecked")
		public Set<Variable> visit(Relation relation) {
			return Collections.EMPTY_SET;
		}

		/**
		 * Returns the singleton set containing the given variable.
		 * @return {variable}
		 */
		public Set<Variable> visit(Variable variable) {
			return Collections.singleton(variable);
		}

		/**
		 * Returns the empty set.
		 * @return {}
		 */
		public Set<Variable> visit(ConstantExpression constExpr) {
			return Collections.emptySet();
		}
		
		/**
		 * Returns the free variables in the given binary expression.
		 * @return freeVars(binExpr.left) + freeVars(bin.right)
		 */
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

		/**
		 * Returns the free variables in the given unary expression.
		 * @return freeVars(unaryExpr.expression)
		 */
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
		
		/**
		 * Returns the free variables in the given comprehension.
		 * @return freeVars(comprehension.declarations) + 
		 *          freeVars(comprehension.body) - comprehension.declarations.declarations[int].Expression 
		 */
		public Set<Variable> visit(Comprehension comprehension) {
			return visit(comprehension, comprehension.declarations(), comprehension.formula());
		}

		/**
		 * Returns the free variables in the given if expression.
		 * @return freeVars(ifExpr.condition) + freeVars(ifExpr.thenExpr) + freeVars(ifExpr.elseExpr)
		 */
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
		
		/**
		 * Returns the free variables in the given quantified formula.
		 * @return freeVars(quantFormula.declarations) + 
		 *          freeVars(quantFormula.body) - quantFormula.declarations.declarations[int].Expression 
		 */
		public Set<Variable> visit(QuantifiedFormula quantFormula) {
			return visit(quantFormula, quantFormula.declarations(), quantFormula.formula());
		}

		/**
		 * Returns the free variables in the given binary formula.
		 * @return freeVars(binFormula.left) + freeVars(binFormula.right) 
		 */
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

		/**
		 * Returns the free variables in the given negation formula.
		 * @return freeVars(not.formula)
		 */
		public Set<Variable> visit(NotFormula not) {
			final Set<Variable> vars = cachedVars(not);
			return vars != null ? vars :  
				   cache(not, not.formula().accept(this));
		}

		/**
		 * Returns the empty set.
		 * @return {}
		 */
		public Set<Variable> visit(ConstantFormula constant) {
			return Collections.emptySet();
		}

		/**
		 * Returns the free variables in the given comparison formula.
		 * @return freeVars(compFormula.left) + freeVars(compFormula.right) 
		 */
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

		/**
		 * Returns the free variables in the given multiplicity formula.
		 * @return freeVars(multFormula.expression)
		 */
		public Set<Variable> visit(MultiplicityFormula multFormula) {
			final Set<Variable> vars = cachedVars(multFormula);
			return vars != null ? vars : 
				   cache(multFormula, multFormula.expression().accept(this));
		}
		
		/**
		 * Returns the free variables in the given predicate.
		 * @return pred.name = FUNCTION => freeVars(domain) + freeVars(range), {}
		 */
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
}
