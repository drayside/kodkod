package kodkod.engine.fol2sat;

import static kodkod.engine.bool.BooleanConstant.TRUE;

import java.util.Collections;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import kodkod.ast.BinaryExpression;
import kodkod.ast.BinaryFormula;
import kodkod.ast.BinaryIntExpression;
import kodkod.ast.ExprIntCast;
import kodkod.ast.ComparisonFormula;
import kodkod.ast.Comprehension;
import kodkod.ast.ConstantExpression;
import kodkod.ast.ConstantFormula;
import kodkod.ast.Decl;
import kodkod.ast.Decls;
import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.IfExpression;
import kodkod.ast.IntExprCast;
import kodkod.ast.IntComparisonFormula;
import kodkod.ast.IntConstant;
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
import kodkod.engine.bool.BooleanFormula;
import kodkod.engine.bool.BooleanMatrix;
import kodkod.engine.bool.BooleanValue;
import kodkod.util.collections.ArrayStack;
import kodkod.util.collections.IdentityHashSet;
import kodkod.util.ints.IndexedEntry;
import kodkod.util.ints.IntSet;
import kodkod.util.ints.Ints;


/**
 * This class manages translation caching during
 * the FOL to SAT phase.  It determines which translations
 * to cache, when to throw them out of the cache, etc. 
 * 
 * @specfield node: Node // node being translated
 * @specfield cached: node.*children  // the nodes whose translations we'll cache
 * @specfield cache: cached -> (BooleanMatrix + BooleanValue + List<BooleanMatrix> + Int) -> Environment
 * @invariant all n: cached | 
 *             n in Expression + Decl => cache[n] in BooleanMatrix -> Environment,
 *             n in Decls => cache[n] in List<BooleanMatrix> -> Environment,
 *             n in Formula => cache[n] in BooleanValue -> Environment,
 *             n in IntExpression => cache[n] in Int -> Environment
 * @invariant all e: Environment | some cache.e => 
 *             let n = cache.e.Object | 
 *               e.map.BooleanMatrix = Variable & n.*children - (n.*children & Decls).variable 
 * @author Emina Torlak
 */
abstract class TranslationCache {
	private final Map<Node,TranslationInfo> cache;
	
	/**
	 * Constructs a new translation cache for the given annotated node.
	 * @effects this.node' = annotated.node 
	 */
	@SuppressWarnings("unchecked") 
	private TranslationCache(AnnotatedNode<? extends Node> annotated) {
		final Collector collector = new Collector(annotated.sharedNodes());
		annotated.node().accept(collector);
		for(Map.Entry<Node, Object> e :  ((Map<Node, Object>)((Map)collector.cachingInfo())).entrySet()) {
			Set<Variable> freeVars = (Set<Variable>)e.getValue();
			if (freeVars.isEmpty())
				e.setValue(new NoVarTranslationInfo());
			else 
				e.setValue(new MultiVarTranslationInfo(freeVars));
		}
		this.cache = (Map<Node, TranslationInfo>)((Map) collector.cachingInfo());
	}

	/**
	 * If the translation of the given node, with its free variables
	 * bound as they are in the given environment, has been cached, 
	 * the cached value is returned.  Otherwise, null is returned.
	 * @return this.cache[node][Object] in env.map =>
	 *         this.cache[node].map, null
	 */
	@SuppressWarnings("unchecked")
	final <T> T get(Node node, Environment<BooleanMatrix> env) {
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
	 *            node in Formula => translation in BooleanValue, 
	 *            node in IntExpression => translation in Int) 
	 * @effects node in this.cached => 
	 *           this.cache' = this.cache ++ 
	 *            node->translation->{e: Environment | e.map = freeVariables(node)<:env.map }, 
	 *           this.cache' = this.cache
	 * @return translation
	 */
	final <T> T cache(Node node, T translation, Environment<BooleanMatrix> env) {
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
	 * @effects expr in this.cached => 
	 *           this.cache' = this.cache ++ 
	 *            expr->translation->{e: Environment | e.map = freeVariables(expr)<:env.map }, 
	 *           this.cache' = this.cache
	 * @return translation
	 */
	abstract BooleanMatrix cache(Expression expr, BooleanMatrix translation, Environment<BooleanMatrix> env);
	
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
	abstract BooleanValue cache(Formula formula, BooleanValue translation, Environment<BooleanMatrix> env);
	
	
	/**
	 * A TranslationCache that tracks variables in addition to 
	 * caching translations.
	 * 
	 * @invariant node in Formula
	 * @specfield varUsage: node.*children & (Expression + Formula) -> set int
	 * @specfield trueFormulas: set node.*children & Formula
	 * @specfield falseFormulas: set node.*children & Formula
	 */
	static final class Tracking extends TranslationCache {
		private final Map<Node, IntSet> varUsage;
		private final Set<Formula> trueFormulas, falseFormulas;
		
		/**
		 * Constructs a new tracking translation cache for the given annotated node.
		 * @effects this.node' = annotated.node 
		 */
		Tracking(AnnotatedNode<Formula> annotated) {
			super(annotated);
			this.varUsage = new IdentityHashMap<Node,IntSet>();
			this.trueFormulas = new IdentityHashSet<Formula>();
			this.falseFormulas = new IdentityHashSet<Formula>();
		}

		/**
		 * Returns this.varUsage
		 * @return this.varUsage
		 */
		Map<Node, IntSet> varUsage() {
			return varUsage;
		}
		
		/**
		 * Return this.trueFormulas
		 * @return this.trueFormulas
		 */
		Set<Formula> trueFormulas() {
			return trueFormulas;
		}
		
		/**
		 * Return this.falseFormulas
		 * @return this.falseFormulas
		 */
		Set<Formula> falseFormulas() {
			return falseFormulas;
		}
		
		/**
		 * If the given expression is one for which we are caching translations,
		 * the provided translation is cached and returned.  Otherwise,
		 * the translation is simply returned.  In addition, this method records
		 * the labels of BooleanFormulas that comprise the dense regions of 
		 * the translation in the varUsage map. 
		 * @return translation
		 * @effects if the expression is one for which we are caching translations,
		 * the provided translation is cached.
		 * @effects this.varUsage' = this.varUsage + 
		 *           expr -> {i: int | some b: translation.elements[int] - BooleanConstant | 
		 *                     i = |b.label| }
		 */
		@Override
		BooleanMatrix cache(Expression expr, BooleanMatrix translation, Environment<BooleanMatrix> env) {
			IntSet vars;
			if (env.parent()==null) { // top-level expression
				vars = Ints.bestSet(1, Integer.MAX_VALUE-1);		
			} else { // not a top-level expression
				vars = varUsage.get(expr);
				if (vars==null)
					vars = Ints.bestSet(1, Integer.MAX_VALUE-1);
			}
			for(IndexedEntry<BooleanValue> e: translation) {
				if (e.value() != TRUE)
					vars.add(StrictMath.abs(((BooleanFormula)e.value()).label()));
			}
			varUsage.put(expr, vars);
			return cache((Node)expr, translation, env);
		}

		/**
		 * If the given formula is one for which we are caching translations,
		 * the provided translation is cached and returned.  Otherwise,
		 * the translation is simply returned. In addition, this method records
		 * the label of the translation in the varUsage map, if the translation
		 * is non-constant.  If it is constant, it records the formula as being
		 * constant.  
		 * @return translation
		 * @effects if the formula is one for which we are caching translations,
		 * the provided translation is cached.
		 * @effects translation = BooleanConstant.TRUE => 
		 * 	          this.trueFormulas' = this.trueFormulas + formula,
		 *          translation = BooleanConstant.FALSE => 
		 * 	          this.falseFormulas' = this.falseFormulas + formula,
		 *          this.varUsage' = this.varUsage + formula -> |translation.label|
		 */
		@Override
		BooleanValue cache(Formula formula, BooleanValue translation, Environment<BooleanMatrix> env) {
			if (translation==BooleanConstant.TRUE) {
				if (env.parent()==null) trueFormulas.add(formula);
			} else if (translation==BooleanConstant.FALSE) {
				if (env.parent()==null) falseFormulas.add(formula);
			} else if (env.parent()==null) { // top-level formula
				varUsage.put(formula, Ints.singleton(StrictMath.abs(((BooleanFormula)translation).label())));	
			} else {
				IntSet vars = varUsage.get(formula);
				if (vars==null)
					vars = Ints.bestSet(1, Integer.MAX_VALUE-1);
				vars.add(StrictMath.abs(((BooleanFormula)translation).label()));
				varUsage.put(formula, vars);
			}
			return cache((Node)formula, translation, env);
		}
		
	}
	
	/**
	 * A simple implementation of a translation cache.
	 */
	static final class Simple extends TranslationCache {
		/**
		 * Constructs a new simple translation cache for the given annotated node.
		 * @effects this.node' = annotated.node 
		 */
		Simple(AnnotatedNode<? extends Node> annotated) {
			super(annotated);
		}
		
		/**
		 * {@inheritDoc}
		 * @see kodkod.engine.fol2sat.TranslationCache#cache(kodkod.ast.Expression, kodkod.engine.bool.BooleanMatrix, kodkod.engine.fol2sat.Environment)
		 */
		@Override
		BooleanMatrix cache(Expression expr, BooleanMatrix translation, Environment<BooleanMatrix> env) {
			return cache((Node)expr, translation, env);
		}
		
		/**
		 * {@inheritDoc}
		 * @see kodkod.engine.fol2sat.TranslationCache#cache(kodkod.ast.Formula, kodkod.engine.bool.BooleanValue, kodkod.engine.fol2sat.Environment)
		 */
		@Override
		BooleanValue cache(Formula formula, BooleanValue translation, Environment<BooleanMatrix> env) {
			return cache((Node)formula, translation, env);
		}
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
	private static final class Collector implements ReturnVisitor<Set<Variable>,Set<Variable>,Set<Variable>,Set<Variable>> {
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
		private final Set<Variable> lookup(Node node) {
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
			if (sharedNodes.contains(node) || 
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
			Set<Variable> vars = lookup(decls);
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
			final Set<Variable> vars = lookup(decl);
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
			Set<Variable> vars = lookup(binExpr);
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
			final Set<Variable> vars = lookup(unaryExpr);
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
			Set<Variable> vars = lookup(creator);
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
			Set<Variable> vars = lookup(ifExpr);
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
			Set<Variable> vars = lookup(binFormula);
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
			final Set<Variable> vars = lookup(not);
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
			Set<Variable> vars = lookup(compFormula);
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
			final Set<Variable> vars = lookup(multFormula);
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
			Set<Variable> vars = lookup(pred);
			if (vars != null) return vars;
			
			final RelationPredicate.Function fp = (RelationPredicate.Function) pred;
			final Set<Variable> domain = fp.domain().accept(this);
			final Set<Variable> range = fp.range().accept(this);
			vars = setOfSize(domain.size() + range.size());
			vars.addAll(domain);
			vars.addAll(range);
			return cache(pred, vars);
		}

		/**
		 * Returns the empty set.
		 * @return {}
		 */
		public Set<Variable> visit(IntConstant intConst) {
			return Collections.emptySet();
		}

		/**
		 * Returns the free variables in intExpr.expression.
		 * @return freeVars(intExpr.expression)
		 */
		public Set<Variable> visit(ExprIntCast intExpr) {
			Set<Variable> ret = lookup(intExpr);
			return ret != null ? ret : cache(intExpr, intExpr.expression().accept(this));
		}

		/**
		 * Returns the free variables of intComp, if any.
		 * @return freeVars(intComp.left) + freeVars(intComp.right)
		 */
		public Set<Variable> visit(IntComparisonFormula intComp) {
			Set<Variable> vars = lookup(intComp);
			if (vars != null) return vars;
			final Set<Variable> left = intComp.left().accept(this);
			final Set<Variable> right = intComp.right().accept(this);
			vars = setOfSize(left.size() + right.size());
			vars.addAll(left);
			vars.addAll(right);
			return cache(intComp, vars);
		}

		/**
		 * Returns the free variables of intExpr, if any.
		 * @return freeVars(intExpr.left) + freeVars(intExpr.right)
		 */
		public Set<Variable> visit(BinaryIntExpression intExpr) {
			Set<Variable> vars = lookup(intExpr);
			if (vars != null) return vars;
			final Set<Variable> left = intExpr.left().accept(this);
			final Set<Variable> right = intExpr.right().accept(this);
			vars = setOfSize(left.size() + right.size());
			vars.addAll(left);
			vars.addAll(right);
			return cache(intExpr, vars);
		}

		/**
		 * Returns the free variables of castExpr, if any.
		 * @return freeVars(castExpr.intExpr)
		 */
		public Set<Variable> visit(IntExprCast castExpr) {
			Set<Variable> vars = lookup(castExpr);
			return  (vars != null) ? vars : cache(castExpr, castExpr.intExpr().accept(this));
		}
	}
}
