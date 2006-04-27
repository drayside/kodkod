package kodkod.engine.bool;

import static kodkod.engine.bool.BooleanConstant.FALSE;
import static kodkod.engine.bool.BooleanConstant.TRUE;
import static kodkod.engine.bool.Operator.AND;
import static kodkod.engine.bool.Operator.OR;
import static kodkod.engine.bool.Operator.ITE;
import static kodkod.engine.bool.Operator.NOT;
import static kodkod.engine.bool.Operator.VAR;
import static kodkod.engine.bool.Operator.CONST;

import java.util.Iterator;
import java.util.Set;

import kodkod.engine.bool.Operator.Nary;
import kodkod.util.CacheSet;
import kodkod.util.IdentityHashSet;


/**
 * A factory for creating and assembling {@link kodkod.engine.bool.BooleanValue boolean values}.
 * Non-constant are not status among factories.
 * 
 * @specfield components: set BooleanValue
 * @invariant no f1, f2: BooleanFactory | f1 != f2 => f1.components & f2.components = BooleanConstant
 * @author Emina Torlak
 */
public final class BooleanFactory {
	/**
	 * Set used for gate comparisons.  Its capacity is 2^(depth), where
	 * depth is the depth to which gates should be checked for equality.
	 */
	private final Set<BooleanFormula> flat0, flat1;
	
	/**
	 * Stores input variables.
	 * @invariant all i: [1..iLits.size()] | vars[i-1].positive.label = i
	 */
	private final BooleanVariable[] vars;
	
	/**
	 * Caches the AND, OR, and ITE gates.  
	 * @invariant all i: [0..2] | c[i].op.ordinal = i
	 */
	private final CacheSet<BooleanFormula>[] cache;
	
	private int k;
	private int nextLiteral;
	
	/**
	 * Constructs a circuit factory with the given number of input variables.  Gates are
	 * checked for semantic equality down to the given depth.
	 * @requires 0 <= numInputVariables < Integer.MAX_VALUE 
	 * @requires checkToDepth >= 0
	 * @effects #this.components' = numInputVariables && this.components' in BooleanVariable &&
	 *          (all i: [1..numInputVariables] | one this.components'.label & i }
	 */
	@SuppressWarnings("unchecked")
	private BooleanFactory(int numInputVariables, int checkToDepth) {
		k = 1<<checkToDepth;
		vars = new BooleanVariable[numInputVariables];
		for(int i = 0; i < numInputVariables; i++) {
			vars[i]= new BooleanVariable(i+1);                                                                        
		}
		nextLiteral = numInputVariables + 1;
		flat0 = new IdentityHashSet<BooleanFormula>(k);
		flat1 = new IdentityHashSet<BooleanFormula>(k);
		cache = new CacheSet[3];
		cache[0] = new CacheSet<BooleanFormula>();
		cache[1] = new CacheSet<BooleanFormula>();
		cache[2] = new CacheSet<BooleanFormula>();
	}
	
	/**
	 * Returns a circuit factory, initialized to contain the given number
	 * of input variables that can be used in circuit construction.  The
	 * integer representations of the initial variables are the labels
	 * [1..numInputVariables].  Gates are checked for semantic equality 
	 * down to depth 5, when composing them using BooleanFactory#compose
	 * method.  The effect of this method is the same as calling BooleanFactory.factory(numInputVariables, 5).
	 * @return {f: BooleanFactory | #f.components = numInputVariables && f.components in BooleanVariable &&
	 *                              (all v: f.components | v.generator = v) &&
	 *                              (all i: [1..numInputVariables] | one f.components.label & i }}
	 * @throws IllegalArgumentException - numInputVariables < 0 || numInputVariables > Integer.MAX_VALUE - 1
	 */
	public static BooleanFactory factory(int numInputVariables) {
		return factory(numInputVariables, 3);
	}
	
	/**
	 * Returns a circuit factory, initialized to contain the given number
	 * of input variables that can be used in circuit construction.  The
	 * integer representations of the initial variables are the labels
	 * [1..numInputVariables].  Gates are checked for semantic equality 
	 * down to the given depth when composing them using BooleanFactory#compose
	 * method.  In general,  setting the
	 * comparison depth to a high value will result in more 
	 * subcomponents being shared.  However, it will also slow down
	 * gate construction.  
	 * @return {f: BooleanFactory | #f.components = numInputVariables && f.components in BooleanVariable &&
	 *                              (all v: f.components | v.generator = v) &&
	 *                              (all i: [1..numInputVariables] | one f.components.label & i }}
	 * @throws IllegalArgumentException - numInputVariables < 0 || numInputVariables > Integer.MAX_VALUE - 1
	 * @throws IllegalArgumentException - compDepth < 1
	 */
	public static BooleanFactory factory(int numInputVariables, int compDepth) {
		if (numInputVariables < 0 || numInputVariables == Integer.MAX_VALUE) 
			throw new IllegalArgumentException("numInputVariables < 0 || numInputVariables > Integer.MAX_VALUE - 1");
		if (compDepth < 1) throw new IllegalArgumentException("checkToDepth < 1: " + compDepth);
		return new BooleanFactory(numInputVariables, compDepth);
	}
	
	/**
	 * Returns the depth (from the root) to which components are checked for 
	 * semantic equality during gate construction.
	 * @return maximum depth to which components are checked for equality
	 */
	public int comparisonDepth() { return k; }
	
	/**
	 * Sets the comparison depth to the given value.  Setting the
	 * comparison depth to a high value will result in more 
	 * subcomponents being shared.  However, it will also slow down
	 * gate construction.
	 * @effects sets the comparison depth to the given value
	 * @throws IllegalArgumentException - newDepth < 1
	 */
	public void setComparisonDepth(int newDepth) {
		if (newDepth < 1)
			throw new IllegalArgumentException("newDepth < 1: " + newDepth);
		k = 1<<newDepth;
	}
	
	/**
	 * Returns the largest label corresponding to a formula created by this factory.
	 * @return max((BooleanFormula & this.components).label)
	 */
	public int maxFormulaLabel() { return nextLiteral-1; }
	
	/**
	 * Returns the variable with the given label, if it has already been produced
	 * by this factory.  If not, null is returned.
	 * @return (this.components & BooleanVariable).label
	 */
	public BooleanVariable variable(int label) {
		return (label > 0 && label <= vars.length ? 
				vars[label - 1] : null);
	}
	
	/**
	 * Returns the largest label corresponding to a variable created by this factory.
	 * @return max((BooleanVariable & this.components).label)
	 */
	public int maxVariableLabel() { return vars.length; }
	
	/**
	 * Returns the negation of the given boolean value.
	 * @return {n: BooleanValue | n.label = -v.label && [[n]] = ![[v]] }
	 * @effects (components.v).components' = (components.v).components + n 
	 * @throws NullPointerException - v = null                             
	 */
	public BooleanValue not(BooleanValue v) {
		return v.negation();
	}
	
	/**
	 * Returns a boolean value that represents the conjunction of the input components.  
	 * The effect of this method is the same as calling this.compose(Operator.AND, v0, v1).
	 * @return {v: BooleanValue | [[v]] = [[v0]] AND [[v1]] }
	 * @effects this.components' = this.components + v 
	 * @throws NullPointerException - any of the arguments are null
	 * @throws IllegalArgumentException - v0 + v1 !in this.components
	 */
	public final BooleanValue and(BooleanValue v0, BooleanValue v1) {
		return compose(AND, v0, v1);
	}
	
	/**
	 * Returns a boolean value that represents the disjunction of the input components.  
	 * The effect of this method is the same as calling this.compose(Operator.OR, v0, v1).
	 * @return {v: BooleanValue | [[v]] = [[v0]] OR [[v1]] }
	 * @effects this.components' = this.components + v 
	 * @throws NullPointerException - any of the arguments are null
	 * @throws IllegalArgumentException - v0 + v1 !in this.components
	 */
	public final BooleanValue or(BooleanValue v0, BooleanValue v1) {
		return compose(OR, v0, v1);
	}
	
	/**
	 * Returns a boolean value that represents the formula v0 => v1.  The effect
	 * of this method is the same as calling this.compose(OR, this.not(v0), v1).
	 * @return { v: BooleanValue | [[v]] = [[v0]] => [[v1]] }
	 * @effects this.components' = this.components + v
	 * @throws NullPointerException - any of the arguments are null
	 * @throws IllegalArgumentException - v0 + v1 !in this.components
	 */
	public final BooleanValue implies(BooleanValue v0, BooleanValue v1) {
		return compose(OR, v0.negation(), v1);
	}
	
	/**
	 * Returns a boolean value that represents the formula v0 = v1.  The
	 * effect of this method is the same as calling 
	 * this.and(this.implies(v0, v1), this.implies(v1, v0)).
	 * @return { v: BooleanValue | [[v]] = [[v0]] iff [[v1]] }
	 * @effects this.components' = this.components + v
	 * @throws NullPointerException - any of the arguments are null
	 * @throws IllegalArgumentException - v0 + v1 !in this.components
	 */
	public final BooleanValue iff(BooleanValue v0, BooleanValue v1) {
		return compose(AND, implies(v0,v1), implies(v1,v0));
	}
	
	/**
	 * Returns a boolean value that represents the formula (if i then t else e). 
	 * @return { v: BooleanValue | [[v]] = if [[i]] then [[t]] else [[e]] }
	 * @effects this.components' = this.components + v
	 * @throws NullPointerException - any of the arguments are null
	 * @throws IllegalArgumentException - i + t + e !in this.components
	 */
	public final BooleanValue ite(BooleanValue i, BooleanValue t, BooleanValue e) {
		if (!contains(i) || !contains(t) || !contains(e)) {
			throw new IllegalArgumentException("i + t + e !in this.components");
		}
		return fastITE(i, t, e);
	}
	
	/**
	 * Returns a boolean value that represents the formula (if i then t else e) without
	 * checking if the specified values are in this.components
	 * @return { v: BooleanValue | [[v]] = if [[i]] then [[t]] else [[e]] }
	 * @effects this.components' = this.components + v
	 * @throws NullPointerException - any of the arguments are null
	 */
	BooleanValue fastITE(BooleanValue i, BooleanValue t, BooleanValue e) {
		if (i==TRUE || t==e) return t;
		else if (i==FALSE) return e;
		else if (t==TRUE || i==t) return fastCompose(OR, i, e);
		else if (t==FALSE || i.negation()==t) return fastCompose(AND, i.negation(), e);
		else if (e==TRUE || i.negation()==e) return fastCompose(OR, i.negation(), t);
		else if (e==FALSE || i==e) return fastCompose(AND, i, t);
		else {
			final BooleanFormula f0 = (BooleanFormula) i, f1 = (BooleanFormula) t, f2 = (BooleanFormula) e;
			final int digest = f0.hash(null) + f1.hash(null) + f2.hash(null);
			
			for(Iterator<BooleanFormula> gates = opCache(ITE).get(digest); gates.hasNext();) {
				BooleanFormula gate = gates.next();
				if (gate.op() == ITE) {
					if (gate.input(0)==i && gate.input(1)==t && gate.input(2)==e)
						return gate;
				}
				
			}
			final BooleanFormula ret = new ITEGate(nextLiteral++, f0, f1, f2);
			opCache(ITE).add(ret);
			return ret;
		}
	}
	
	/**
	 * Returns a boolean value that represents
	 * the composition of the inputs using the given operator.
	 * @return {v: BooleanValue | [[v]] = [[v0]] op [[v1]]  }
	 * @effects this.components' = this.components + v
	 * @throws NullPointerException - any of the arguments are null
	 * @throws IllegalArgumentException - v0 + v1 !in this.components
	 */
	public BooleanValue compose(Operator.Nary op, BooleanValue v0, BooleanValue v1) {
		if (!contains(v0) || !contains(v1))
			throw new IllegalArgumentException("v0 + v1 !in this.components");
		return fastCompose(op, v0, v1);
	}
	
	/**
	 * Converts the given accumulator into an immutable boolean value and adds it to this.components.
	 * This method requires that all of g's inputs are in this.components.  If g has no inputs,
	 * its operator's identity constant is returned.  If g has one input, that input is returned.
	 * Otherwise, an immutable value that is semantically equivalent to g is returned.
	 * @return no g.inputs => g.op.identity(), 
	 *         one g.inputs => g.inputs, 
	 *         {g' : BooleanValue - MutableMultiGate | [[g']] = [[g]] }
	 * @effects this.components' = this.components + g'
	 * @throws IllegalArgumentException - g.inputs !in this.components
	 */
	public BooleanValue adopt(BooleanAccumulator g) {
		for(Iterator<BooleanValue> inputs = g.iterator(); inputs.hasNext();) {
			if (!contains(inputs.next())) throw new IllegalArgumentException();
		}
		return fastAdopt(g);
	}
	
	/**
	 * Returns true if v is in this.components.
	 * @return v in this.components
	 * @throws NullPointerException - v = null
	 */
	public boolean contains(BooleanValue v) {
		if (v.op()==CONST) return true;
		if (v.label()==0) return false;
		if (v.label() < 0) v = v.negation();
		final int absLit = v.label();
		if (absLit <= vars.length) {
			return v == vars[absLit-1];
		} else {
			final BooleanFormula g = (BooleanFormula) v;
			for(Iterator<BooleanFormula> gates = opCache(g.op()).get(g.hashCode()); gates.hasNext(); ) {
			    	if (gates.next()==g) return true;
		    }
			return false;
		}
	}
	
	/**
	 * Makes the given accumulator into an immutable value and adds it to this.components.
	 * This method requires that all of g's inputs are in this.components.  If g has no inputs,
	 * its operator's identity constant is returned.  If g has one input, that input is returned.
	 * Otherwise, an immutable value that is semantically equivalent to g is returned.
	 * @requires g.inputs in this.components 
	 * @return no g.inputs => g.op.identity(), 
	 *         one g.inputs => g.inputs, 
	 *         {g' : BooleanValue - MutableMultiGate | [[g']] = [[g]] }
	 * @effects this.components' = this.components + g'
	 */
	@SuppressWarnings("unchecked") BooleanValue fastAdopt(BooleanAccumulator acc) {
		final int asize = acc.size();
		switch(asize) {
		case 0 : return acc.op.identity();
		case 1 : return acc.iterator().next();
		case 2 : 
			final Iterator<BooleanValue> inputs = acc.iterator();
			return fastCompose(acc.op, inputs.next(), inputs.next());
		default :
			final int hash = acc.op.hash((Iterator)acc.iterator());
			for(Iterator<BooleanFormula> gates = opCache(acc.op).get(hash); gates.hasNext(); ) {
				BooleanFormula g = gates.next();
				if (g.size()==asize && ((MultiGate.NAry) g).sameInputs(acc.iterator())) { 
					return g;
				}
			}
			final BooleanFormula ret = MultiGate.make(acc, nextLiteral++, hash);
			opCache(acc.op).add(ret);
			return ret;
		}
	}

	/**
	 * Given two operators, op0 and op1, returns an AndOrCreator,
	 * which contains the creator for expressions of the form v0 op v1 where 
	 * op in Operator.Nary and v0.op = op0 and v1.op = op1.
	 * @requires op0 <= op1
	 * @requires op0 != null && op1 != null
	 * @return CREATORS[4op0 + op1 - (op0(op0-1))/2]
	 */
	private static final AndOrFactory creator(Operator op0, Operator op1) { 
		return AOFACTORIES[(op0.ordinal << 2) + op1.ordinal - ( (op0.ordinal*(op0.ordinal-1) >> 1 ))];
	}
	
	/**
	 * Returns a boolean value that represents
	 * the composition of the inputs using the given operator, 
	 * without checking that v0 and v1 are in this components
	 * @return {v: BooleanValue | [[v]] = [[v0]] op [[v1]]  }
	 * @effects this.components' = this.components + v
	 * @throws NullPointerException - any of the arguments are null
	 */
	BooleanValue fastCompose(Operator.Nary op, BooleanValue v0, BooleanValue v1) {
		final BooleanValue l, h;
		if (v0.op().ordinal < v1.op().ordinal) {
			l = v0; h = v1;
		} else {
			l = v1; h = v0;
		}
		if (h.op()==CONST) 
			return h==op.identity() ? l : h;
		else 
			return creator(l.op(), h.op()).compose(op, (BooleanFormula)l, (BooleanFormula)h, this);
	}
	
	/**
	 * Removes all gates from this.components.
	 * @effects this.componets' = 
	 *    BooleanConstant + this.components - { f: BooleanFormula | |f.label| > this.maxVariableLabel() }
	 */
	public void clear() {
		cache[0].clear(); 
		cache[1].clear(); 
		cache[2].clear();
		this.nextLiteral = vars.length + 1;
	}
	
	/**
	 * Returns a BooleanMatrix with the given dimensions, zero, and this 
	 * as the factory for its non-zero components.  
	 * @throws NullPointerException - any of the arguments are null 
	 * @return { m: BooleanMatrix | no m.elements && m.factory = this && m.dimensions = dims && m.zero = zero }
	 */
	public BooleanMatrix matrix(Dimensions d, BooleanConstant c) {
		if (d == null || c == null) throw new NullPointerException();
		return new BooleanMatrix(d, c, this);
	}
	
	/**
	 * Returns the cache for gates with the given operator.
	 * @requires op in AND + OR + ITE
	 * @return cache[op.ordinal]
	 */
	private CacheSet<BooleanFormula> opCache(Operator op) {
		return cache[op.ordinal];
	}
	
	/**
	 * Returns a BooleanFormula f such that [[f]] = f0 op f1.  The method
	 * requires that the formulas f0 and f1 be already reduced with respect to op.
	 * A new formula is created and cached iff the circuit with the meaning
	 * [[f0]] op [[f1]] has not already been created.
	 * @requires f0 and f1 have already been reduced with respect to op; i.e.  
	 * f0 op f1 cannot be reduced to a constant or a simple circuit 
	 * by applying absorption, idempotence, etc. laws to f0 and f1.
	 * @return f : BooleanFormula | [[f]] = [[f0]] op [[f1]]
	 * @effects f !in this.components => this.components' = this.components + f,
	 * 	        this.compnents' = this.components
	 */
	private BooleanFormula cache(Operator.Nary op, BooleanFormula f0, BooleanFormula f1) {
		final BooleanFormula l, h;
		if (f0.label()<f1.label()) {
			l = f0; h = f1;
		} else {
			l = f1; h = f0;
		}
		final int hash = op.hash(l,h);
		if (l.op()==op || h.op()==op) {
			flat0.clear();
			l.flatten(op, flat0, k-1);
			h.flatten(op, flat0, k-flat0.size());
			for(Iterator<BooleanFormula> gates = opCache(op).get(hash); gates.hasNext(); ) {
				BooleanFormula gate = gates.next();
				if (gate.size()==2 && gate.input(0)==l && gate.input(1)==h)
					return gate;
				else {
					flat1.clear();
					gate.flatten(op, flat1, k);
					if (flat0.equals(flat1))
						return gate;
				}
			}
		} else {
			for(Iterator<BooleanFormula> gates = opCache(op).get(hash); gates.hasNext(); ) {
				BooleanFormula gate = gates.next();
				if (gate.size()==2 && gate.input(0)==l && gate.input(1)==h)
					return gate;
			}
		}
		final BooleanFormula ret = MultiGate.make(op, nextLiteral++, hash, l, h);
		opCache(op).add(ret);
		return ret;
	}
	
	/**
	 * Wrapper for a method that performs circuit reductions and caching 
	 * for circuits created using AND and OR operators.
	 * @author Emina Torlak
	 */
	private static interface AndOrFactory {
		
		/**
		 * Returns a BooleanValue whose meaning is [[f0]] op [[f1]].  A
		 * new circuit is created and stored in factory.cache(op) iff [[f0]] op [[f1]] cannot be reduced
		 * to a simpler value and opCache does not already contain a circuit
		 * with equivalent meaning.
		 * @requires f0.op <= f1.op
		 * @requires f0 + f1 in factory.components
		 * @requires all arguments are non-null
		 * @return v: BooleanValue | [[v]] = [[f0]] op [[f1]]
		 * @effects (no v: BooleanValue | [[v]] = [[f0]] op [[f1]]) => 
		 *          factory.components' = factory.components + {v: BooleanValue - factory.components | [[v]] = [[f0]] op [[f1]]} => 
		 *          factory.components' - factory.components =  
		 */
		public abstract BooleanValue compose(Operator.Nary op, BooleanFormula f0, BooleanFormula f1, BooleanFactory factory);
		
	}
	
	/**
	 * Performs common simplifications on circuits of the form AND op X or OR op X, 
	 * where X can be any operator other than CONST (J stands for 'junction').
	 */
	private static final AndOrFactory JoX = new AndOrFactory() {
		/**
		 * Performs the following reductions, if possible.  Note that 
		 * these reductions will be possible only if f0 was created after f1 (i.e.  |f0.label| > |f1.label|).
		 * (a & b) & a = a & b	(a & b) & !a = F	  (a & b) | a = a
		 * (a | b) | a = a | b	(a | b) | !a = T	  (a | b) & a = a
		 * @requires f0.op in (AND + OR)
		 */
		public BooleanValue compose(Nary op, BooleanFormula f0, BooleanFormula f1, BooleanFactory factory) {
			assert f0.op().ordinal < 2;
			if (f0.contains(f0.op(), f1, factory.k) > 0) 
				return op==f0.op() ? f0 : f1;
			else if (op==f0.op() && f0.contains(op, f1.negation(), factory.k) > 0) 
				return op.shortCircuit();
			else 
				return factory.cache(op, f0, f1);
		}
	};
	
	/**
	 * Performs common simplifications on circuits of the form AND op OR.
	 */
	private static final AndOrFactory AoO = new AndOrFactory() {
		/**
		 * Performs the following reductions, if possible, along with JoX reductions.
		 * (aj & ... & ak) & (a1 | ... | an) = (aj & ... & ak) where 1 <= j <= k <= n  
		 * (a1 & ... & an) | (aj | ... | ak) = (ai | ... | ak) where 1 <= j <= k <= n
		 * @requires f0.op in (AND + OR)
		 */
		public BooleanValue compose(Nary op, BooleanFormula f0, BooleanFormula f1, BooleanFactory factory) {
			assert f0.op() == AND && f1.op() == OR;
			factory.flat0.clear(); 
			factory.flat1.clear();
			f0.flatten(op, factory.flat0, factory.k); 
			f1.flatten(op, factory.flat1, factory.k);
			if (op==AND && factory.flat0.size()<=factory.flat1.size() && factory.flat1.containsAll(factory.flat0)) 
				return f0;
			else if (op==OR && factory.flat0.removeAll(factory.flat1))
				return f1;
			else if (f0.label() < f1.label()) // f0 created before f1
				return JoX.compose(op, f1, f0, factory);
			else
				return JoX.compose(op, f0, f1, factory);
		}
		
	};
	
	/**
	 * Performs common simplifications on circuits of the form AND op AND or OR op OR.
	 */
	private static final AndOrFactory JoJ = new AndOrFactory() {
		/**
		 * Performs the following reductions, if possible, along with the JoX reductions.
		 * (a1 & ... & an) & (aj & ... & ak) = (a1 & ... & an) where 1 <= j <= k <= n
		 * (a1 & ... & an) | (aj & ... & ak) = (aj & ... & ak) where 1 <= j <= k <= n
		 * (a1 | ... | an) | (aj | ... | ak) = (a1 | ... | an) where 1 <= j <= k <= n
		 * (a1 | ... | an) & (aj | ... | ak) = (aj | ... | ak) where 1 <= j <= k <= n
		 * @requires (f0+f1).op in (AND + OR)
		 */
		public BooleanValue compose(Nary op, BooleanFormula f0, BooleanFormula f1, BooleanFactory factory) {
			assert f0.op() == f1.op();
			if (f0==f1) return f0;
			factory.flat0.clear(); 
			factory.flat1.clear();
			f0.flatten(op, factory.flat0, factory.k); 
			f1.flatten(op, factory.flat1, factory.k);
			if (factory.flat0.size()<factory.flat1.size() && factory.flat1.containsAll(factory.flat0)) 
				return op==f0.op() ? f1 : f0;
			else if (factory.flat0.size()>=factory.flat1.size() && factory.flat0.containsAll(factory.flat1))
				return op==f0.op() ? f0 : f1;
			else if (f0.label() < f1.label()) // f0 created before f1
				return JoX.compose(op, f1, f0, factory);
			else
				return JoX.compose(op, f0, f1, factory);
		}
		
	};
	
	/**
	 * Performs common simplifications on circuits of the form AND op ITE or OR op ITE.
	 */
	private static final AndOrFactory JoI = new AndOrFactory() {
		/**
		 * Combines JoX and IoX reductions.
		 * @requires f0.op in (AND + OR) && f1.op = ITE
		 */
		public BooleanValue compose(Nary op, BooleanFormula f0, BooleanFormula f1, BooleanFactory factory) {
			assert f0.op().ordinal < 2 && f1.op() == ITE;
			if (f0.label() < f1.label()) // f0 created before f1
				return IoX.compose(op, f1, f0, factory); 
			else
				return JoX.compose(op, f0, f1, factory);
		}
	};
	
	/**
	 * Performs common simplifications on circuits of the form AND op NOT or OR op NOT.
	 */
	private static final AndOrFactory JoN = new AndOrFactory() {
		/**
		 * Performs the following reductions, if possible, along with the JoX/NoX reductions.
		 * a & !a = F	a | !a = T
		 * @requires f0.op in (AND + OR) && f1.op = NOT
		 */
		public BooleanValue compose(Nary op, BooleanFormula f0, BooleanFormula f1, BooleanFactory factory) {
			assert f0.op().ordinal < 2 && f1.op() == NOT;
			if (f0.label()==-f1.label()) return op.shortCircuit();
			else if (f0.label() < StrictMath.abs(f1.label()))  // f0 created before f1
				return NoX.compose(op, f1, f0, factory);
			else 
				return JoX.compose(op, f0, f1, factory);
		}
	};
	
	/**
	 * Performs common simplifications on circuits of the form ITE op X, where X can be any operator other than CONST.
	 */
	private static final AndOrFactory IoX = new AndOrFactory() {
		/**
		 * Performs the following reductions, if possible.  Note that 
		 * these reductions will be possible only if f0 was created after f1 (i.e.  |f0.label| > |f1.label|).
		 * (a ? b : c) & a = a & b	(a ? b : c) & !a  = !a & c
		 * (a ? b : c) | a = a | c	(a ? b : c) | !a = !a | b
		 * @requires f0.op = ITE && AND.ordinal = 0 && OR.ordinal = 1
		 */
		public BooleanValue compose(Nary op, BooleanFormula f0, BooleanFormula f1, BooleanFactory factory) {
			assert f0.op() == ITE;
			if (f0.input(0)==f1) 
				return factory.fastCompose(op, f0.input(op.ordinal+1), f1);
			else if (f0.input(0).label()==-f1.label()) 
				return factory.fastCompose(op, f0.input(2-op.ordinal), f1);
			else
				return factory.cache(op, f0, f1);
		}
	};
	
	/**
	 * Performs common simplifications on circuits of the form ITE op ITE.
	 */
	private static final AndOrFactory IoI = new AndOrFactory() {
		/**
		 * Performs the following reductions, if possible, along with IoX reductions.
		 * (a ? b : c) & (a ? b : c) = (a ? b : c)		(a ? b : c) & (!a ? b : c) = b & c
		 * (a ? b : c) | (a ? b : c) = (a ? b : c)		(a ? b : c) | (!a ? b : c) = b | c
		 * @requires f0.op + f1.op = ITE 
		 */
		public BooleanValue compose(Nary op, BooleanFormula f0, BooleanFormula f1, BooleanFactory factory) {
			assert f0.op() == ITE && f1.op() == ITE;
			if (f0==f1) return f0;
			else if (f0.input(0).label()==-f1.input(0).label() && f0.input(1)==f1.input(1) && f0.input(2)==f1.input(2)) 
				return factory.fastCompose(op, f0.input(1), f0.input(2)); 
			else if (f0.label() < f1.label()) // f0 created before f1
				return IoX.compose(op, f1, f0, factory); 
			else
				return IoX.compose(op, f0, f1, factory);
		}
	};
	
	/**
	 * Performs common simplifications on circuits of the form ITE op NOT.
	 */
	private static final AndOrFactory IoN =new AndOrFactory() {
		/**
		 * Performs the following reductions, if possible, along with IoX/NoX reductions.
		 * (a ? b : c) & !(a ? b : c) = F		
		 * (a ? b : c) | !(a ? b : c) = T
		 * @requires f0.op = ITE && f1.op = NOT
		 */
		public BooleanValue compose(Nary op, BooleanFormula f0, BooleanFormula f1, BooleanFactory factory) {
			assert f0.op() == ITE && f1.op() == NOT;
			if (f0.label()==-f1.label()) return op.shortCircuit();
			else if (f0.label() < StrictMath.abs(f1.label()))  // f0 created before f1
				return NoX.compose(op, f1, f0, factory);
			else 
				return IoX.compose(op, f0, f1, factory);
		}
	};
	
	/**
	 * Performs common simplifications on circuits of the form NOT op X, where X can be any operator other than CONST.
	 */
	private static final AndOrFactory NoX = new AndOrFactory() {
		/**
		 * Performs the following reductions, if possible.  Note that 
		 * these reductions will be possible only if f0 was created after f1 (i.e.  |f0.label| > |f1.label|).
		 * !(a | b) & a = F	!(a | b) & !a = !(a | b)
		 * !(a & b) | a = T	!(a & b) | !a = !(a & b)
		 * @requires f0.op = NOT
		 */
		public BooleanValue compose(Nary op, BooleanFormula f0, BooleanFormula f1, BooleanFactory factory) {
			assert f0.op() == NOT ;
			if (f0.input(0).contains(op.complement(), f1, factory.k) > 0) return op.shortCircuit();
			else if (f0.input(0).contains(op.complement(), f1.negation(), factory.k) > 0) return f0;
			else return factory.cache(op, f0, f1);
		}
	};
	
	/**
	 * Performs common simplifications on circuits of the form NOT op NOT.
	 */
	private static final AndOrFactory NoN = new AndOrFactory() {
		/**
		 * Performs the following reductions, if possible, along with NoX reductions.
		 * !a & !a = !a		!a | !a = !a
		 * @requires f1.op + f0.op = NOT
		 */
		public BooleanValue compose(Nary op, BooleanFormula f0, BooleanFormula f1, BooleanFactory factory) {
			assert f0.op() == NOT && f1.op() == NOT;
			if (f0==f1) return f0;
			else if (f0.label() < f1.label()) // f0 created after f1
				return NoX.compose(op, f0, f1, factory);
			else 
				return NoX.compose(op, f1, f0, factory);
		}
	};
	
	/**
	 * Performs common simplifications on circuits of the form NOT op VAR.
	 */
	private static final AndOrFactory NoV = new AndOrFactory() {
		/**
		 * Performs the following reductions, if possible, along with NoX reductions.
		 * !a & a = F		!a | a = T
		 * @requires f1.op = NOT && f1.op = VAR
		 */
		public BooleanValue compose(Nary op, BooleanFormula f0, BooleanFormula f1, BooleanFactory factory) {
			assert f0.op() == NOT && f1.op() == VAR;
			if (f0.label()==-f1.label()) return op.shortCircuit();
			else return NoX.compose(op, f0, f1, factory);
		}
	};
	
	/**
	 * Performs common simplifications on circuits of the form VAR op VAR.
	 */
	private static final AndOrFactory VoV = new AndOrFactory() {
		/**
		 * Performs the following reductions, if possible:
		 * a & a = a 
		 * a | a = a
		 * @requires f0.op + f1.op = VAR
		 */
		public BooleanValue compose(Nary op, BooleanFormula f0, BooleanFormula f1, BooleanFactory factory) {
			assert f0.op() == VAR && f1.op() == VAR;
			return (f0==f1) ? f0 : factory.cache(op, f0, f1); 
		}
	};
	
	/**
	 * 15 AndOrFactories representing all possible composition combinations of 
	 * non-constant vertices using the operators AND and OR.  Note that there
	 * are 15 of them rather than 25 because of the v0.op <= v1.op requirement
	 * of the {@link AndOrFactory#compose(Operator.Nary, BooleanFormula, BooleanFormula, BooleanFactory) compose} method.
	 */
	private static final AndOrFactory[] AOFACTORIES = {
		JoJ,		/* AND op AND */
		AoO,		/* AND op OR */
		JoI,		/* AND op ITE */
		JoN,		/* AND op NOT */
		JoX,		/* AND op VAR */
		JoJ,		/* OR op OR */
		JoI,		/* OR op ITE */
		JoN,		/* OR op NOT */
		JoX,		/* OR op VAR */
		IoI,		/* ITE op ITE */
		IoN, 	/* ITE op NOT */
		IoX,		/* ITE op VAR */
		NoN,		/* NOT op NOT */
		NoV,		/* NOT op VAR */
		VoV		/* VAR op VAR */
	};
	
	
	/**
	 * Wrapper for a method that performs circuit reductions and caching 
	 * for circuits created using the ITE operator.
	 * @author Emina Torlak
	 */
	private static interface ITEFactory {
		
		/**
		 * Returns a BooleanValue whose meaning is (i ? t : e).  A
		 * new circuit is created and stored in factory.cache(ITE) iff (i ? t : e) cannot be reduced
		 * to a simpler value and opCache does not already contain a circuit
		 * with equivalent meaning.
		 * @requires an implementing class may place constraints on the op values of the arguments
		 * @requires i + t + e in factory.components
		 * @requires all arguments are non-null
		 * @return {v: factory.components' | [[v]] = (i ? t : e)} 
		 * @effects (no v: BooleanValue | [[v]] = (i ? t : e)) =>  
		 *          factory.components' = factory.components + {v: BooleanValue - factory.components | [[v]] = (i ? t : e)},
		 *          factory.components' = factory.components
		 */
		public abstract BooleanValue ite(BooleanValue i, BooleanValue t, BooleanValue e, BooleanFactory factory);
		
	}
}