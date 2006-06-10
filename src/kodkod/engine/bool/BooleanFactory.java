package kodkod.engine.bool;

import static kodkod.engine.bool.Operator.AND;
import static kodkod.engine.bool.Operator.OR;
import kodkod.engine.Options;
import kodkod.util.ints.IntSet;


/**
 * A factory for creating {@link kodkod.engine.bool.BooleanValue boolean values}, 
 * {@link kodkod.engine.bool.BooleanMatrix matrices}, and {@link kodkod.engine.bool.Int ints}.
 * 
 * @specfield comparisonDepth: int // the depth to which circuits should be checked for equality 
 * @specfield intEncoding: Options.IntEncoding // the encoding used for generating integers ({@link #integer(int)}
 * @specfield bitwidth: int // the bitwidth used for integer computations
 * @specfield components: set BooleanValue
 * @invariant no f1, f2: BooleanFactory | f1 != f2 => f1.components & f2.components = BooleanConstant
 * @author Emina Torlak
 */
public final class BooleanFactory {
	/**
	 * IMPLEMENTATION NOTE:  BooleanFactory is the facade and a mediator for this package.
	 */
	private static CircuitFactory CONSTANT_FACTORY;
	/**
	 * A circuit factory used internally to assemble circuits.
	 */
	private final CircuitFactory circuits;
	
	/** The bitwidth used for integer computations */
	final int bitwidth;
	
	/**
	 * Constructs a boolean factory with the given number of input variables.  Gates are
	 * checked for semantic equality down to the given depth.  Integers are represented
	 * using the given number of bits.
	 * @requires 0 <= numVars < Integer.MAX_VALUE
	 * @requires checkToDepth >= 0 && bitwidth > 0
	 * @effects #this.components' = numInputVariables && this.components' in BooleanVariable 
	 * @effects this.bitwidth' = bitwidth
	 * @effects this.comparisonDepth' = comparisonDepth
	 */
	@SuppressWarnings("unchecked")
	private BooleanFactory(int numVars, int comparisonDepth, int bitwidth) {
		if (numVars==0) {
			if (CONSTANT_FACTORY==null)
				CONSTANT_FACTORY = new CircuitFactory(0, 1);
			this.circuits = CONSTANT_FACTORY;
		} else {
			this.circuits = new CircuitFactory(numVars, 1<<comparisonDepth);
		}
		this.bitwidth = bitwidth;
	}
	
	/**
	 * Returns a boolean factory, initialized to contain the given number
	 * of boolean variables.  Gates are checked for semantic equality 
	 * down to depth 3 when checking for cached values.  Integers are created/manipulated 
	 * according to the specifications in the given Options object.
	 * The effect of this method is the same as calling BooleanFactory.factory(numVars, 3, options).
	 * @return {f: BooleanFactory | #(f.components & BooleanVariable) = numVars &&
	 *                              BooleanConstant in f.components && f.components in BooleanVariable + BooleanConstant &&
	 *                              f.comparisonDepth = compDepth && 
	 *                              f.bitwidth = options.bitwidth && f.intEncoding = options.intEncoding && 
	 *                              (all i: [1..numVars] | one f.components.label & i }}
	 * @throws IllegalArgumentException - numVars < 0 || numVars = Integer.MAX_VALUE
	 */
	public static BooleanFactory factory(int numVars, Options options) {
		return factory(numVars, 3, options);
	}
	
	/**
	 * Returns a boolean factory, initialized to contain the given number
	 * of boolean variables.  
	 * <p>Gates are checked for semantic equality 
	 * down to the given depth when checking for cached values.  In general,  setting the
	 * comparison depth to a higher value will result in more 
	 * subcomponents being shared.  However, it will also slow down
	 * gate construction.  </p>
	 * <p>Integers are created/manipulated according to the specifications in the given Options object.</p>
	 * @return {f: BooleanFactory | #(f.components & BooleanVariable) = numVars &&
	 *                              BooleanConstant in f.components && f.components in BooleanVariable + BooleanConstant &&
	 *                              f.comparisonDepth = compDepth && 
	 *                              f.bitwidth = options.bitwidth && f.intEncoding = options.intEncoding && 
	 *                              (all i: [1..numVars] | one f.components.label & i }}
	 * @throws IllegalArgumentException - numVars < 0 || numVars = Integer.MAX_VALUE
	 * @throws IllegalArgumentException - compDepth < 1
	 * @throws NullPointerException - options = null
	 */
	public static BooleanFactory factory(int numVars, int compDepth, Options options) {
		if (numVars < 0 || numVars == Integer.MAX_VALUE) 
			throw new IllegalArgumentException("numVars < 0 || numVars = Integer.MAX_VALUE");
		if (compDepth < 1) throw new IllegalArgumentException("checkToDepth < 1: " + compDepth);
		return new BooleanFactory(numVars, compDepth, options.bitwidth());
	}
	
	/**
	 * Returns the depth (from the root) to which components are checked for 
	 * semantic equality during gate construction.
	 * @return this.comparisonDepth
	 */
	public final int comparisonDepth() { return Integer.numberOfTrailingZeros(circuits.cmpMax()); }
	
	/**
	 * Sets the comparison depth to the given value.  Setting the
	 * comparison depth to a high value will result in more 
	 * subcomponents being shared.  However, it will also slow down
	 * gate construction.
	 * @effects this.comparisonDepth' = newDepth
	 * @throws IllegalArgumentException - newDepth < 1
	 */
	public final void setComparisonDepth(int newDepth) {
		if (newDepth < 1)
			throw new IllegalArgumentException("newDepth < 1: " + newDepth);
		circuits.setCmpMax(1<<newDepth);
	}
	
	/**
	 * Returns the bitwidth used for integer representation.
	 * @return this.bitwidth
	 */
	public final int bitwidth() { return bitwidth; }
	
	/**
	 * Returns true if v is in this.components.
	 * @return v in this.components
	 * @throws NullPointerException - v = null
	 */
	public final boolean contains(BooleanValue v) {
		return circuits.canAssemble(v);
	}
	
	/**
	 * Returns the number of variables in this.components
	 * @return #(BooleanVariable & this.components)
	 */
	public final int numberOfVariables() { return circuits.numVars(); }
	
	/**
	 * Returns the variable with the given label.
	 * @requires 0 < label <= numberOfVariables()
	 * @return (this.components & BooleanVariable).label
	 */
	public final BooleanVariable variable(int label) {
		return circuits.variable(label);
	}
	
	/**
	 * Returns the negation of the given boolean value.
	 * @return {n: BooleanValue | n.label = -v.label && [[n]] = ![[v]] }
	 * @effects (components.v).components' = (components.v).components + n 
	 * @throws NullPointerException - v = null                             
	 */
	public final BooleanValue not(BooleanValue v) {
		return v.negation();
	}
	
	/**
	 * Returns a boolean value whose meaning is the conjunction of the input components.  
	 * The behavior of this method is unspecified if v0 or v1 are not components of this factory.
	 * @requires v0 + v1 in this.components
	 * @return {v: BooleanValue | [[v]] = [[v0]] AND [[v1]] }
	 * @effects this.components' = this.components + v 
	 * @throws NullPointerException - any of the arguments are null
	 */
	public final BooleanValue and(BooleanValue v0, BooleanValue v1) {
		return circuits.assemble(AND, v0, v1);
	}
	
	/**
	 * Returns a boolean value whose meaning is the disjunction of the input components.  
	 * The behavior of this method is unspecified if v0 or v1 are not components of this factory.
	 * @requires v0 + v1 in this.components
	 * @return {v: BooleanValue | [[v]] = [[v0]] OR [[v1]] }
	 * @effects this.components' = this.components + v 
	 * @throws NullPointerException - any of the arguments are null
	 * @throws IllegalArgumentException - v0 + v1 !in this.components
	 */
	public final BooleanValue or(BooleanValue v0, BooleanValue v1) {
		return circuits.assemble(OR, v0, v1);
	}
	
	/**
	 * Returns a boolean value whose meaning is [[v0]] ^ [[v1]].  
	 * The behavior of this method is unspecified if v0 or v1 are not components of this factory.
	 * @requires v0 + v1 in this.components
	 * @return { v: BooleanValue | [[v]] = [[v0]] xor [[v1]] }
	 * @effects this.components' = this.components + v
	 * @throws NullPointerException - any of the arguments are null
	 */
	public final BooleanValue xor(BooleanValue v0, BooleanValue v1) {
		return circuits.assemble(v0, v1.negation(), v1);
	}
	
	/**
	 * Returns a boolean value whose meaning is [[v0]] => [[v1]].  
	 * The behavior of this method is unspecified if v0 or v1 are not components of this factory.
	 * @requires v0 + v1 in this.components
	 * @return { v: BooleanValue | [[v]] = [[v0]] => [[v1]] }
	 * @effects this.components' = this.components + v
	 * @throws NullPointerException - any of the arguments are null
	 */
	public final BooleanValue implies(BooleanValue v0, BooleanValue v1) {
		return circuits.assemble(OR, v0.negation(), v1);
	}
	
	/**
	 * Returns a boolean value whose meaning is [[v0]] <=> [[v1]].
	 * The behavior of this method is unspecified if v0 or v1 are not components of this factory.
	 * @requires v0 + v1 in this.components
	 * @return { v: BooleanValue | [[v]] = [[v0]] iff [[v1]] }
	 * @effects this.components' = this.components + v
	 * @throws NullPointerException - any of the arguments are null
	 */
	public final BooleanValue iff(BooleanValue v0, BooleanValue v1) {
		return circuits.assemble(v0, v1, v1.negation());
	}
	
	/**
	 * Returns a boolean value whose meaning is [[i]] ? [[t]] : [[e]].
	 * The behavior of this method is unspecified if i, t, or e are not components of this factory.
	 * @requires i + t + e in this.components 
	 * @return { v: BooleanValue | [[v]] = [[i]] ? [[t]] : [[e]] }
	 * @effects this.components' = this.components + v
	 * @throws NullPointerException - any of the arguments are null
	 */
	public final BooleanValue ite(BooleanValue i, BooleanValue t, BooleanValue e) {
		return circuits.assemble(i, t, e);
	}
	
	/**
	 * Converts the given accumulator into an immutable boolean value and adds it to this.components.
	 * This method requires that all of g's inputs are in this.components.  If g has no inputs,
	 * its operator's identity constant is returned.  If g has one input, that input is returned.
	 * Otherwise, an immutable value that is semantically equivalent to g is returned.
	 * The behavior of this method is unspecified if the components of g are not components of this factory.
	 * @requires g.components in this.components 
	 * @return no g.inputs => g.op.identity(), 
	 *         one g.inputs => g.inputs, 
	 *         {g' : BooleanValue - BooleanAccumulator | [[g']] = [[g]] }
	 * @effects this.components' = this.components + g'
	 */
	public final BooleanValue accumulate(BooleanAccumulator g) {
		return circuits.assemble(g);
	}
	
	/**
	 * Returns an Int that represents the given number as a string
	 * of boolean values using the specified encoding.
	 * @return an Int that represents the given number as a string
	 * of boolean values using the specified encoding.
	 * @throws IllegalArgumentException - the number cannot be represented using 
	 * the specified encoding
	 */
	public Int integer(int number, Int.Encoding encoding) {
		return new Int(this, encoding, number);
	}
		
	/**
	 * Removes all formulas with one or more inputs from this.components.
	 * @effects this.componets' = 
	 *    BooleanConstant + this.components & BooleanVariable
	 */
	public final void clear() {
		circuits.clear();
	}
	
	/**
	 * Returns a BooleanMatrix with the given dimensions and this 
	 * as the factory for its non-FALSE components.  The returned matrix 
	 * can store any value from this.components at all
	 * indices between 0, inclusive, and d.capacity(), exclusive.  
	 * @throws NullPointerException - d = null
	 * @return { m: BooleanMatrix | m.factory = this && m.dimensions = d && m.elements = [0..d.capacity) -> one FALSE }
	 */
	public final BooleanMatrix matrix(Dimensions d) {
		if (d == null ) throw new NullPointerException();
		return new BooleanMatrix(d, this);
	}
	
	/**
	 * @throws IllegalArgumentException - indices !in [0..d.capacity())
	 */
	private static void validate(IntSet indices, Dimensions d) {
		if (!indices.isEmpty()) {
			if (!d.validate(indices.min()) ||	!d.validate(indices.max()))
				throw new IllegalArgumentException();
		}
	}
	
	/**
	 * Returns a BooleanMatrix <tt>m</tt> with the given dimensions, this
	 * as its factory, and the indices from the set <tt>trueIndices</tt> initialized
	 * to TRUE.  An IndexOutOfBoundsException may be thrown
	 * if {@link BooleanMatrix#set(int, BooleanValue)} is called on <tt>m</tt> with an index 
	 * not contained in <tt>allIndices</tt>.  If <tt>allIndices.equals(trueIndices)</tt>, 
	 * <tt>m</tt> may be a constant matrix; that is, an IllegalArgumentException may be
	 * thrown if {@link BooleanMatrix#set(int, BooleanValue)} is called on <tt>m</tt> with
	 * a non-constant value.  Finally, if cloning <tt>trueIndices</tt> results in an immutable
	 * set, then {@link BooleanMatrix#set(int, BooleanValue) m.set(int, BooleanValue)} may throw
	 * an UnsupportedOperationException when called with a member of <tt>trueIndices</tt>.
	 * @requires allIndices.containsAll(trueIndices)
	 * @return { m: BooleanMatrix |  m.factory = this && m.dimensions = dims && 
	 *           m.elements = [0..d.capacity()-1] ->one FALSE ++ indices->TRUE }
	 * @throws IllegalArgumentException - allIndices !in [0..d.capacity())
	 * @throws IllegalArgumentException - one of the input sets is not cloneable
	 * @throws NullPointerException - d = null || allIndices = null || trueIndices = null	 	 
	 */
	public final BooleanMatrix matrix(Dimensions d, IntSet allIndices, IntSet trueIndices) {
		assert allIndices.size() >= trueIndices.size(); // sanity check
		validate(allIndices, d); validate(trueIndices, d);
		try {
			return new BooleanMatrix(d, this, allIndices, trueIndices.clone());
		} catch (CloneNotSupportedException e) {
			throw new IllegalArgumentException();
		}
	
	}
		
}

