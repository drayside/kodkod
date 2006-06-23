package kodkod.engine.bool;

import static kodkod.engine.bool.BooleanConstant.FALSE;
import static kodkod.engine.bool.Operator.AND;
import static kodkod.engine.bool.Operator.OR;

/**
* An integer represented using {@link kodkod.engine.bool.BooleanValue boolean values}
* and {@link kodkod.engine.Options.IntEncoding#UNARY unary} or
* {@link kodkod.engine.Options.IntEncoding#BINARY binary} encoding.
* 
* @specfield factory: BooleanFactory
* @specfield bits: [0..factory.bitwidth) -> one factory.components
* @specfield encoding: factory.intEncoding
* @author Emina Torlak
*/
public abstract class Int {
	final BooleanFactory factory;
	
	/**
	 * Creates an Int with the given factory
	 * @effects this.factory' = factory
	 */
	Int(BooleanFactory factory) {
		this.factory = factory;
	}
	
	/**
	 * Returns this.factory
	 * @return this.factory
	 */
	public final BooleanFactory factory() { return factory; }
	
	/**
	 * Returns the number of bits in the representation of this Int,
	 * including sign bits (if any).
	 * @return this.width
	 */
	public abstract int width(); 

	/**
	 * Returns the BooleanValue at the specified index.
	 * @requires 0 <= i < this.factory.bitwidth
	 * @return this.bits[i]
	 */
	public abstract BooleanValue bit(int i);
	
	/**
	 * Returns a BooleanValue encoding the comparator circuit
	 * that checks whether the integer represented by this
	 * Int is equal to the integer represented by the specified Int.
	 * @requires this.factory = other.factory
	 * @return BooleanValue encoding the comparator circuit
	 * that checks whether the integer represented by this
	 * Int is equal to the integer represented by the specified Int
	 * @throws IllegalArgumentException - this.factory != other.factory 
	 */
	public final BooleanValue eq(Int other) {
		validate(other);
		final BooleanAccumulator cmp = BooleanAccumulator.treeGate(AND);
		for(int i = 0, width = StrictMath.max(width(), other.width()); i < width; i++) {
			if (cmp.add(factory.iff(bit(i), other.bit(i)))==FALSE)
				return FALSE;
		}
		return factory.accumulate(cmp);
	}
	
	/**
	 * Returns a BooleanValue encoding the comparator circuit
	 * that checks whether the integer represented by this
	 * Int is less than or equal to the integer
	 * represented by the specified Int
	 * @requires this.factory = other.factory 
	 * @return BooleanValue encoding the comparator circuit
	 * that checks whether the integer represented by this
	 * Int is less than or equal to the integer
	 * represented by the specified Int
	 * @throws IllegalArgumentException - this.factory != other.factory 
	 */
	public abstract BooleanValue lte(Int other);
	
	/**
	 * Returns a BooleanValue encoding the comparator circuit
	 * that checks whether the integer represented by this
	 * Int is less than the integer
	 * represented by the specified Int.
	 * @requires this.factory = other.factory
	 * @return BooleanValue encoding the comparator circuit
	 * that checks whether the integer represented by this
	 * Int is less than the integer
	 * represented by the specified Int
	 * @throws IllegalArgumentException - this.factory != other.factory 
	 */
	public BooleanValue lt(Int other) {
		final BooleanValue leq = lte(other);
		final BooleanAccumulator acc = BooleanAccumulator.treeGate(OR);
		for(int i = 0, width = StrictMath.max(width(), other.width()); i < width; i++) {
			acc.add(factory.xor(bit(i), other.bit(i)));
		}
		return factory.and(leq, factory.accumulate(acc));
	}
	
	/**
	 * Returns a BooleanValue encoding the comparator circuit
	 * that checks whether the integer represented by this
	 * Int is greater than or equal to the integer
	 * represented by the specified Int.
	 * @requires this.factory = other.factory 
	 * @return BooleanValue encoding the comparator circuit
	 * that checks whether the integer represented by this
	 * Int is greater than or equal to the integer
	 * represented by the specified Int
	 * @throws IllegalArgumentException - this.factory != other.factory 
	 */
	public BooleanValue gte(Int other) {
		return other.lte(this);
	}
	
	/**
	 * Returns a BooleanValue encoding the comparator circuit
	 * that checks whether the integer represented by this
	 * Int is greater than the integer
	 * represented by the specified Int.
	 * @requires this.factory = other.factory 
	 * @return BooleanValue encoding the comparator circuit
	 * that checks whether the integer represented by this
	 * Int is greater than the integer
	 * represented by the specified Int
	 * @throws IllegalArgumentException - this.factory != other.factory
	 */
	public BooleanValue gt(Int other) {
		return other.lt(this);
	}
	
	/**
	 * Returns an Int that represents the sum of this and the given Int.
	 * @requires this.factory = other.factory 
	 * @return an Int that represents the sum of this and the given Int
	 * @throws IllegalArgumentException - this.factory != other.factory
	 */
	public abstract Int plus(Int other);
	
	/**
	 * Returns an Int that represents the difference between this and the given Int.
	 * @requires this.factory = other.factory
	 * @return an Int that represents the difference between this and the given Int
	 * @throws UnsupportedOperationException - this.encoding does not support subtraction
	 */
	public abstract Int minus(Int other);
	
	/**
	 * Returns an Int that represents the product between this and the given Int.
	 * @requires this.factory = other.factory
	 * @return an Int that represents the product between this and the given Int
	 * @throws UnsupportedOperationException - this.encoding does not support multiplication
	 */
	public abstract Int multiply(Int other);
	
	/**
	 * Returns an Int that represents the ratio between this and the given Int.
	 * @requires this.factory = other.factory
	 * @return an Int that represents the ratio between this and the given Int
	 * @throws UnsupportedOperationException - this.encoding does not support division
	 */
	public abstract Int divide(Int other);
	
//	/**
//	 * Returns an Int that represents the bitwise conjunction of this and the given Int.
//	 * @requires this.factory = other.factory
//	 * @return an Int that represents the bitwise conjunction of this and the given Int.
//	 */
//	public abstract Int and(Int other);
//	
//	/**
//	 * Returns an Int that represents the bitwise disjunction of this and the given Int.
//	 * @requires this.factory = other.factory
//	 * @return an Int that represents the bitwise disjunction of this and the given Int.
//	 */
//	public abstract Int or(Int other);
//	
//	/**
//	 * Returns an Int that represents the bitwise XOR of this and the given Int.
//	 * @requires this.factory = other.factory
//	 * @return an Int that represents the bitwise XOR of this and the given Int.
//	 * @throws UnsupportedOperationException - this.encoding does not support XOR
//	 */
//	public abstract Int xor(Int other);
	
	/**
	 * Throws IllegalArgumentException if other.factory != this.factory.
	 * @throws IllegalArgumentException - other.factory != this.factory.
	 */
	final void validate(Int other) {
		if (other.factory != factory)
			throw new IllegalArgumentException("other.factory != this.factory");
	}
}
