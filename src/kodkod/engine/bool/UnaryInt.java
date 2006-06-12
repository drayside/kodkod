package kodkod.engine.bool;

import static kodkod.engine.bool.BooleanConstant.FALSE;
import static kodkod.engine.bool.BooleanConstant.TRUE;
import static kodkod.engine.bool.Operator.OR;

import java.util.Arrays;

/**
 * Unary integer representation.  Supports comparisons and addition of 
 * non-negative numbers.  A non-negative integer i is represented as a
 * string of i non-FALSE BooleanValues.
 * 
 * @author Emina Torlak
 */
final class UnaryInt extends Int {
	private final BooleanValue[] bits;
	
	/**
	 * Constructs a UnaryInt out of the given factory and bits.
	 * @requires bits is well formed
	 * @effects this.factory' = factory && this.bits' = bits
	 */
	private UnaryInt(BooleanFactory factory, BooleanValue[] bits) {
		super(factory);
		this.bits = bits;
	}
	
	/**
	 * Constructs a UnaryInt encoding of the given number, using at most
	 * factory.bitwidth bits.
	 * @requires factory.encoding = UNARY && number >= 0  
	 * @effects this.factory' = factory && this.bits'[0..min(number, factory.bitwidth)) -> one TRUE 
	 */
	UnaryInt(BooleanFactory factory, int number) {
		super(factory);
		assert number >= 0;
		final int width = StrictMath.min(number, factory.bitwidth);
		this.bits = new BooleanValue[width];
		for(int i = 0; i < width; i++) 
			bits[i] = TRUE;
	}

	/**
	 * Constructs a UnaryInt encoding of the given bit.
	 * @requires factory.encoding = UNARY 
	 * @effects this.factory' = factory 
	 * @effects bit=FALSE => no this.bits', this.bits' = 0 -> bit 
	 */
	UnaryInt(BooleanFactory factory, BooleanValue bit) {
		super(factory);
		if (bit==FALSE) {
			this.bits = new BooleanValue[0];
		} else {
			this.bits = new BooleanValue[]{ bit };
		}
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.bool.Int#width()
	 */
	@Override
	public int width() {
		return bits.length;
	}

	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.bool.Int#bit(int)
	 */
	@Override
	public BooleanValue bit(int i) {
		return i < bits.length ? bits[i] : FALSE;
	}

	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.bool.Int#lte(kodkod.engine.bool.Int)
	 */
	@Override
	public BooleanValue lte(Int other) {
		validate(other);
		final BooleanAccumulator cmp = BooleanAccumulator.treeGate(Operator.AND);
		for(int i = 0, width = StrictMath.max(width(), other.width()); i < width; i++) {
			if (cmp.add(factory.implies(bit(i), other.bit(i)))==FALSE) 
				return FALSE;
		}
		return factory.accumulate(cmp);
	}

	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.bool.Int#plus(kodkod.engine.bool.Int)
	 */
	@Override
	public Int plus(Int other) {
		validate(other);
		final int width = StrictMath.min(width() + other.width(), factory.bitwidth);
		final BooleanValue[] plus = new BooleanValue[width];
		for(int i = 0; i < width; i++) {
			BooleanAccumulator acc = BooleanAccumulator.treeGate(OR);
			acc.add(bit(i)); 
			acc.add(other.bit(i));
			for(int j = 0; j < i; j++) {
				acc.add(factory.and( bit(j), other.bit(i - 1 - j)));
			}
			plus[i] = factory.accumulate(acc);
		}
		return new UnaryInt(factory, plus);
	}

	/**
	 * Unsupported.
	 * @throws UnsupportedOperationException
	 */
	@Override
	public Int minus(Int other) {
		throw new UnsupportedOperationException();
	}
	
	/**
	 * {@inheritDoc}
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return "u"+Arrays.toString(bits);
	}
	
}


