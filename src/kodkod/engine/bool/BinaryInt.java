/**
 * 
 */
package kodkod.engine.bool;

import static kodkod.engine.bool.BooleanConstant.FALSE;
import static kodkod.engine.bool.BooleanConstant.TRUE;

import java.util.Arrays;

/**
 * Two's complement integer representation.  Supports comparisons, addition and subtraction.  
 * Integers are represented in little-endian (least significant bit first) order.
 * @author Emina Torlak
 */
final class BinaryInt extends Int {
	private final BooleanValue[] bits;
	
	/**
	 * Constructs a BinaryInt out of the given factory and bits.
	 * @requires bits is well formed
	 * @effects this.factory' = factory && this.bits' = bits
	 */
	private BinaryInt(BooleanFactory factory, BooleanValue[] bits) {
		super(factory);
		this.bits = bits;
	}
	
	/**
	 * Constructs a BinaryInt encoding of the given number, using at most
	 * factory.bitwidth bits.
	 * @requires factory.encoding = BINARY  
	 * @effects this.factory' = factory 
	 */
	BinaryInt(BooleanFactory factory, int number) {
		super(factory);
		final int width = bitwidth(number);
		this.bits = new BooleanValue[width];
		for(int i = 0; i < width; i++) {
			bits[i] = (number & (1<<i)) == 0 ? FALSE : TRUE;
		}
	}

	/**
	 * Constructs a BinaryInt that represents either 0 or 1, depending on 
	 * the value of the given bit.
	 * @requires factory.encoding = BINARY  
	 * @effects this.factory' = factory
	 * @effects bits = FALSE => this.bits' = 0 -> bit, this.bits' = 0 -> bit + 1 -> FALSE
	 */
	BinaryInt(BooleanFactory factory, BooleanValue bit) {
		super(factory);
		if (bit==FALSE)
			this.bits = new BooleanValue[] { bit };
		else 
			this.bits = new BooleanValue[] { bit, FALSE };
	}
	
	/**
	 * Returns the number of bits needed/allowed to represent the given number.
	 * @return the number of bits needed/allowed to represent the given number.
	 */
	private int bitwidth(int number) {
		if (number > 0)
			return StrictMath.min(33 - Integer.numberOfLeadingZeros(number), factory.bitwidth);
		else if (number < 0)
			return StrictMath.min(33 - Integer.numberOfLeadingZeros(~number), factory.bitwidth);
		else // number = 0
			return 1;
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
		return bits[StrictMath.min(i, bits.length-1)];
	}

	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.bool.Int#lte(kodkod.engine.bool.Int)
	 */
	@Override
	public BooleanValue lte(Int other) {
		validate(other);
		final BooleanAccumulator cmp = BooleanAccumulator.treeGate(Operator.AND);
		final int last = StrictMath.max(width(), other.width())-1;
		cmp.add(factory.implies(other.bit(last), bit(last)));
		BooleanValue prevEquals = factory.iff(bit(last), other.bit(last));
		for(int i = last-1; i >= 0; i--) {
			BooleanValue v0 = bit(i), v1 = other.bit(i);
			cmp.add(factory.implies(prevEquals, factory.implies(v0, v1)));
			prevEquals = factory.and(prevEquals, factory.iff(v0, v1));
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
		final int width = bitwidth(-(1<<width())-(1<<other.width()));
		final BooleanValue[] plus = new BooleanValue[width];
		BooleanValue carry = FALSE;
		for(int i = 0; i < width; i++) {
			BooleanValue v0 = bit(i), v1 = other.bit(i);
			plus[i] = factory.sum(v0, v1, carry);
			carry = factory.carry(v0, v1, carry);
		}
		return new BinaryInt(factory, plus);
	}

	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.bool.Int#minus(kodkod.engine.bool.Int)
	 */
	@Override
	public Int minus(Int other) {
		validate(other);
		final int width = bitwidth(-(1<<width())-(1<<other.width()));
		final BooleanValue[] minus = new BooleanValue[width];
		BooleanValue carry = TRUE;
		for(int i = 0; i < width; i++) {
			BooleanValue v0 = bit(i), v1 = other.bit(i).negation();
			minus[i] = factory.sum(v0, v1, carry);
			carry = factory.carry(v0, v1, carry);
		}
		return new BinaryInt(factory, minus);
	}

	/**
	 * {@inheritDoc}
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return "b" + Arrays.toString(bits);
	}
}
