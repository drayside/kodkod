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
	 * Constructs a BinaryInt that represents either 0 or the given number, depending on 
	 * the value of the given bit.
	 * @requires factory.encoding = BINARY  && bit in factory.components 
	 * @effects this.factory' = factory
	 * @effects bits is a two's-complement representation of the given number
	 * that uses the provided bit in place of 1's
	 */
	BinaryInt(BooleanFactory factory, int number, BooleanValue bit) {
		super(factory);
		final int width = bitwidth(number);
		this.bits = new BooleanValue[width];
		for(int i = 0; i < width; i++) {
			bits[i] = (number & (1<<i)) == 0 ? FALSE : bit;
		}
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
	 * @see kodkod.engine.bool.Int#value()
	 */
	public final int value() {
		int ret = 0;
		final int max = bits.length-1;
		for(int i = 0; i < max; i++) {
			if (bits[i]==TRUE) ret += 1<<i;
			else if (bits[i]!=FALSE)
				throw new IllegalStateException(this + " is not constant.");       
		}
		if (bits[max]==TRUE) ret -= 1<<max;
		else if (bits[max]!=FALSE)
			throw new IllegalStateException(this + " is not constant.");       
		return ret;
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
		final int width = StrictMath.min(StrictMath.max(width(), other.width()) + 1, factory.bitwidth);
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
		final int width = StrictMath.min(StrictMath.max(width(), other.width()) + 1, factory.bitwidth);
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
	 * Adds the newBit and the given carry to this.bits[index] and returns the new carry.
	 * @requires 0 <= index < this.width
	 * @effects this.bits'[index] = this.factory.sum(this.bits[index], newBit, cin)
	 * @return this.factory.carry(this.bits[index], newBit, cin)
	 */
	private BooleanValue addAndCarry(int index, BooleanValue newBit, BooleanValue cin) {
		BooleanValue oldBit = bits[index];
		bits[index] = factory.sum(oldBit, newBit, cin);
		return factory.carry(oldBit, newBit, cin);
	}

	
	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.bool.Int#multiply(kodkod.engine.bool.Int)
	 */
	@Override
	public Int multiply(Int other) {
		validate(other);
		final int imax = StrictMath.max(width(), other.width())-1;
		final int width = StrictMath.min(width()+other.width()-1, factory.bitwidth);
		final BooleanValue[] mult = new BooleanValue[width];
		final BinaryInt ret = new BinaryInt(factory, mult);
		
		/* first partial sum */
		BooleanValue iBit = bit(0), carry;
		for(int j = 0; j < width; j++) {
			mult[j] = factory.and(iBit, other.bit(j));
		}
		
		/* intermediate partial sums */
		for(int i = 1; i < imax; i++) {
			carry = FALSE;
			iBit = bit(i);
			for(int j = 0, jmax = width-i; j < jmax; j++) {
				carry = ret.addAndCarry(j+i, factory.and(iBit, other.bit(j)), carry);
			}
		}
		
		/* last partial sum is subtracted (see http://en.wikipedia.org/wiki/Multiplication_ALU) */
		carry = TRUE;
		iBit = bit(imax);
		for(int j = 0, jmax = width-imax; j < jmax; j++) {
			carry = ret.addAndCarry(j+imax, factory.and(iBit, other.bit(j)).negation(), carry);
		}
		//System.out.println(ret);
		return ret;
	}
	
	/**
	 * Returns an array of BooleanValues that represents the same
	 * integer as this, but using extwidth bits.
	 * @requires extwidth >= this.width()
	 * @return an array of BooleanValues that represents the same
	 * integer as this, but using extwidth bits.
	 */
	private BooleanValue[] extend(int extwidth) {
		final BooleanValue[] ext = new BooleanValue[extwidth];
		final int width = width();
		for(int i = 0; i < width; i++) {
			ext[i] = bits[i];
		}
		final BooleanValue sign = bits[width-1];
		for(int i = width; i < extwidth; i++) {
			ext[i] = sign;
		}
		return ext;
	}

	/**
	 * Returns the disjunction of the given values.
	 * @return the disjunction of the given values.
	 */
	private static BooleanValue or(BooleanValue[] values, BooleanFactory factory) {
		final BooleanAccumulator acc = BooleanAccumulator.treeGate(Operator.OR);
		for(int i = values.length-1; i >= 0; i--) {
			if (acc.add(values[i])==TRUE)
				return TRUE;
		}
		return factory.accumulate(acc);
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.bool.Int#divide(kodkod.engine.bool.Int)
	 */
	@Override
	public Int divide(Int other) {
		validate(other);
		// Non-restoring signed division: Behrooz Parhami, Computer Arithmetic: Algorithms and Hardware Designs,
		// Oxford University Press, 2000, pp. 218-221.
		final int width = factory.bitwidth, extended = width*2 + 1;
		
		// extend the dividend to bitwidth*2 + 1 and store it in s; the quotient will have width digits  
		final BooleanValue[] s = extend(extended), q = new BooleanValue[width];
		
		BooleanValue carry, sbit, qbit, dbit;
		
		// the sign bit of the divisor
		final BooleanValue dMSB = other.bit(width);
		
		int sleft = 0; // the index which contains the LSB of s
		for(int i = 0; i < width; i++) {
			int sright = (sleft + extended - 1) % extended; // the index which contains the MSB of s
			// q[width-i-1] is 1 if sign(s_(i)) = sign(d), otherwise it is 0
			qbit = factory.iff(s[sright], dMSB);
			q[width-i-1] = qbit;
			// shift s to the left by 1 -- simulated by setting sright to FALSE and sleft to sright
			s[sright] = FALSE;
			sleft = sright;
			// if sign(s_(i)) = sign(d), form s_(i+1) by subtracting (2^width)d from s_(i);
			// otherwise, form s_(i+1) by adding (2^width)d to s_(i).
			carry = qbit;
			for(int di = 0, si = (sleft+width) % extended; di <= width; di++, si = (si+1) % extended) {
				dbit = factory.xor(qbit, other.bit(di));
				sbit = s[si];
				s[si] = factory.sum(sbit, dbit, carry);
				carry = factory.carry(sbit, dbit, carry);
			}
		}
	
		// shift q one bit to the left; and let the LSB position of q be 1
		System.arraycopy(q, 0, q, 1, width-1);
		
		// Check if the MSB of the final remainder and the dividend are the same, or 
		// the final remainder is zero; if so,
		// we are done.  Otherwise, correct q as follows:
		// if s and d have the same sign, q + 1 is the correct quotient,
		// else, q - 1 is the correct quotient.
		sbit = s[(sleft + extended - 1) % extended];
		final BooleanValue incorrect = factory.and(or(s, factory), factory.xor(sbit, bit(width)));
		final BooleanValue addOne = factory.and(incorrect, factory.xor(sbit, dMSB));
		
		q[0] = factory.sum(TRUE, incorrect, FALSE); // LSB position of q is 1
		carry = factory.carry(TRUE, incorrect, FALSE); // LSB position of q is 1
		
		for(int i = 1; i < width; i++) {
			qbit = q[i];
			q[i] = factory.sum(qbit, addOne, carry);
			carry = factory.carry(qbit, addOne, carry);
		}
			
		return new BinaryInt(factory, q);
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.bool.Int#choice(kodkod.engine.bool.BooleanValue, kodkod.engine.bool.Int)
	 */
	@Override
	public Int choice(BooleanValue condition, Int other) {
		validate(other);
		final int width = StrictMath.max(width(), other.width());
		final BooleanValue[] choice = new BooleanValue[width];
		for(int i = 0; i < width; i++) {
			choice[i] = factory.ite(condition, bit(i), other.bit(i));
		}
		return new BinaryInt(factory, choice);
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.bool.Int#and(kodkod.engine.bool.Int)
	 */
	@Override
	public Int and(Int other) {
		validate(other);
		final int width = StrictMath.min(width(), other.width());
		final BooleanValue[] and = new BooleanValue[width];
		for(int i = 0; i < width; i++) {
			and[i] = factory.and(bit(i), other.bit(i));
		}
		return new BinaryInt(factory, and);
	}

	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.bool.Int#or(kodkod.engine.bool.Int)
	 */
	@Override
	public Int or(Int other) {
		validate(other);
		final int width = StrictMath.max(width(), other.width());
		final BooleanValue[] or = new BooleanValue[width];
		for(int i = 0; i < width; i++) {
			or[i] = factory.or(bit(i), other.bit(i));
		}
		return new BinaryInt(factory, or);
	}

	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.bool.Int#xor(kodkod.engine.bool.Int)
	 */
	@Override
	public Int xor(Int other) {
		validate(other);
		final int width = StrictMath.max(width(), other.width());
		final BooleanValue[] xor = new BooleanValue[width];
		for(int i = 0; i < width; i++) {
			xor[i] = factory.xor(bit(i), other.bit(i));
		}
		return new BinaryInt(factory,xor);
	}

	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.bool.Int#shl(kodkod.engine.bool.Int)
	 */
	@Override
	public Int shl(Int other) {
		validate(other);
		final int width = factory.bitwidth;
		final BinaryInt shifted = new BinaryInt(factory, extend(width));
		final int max = StrictMath.min(32 - Integer.numberOfLeadingZeros(width - 1), other.width());
		for(int i = 0; i < max; i++) {
			int shift = 1 << i;
			BooleanValue bit = other.bit(i);
			for(int j = width-1; j >= 0; j--) {
				shifted.bits[j] = factory.ite(bit, j < shift ? FALSE : shifted.bit(j-shift), shifted.bits[j]);
			}
		}
		return shifted;
	}
	
	/**
	 * Performs a right shift with the given extension.
	 */
	private Int shr(Int other, BooleanValue sign) {
		validate(other);
		final int width = factory.bitwidth;
		final BinaryInt shifted = new BinaryInt(factory, extend(width));
		final int max = StrictMath.min(32 - Integer.numberOfLeadingZeros(width - 1), other.width());
		for(int i = 0; i < max; i++) {
			int shift = 1 << i;
			int fill = width - shift;
			BooleanValue bit = other.bit(i);
			for(int j = 0; j < width; j++) {
				shifted.bits[j] = factory.ite(bit, j < fill ? shifted.bit(j+shift) : sign, shifted.bits[j]);
			}
		}
		return shifted;
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.bool.Int#shr(kodkod.engine.bool.Int)
	 */
	@Override
	public Int shr(Int other) {
		return shr(other, FALSE);
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.bool.Int#sha(kodkod.engine.bool.Int)
	 */
	@Override
	public Int sha(Int other) {
		return shr(other, bits[bits.length-1]);
	}
	
	/**
	 * {@inheritDoc}
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return "b" + Arrays.toString(bits);
	}

	
}
