/* 
 * Kodkod -- Copyright (c) 2005-2007, Emina Torlak
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
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
	 * Constructs a UnaryInt that represents either 0 or the given number, depending on 
	 * the value of the given bit.
	 * @requires factory.encoding = UNARY && number >= 0 && bit in factory.components  
	 * @effects this.factory' = factory
	 * @effects bits is a unary representation of the given number
	 * that uses the provided bit in place of 1's
	 */
	UnaryInt(BooleanFactory factory, int number, BooleanValue bit) {
		super(factory);
		assert number >= 0;
		final int width = StrictMath.min(number, factory.bitwidth);
		this.bits = new BooleanValue[width];
		for(int i = 0; i < width; i++) 
			bits[i] = bit;
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
		for(BooleanValue bit : bits) {
			if (bit==TRUE) ret++;
			else if (bit!=FALSE)
				throw new IllegalStateException(this + " is not constant");
		}
		return ret;
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
	 * Unsupported.
	 * @throws UnsupportedOperationException
	 */
	@Override
	public Int multiply(Int other) {
		throw new UnsupportedOperationException();
	}

	/**
	 * Unsupported.
	 * @throws UnsupportedOperationException
	 */
	@Override
	public Int divide(Int other) {
		throw new UnsupportedOperationException();
	}
	
	/**
	 * Unsupported.
	 * @throws UnsupportedOperationException
	 */
	@Override
	public Int modulo(Int other) {
		throw new UnsupportedOperationException();
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
		return new UnaryInt(factory, choice);
	}
	
	/**
	 * Unsupported.
	 * @throws UnsupportedOperationException
	 */
	@Override
	public Int negate() { 
		throw new UnsupportedOperationException();
	}
	
	/**
	 * Unsupported.
	 * @throws UnsupportedOperationException
	 */
	@Override
	public Int not() {
		throw new UnsupportedOperationException();
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.bool.Int#abs()
	 */
	@Override
	public Int abs() { 
		return this;
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.bool.Int#sgn()
	 */
	@Override
	public Int sgn() { 
		return new UnaryInt(factory, new BooleanValue[]{bit(0)});
	}
	
	/**
	 * {@inheritDoc}
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return "u"+Arrays.toString(bits);
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
		return new UnaryInt(factory, and);
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
		return new UnaryInt(factory, or);
	}

	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.bool.Int#xor(kodkod.engine.bool.Int)
	 */
	@Override
	public Int xor(Int other) {
		throw new UnsupportedOperationException();
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.bool.Int#shl(kodkod.engine.bool.Int)
	 */
	@Override
	public Int shl(Int other) {
		throw new UnsupportedOperationException();
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.bool.Int#shr(kodkod.engine.bool.Int)
	 */
	@Override
	public Int shr(Int other) {
		throw new UnsupportedOperationException();
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.bool.Int#sha(kodkod.engine.bool.Int)
	 */
	@Override
	public Int sha(Int other) {
		throw new UnsupportedOperationException();
	}

}


