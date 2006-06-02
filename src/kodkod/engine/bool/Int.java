package kodkod.engine.bool;

import static kodkod.engine.bool.BooleanConstant.FALSE;
import static kodkod.engine.bool.BooleanConstant.TRUE;
import static kodkod.engine.bool.Operator.AND;
import static kodkod.engine.bool.Operator.OR;

import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;

/**
* A string of {@link kodkod.engine.bool.BooleanValue boolean values}
* representing an integer in {@link Int.Encoding#UNARY}, 
* {@link Int.Encoding#BINARY}, or {@link Int.Encoding#TWOS_COMPLEMENT} form.
* 
* @specfield factory: BooleanFactory
* @specfield width: [1..)
* @specfield bits: [0..width) -> one factory.components
* @specfield encoding: Encoding
* @author Emina Torlak
*/
public final class Int {
	private final BooleanFactory factory;
	private final BooleanValue[] bits;
	private final Encoding encoding;
	
	/**
	 * Constructs an Int that encodes the given number using the specified
	 * encoding, bits, factory.
	 * @effects this.factory' = factory && this.encoding' = encoding && this.bits' = bits
	 */
	private Int(BooleanFactory factory, Encoding encoding, BooleanValue[] bits) {
		this.factory = factory;
		this.encoding = encoding;
		this.bits = bits;
	}
	
	/**
	 * Constructs an Int that encodes the given number using the specified
	 * encoding and factory.
	 * @effects this.factory' = factory && this.encoding' = encoding && [[this]] = number
	 * @requires factory != null && encoding != null
	 * @throws IllegalArgumentException - number < 0 && encoding in {BINARY, UNARY}
	 */
	Int(BooleanFactory factory, Encoding encoding, int number) {
		this.factory = factory;
		this.encoding = encoding;
		this.bits = encoding.encode(number);
	}
	
	/**
	 * Constructs an Int that encodes the sum of the values in the given collection, using the specified
	 * encoding and factory.
	 * @effects this.factory' = factory && this.encoding' = encoding && [[this]] = sum({v: elements[int] | if [[v]] then 1 else 0})
	 * @requires factory != null && encoding != null && bits != null 
	 * @requires no bits.elemements & FALSE && bits.elements in factory.components
	 */
	Int(BooleanFactory factory, Encoding encoding, Collection<BooleanValue> bits) {
		this.factory = factory;
		this.encoding = encoding;
		this.bits = sum(factory, encoding, bits.iterator(), 0, bits.size()-1);
	}

	/**
	 * Returns an array of boolean values that represents the sum of the elements returned by the iterator,
	 * using the given encoding.
	 * @param lo the first element of the current partial sum. Initial should be 0.
	 * @param hi the last element of the current partial sum.  Initial should be size-1, where size is the total
	 * number of elements returned by the iterator.
	 * @return an array of boolean values that represents the sum of the elements returned by the iterator,
	 * using the given encoding.
	 */
	private static BooleanValue[] sum(BooleanFactory factory, Encoding encoding, Iterator<BooleanValue> values, int lo, int hi) {
		if (lo > hi) 
			return encoding.encode(0);
		else if (lo == hi) 
			return encoding.encode(values.next());
		else {
			final int mid = (lo + hi) / 2;
			final BooleanValue[] lsum = sum(factory, encoding, values, lo, mid);
			final BooleanValue[] hsum = sum(factory, encoding, values, mid+1, hi);
			return encoding.plus(factory, lsum, hsum);
		}
	}
	
	/**
	 * Returns this.factory
	 * @return this.factory
	 */
	public final BooleanFactory factory() { return factory; }
	
	/**
	 * Returns the number of bits in the minimal representation of this Int,
	 * including sign bits (if any).
	 * @return this.width
	 */
	public final int width() { return bits.length; }
	
	/**
	 * Returns this.encoding.
	 * @return this.encoding
	 */
	public Encoding encoding() { return encoding; }
	
	/**
	 * Returns a BooleanValue encoding the comparator circuit
	 * that checks whether the integer represented by this
	 * Int is equal to the integer represented by the specified Int.
	 * @requires this.factory = other.factory && this.encoding = other.encoding
	 * @return BooleanValue encoding the comparator circuit
	 * that checks whether the integer represented by this
	 * Int is equal to the integer represented by the specified Int
	 * @throws IllegalArgumentException - this.factory != other.factory || this.encoding = other.encoding
	 */
	public BooleanValue eq(Int other) {
		validate(other);
		final BooleanAccumulator cmp = BooleanAccumulator.treeGate(AND);
		for(int i = 0, width = StrictMath.max(width(), other.width()); i < width; i++) {
			if (cmp.add(factory.iff(encoding.get(bits,i), encoding.get(other.bits,i)))==FALSE)
				return FALSE;
		}
		return factory.fastAdopt(cmp);
	}
	
	/**
	 * Returns a BooleanValue encoding the comparator circuit
	 * that checks whether the integer represented by this
	 * Int is less than or equal to the integer
	 * represented by the specified Int
	 * @requires this.factory = other.factory && this.encoding = other.encoding
	 * @return BooleanValue encoding the comparator circuit
	 * that checks whether the integer represented by this
	 * Int is less than or equal to the integer
	 * represented by the specified Int
	 * @throws IllegalArgumentException - this.factory != other.factory || this.encoding = other.encoding
	 */
	public BooleanValue lte(Int other) {
		validate(other);
		return encoding.lte(factory, bits, other.bits);
	}
	
	/**
	 * Returns a BooleanValue encoding the comparator circuit
	 * that checks whether the integer represented by this
	 * Int is less than the integer
	 * represented by the specified Int.
	 * @requires this.factory = other.factory && this.encoding = other.encoding
	 * @return BooleanValue encoding the comparator circuit
	 * that checks whether the integer represented by this
	 * Int is less than the integer
	 * represented by the specified Int
	 * @throws IllegalArgumentException - this.factory != other.factory || this.encoding = other.encoding
	 */
	public BooleanValue lt(Int other) {
		final BooleanValue leq = lte(other);
		final BooleanAccumulator acc = BooleanAccumulator.treeGate(OR);
		for(int i = 0, width = StrictMath.max(width(), other.width()); i < width; i++) {
			acc.add(factory.fastCompose(AND, encoding.get(bits, i).negation(), encoding.get(other.bits, i)));
		}
		return factory.fastCompose(AND, leq, factory.fastAdopt(acc));
	}
	
	/**
	 * Returns a BooleanValue encoding the comparator circuit
	 * that checks whether the integer represented by this
	 * Int is greater than or equal to the integer
	 * represented by the specified Int.
	 * @requires this.factory = other.factory && this.encoding = other.encoding
	 * @return BooleanValue encoding the comparator circuit
	 * that checks whether the integer represented by this
	 * Int is greater than or equal to the integer
	 * represented by the specified Int
	 * @throws IllegalArgumentException - this.factory != other.factory || this.encoding = other.encoding
	 */
	public BooleanValue gte(Int other) {
		return other.lte(this);
	}
	
	/**
	 * Returns a BooleanValue encoding the comparator circuit
	 * that checks whether the integer represented by this
	 * Int is greater than the integer
	 * represented by the specified Int.
	 * @requires this.factory = other.factory && this.encoding = other.encoding
	 * @return BooleanValue encoding the comparator circuit
	 * that checks whether the integer represented by this
	 * Int is greater than the integer
	 * represented by the specified Int
	 * @throws IllegalArgumentException - this.factory != other.factory || this.encoding = other.encoding
	 */
	public BooleanValue gt(Int other) {
		return other.lt(this);
	}
	
	/**
	 * Returns an Int that represents the sum of this and the given Int.
	 * @requires this.factory = other.factory && this.encoding = other.encoding
	 * @return an Int that represents the sum of this and the given Int
	 * @throws IllegalArgumentException - this.factory != other.factory || this.encoding = other.encoding
	 */
	public Int plus(Int other) {
		validate(other);
		return new Int(factory, encoding, encoding.plus(factory, bits, other.bits));
	}
	
	
	/**
	 * @throws IllegalArgumentException - other.factory != this.factory || other.encoding != this.encoding
	 */
	private final void validate(Int other) {
		if (other.factory != factory || other.encoding != this.encoding)
			throw new IllegalArgumentException(other.toString());
	}
	
	/**
	 * {@inheritDoc}
	 * @see java.lang.Object#toString()
	 */
	public String toString() { 
		return encoding + Arrays.toString(bits);
	}
	
	/**
	 * Available integer encodings.
	 */
	public static enum Encoding {
		/**
		 * Unary encoding of integers permits comparisons and
		 * addition of non-negative numbers.
		 */
		UNARY {
		
			BooleanValue[] encode(int number) {
				if (number < 0) throw new IllegalArgumentException("number < 0: " + number);
				final BooleanValue[] bits = new BooleanValue[number];
				for(int i = 0; i < number; i++) {
					bits[i] = TRUE;
				}
				return bits;
			}
			
			BooleanValue lte(BooleanFactory factory, BooleanValue[] bits0, BooleanValue[] bits1) {
				final BooleanAccumulator cmp = BooleanAccumulator.treeGate(Operator.AND);
				for(int i = 0, width = StrictMath.max(bits0.length, bits1.length); i < width; i++) {
					cmp.add(factory.implies(get(bits0, i), get(bits1, i)));
				}
				return factory.fastAdopt(cmp);
			}
			
			BooleanValue[] plus(BooleanFactory factory, BooleanValue[] bits0, BooleanValue[] bits1) {
				final int width = bits0.length + bits1.length;
				final BooleanValue[] plus = new BooleanValue[width];
				for(int i = 0; i < width; i++) {
					BooleanAccumulator acc = BooleanAccumulator.treeGate(OR);
					acc.add(get(bits0,i)); 
					acc.add(get(bits1, i));
					for(int j = 0; j < i; j++) {
						acc.add(factory.fastCompose(AND, get(bits0, j), get(bits1, i - 1 - j)));
					}
					plus[i] = factory.fastAdopt(acc);
				}
				return plus;
			}
		
		},
		
		/**
		 * (Unsigned) binary encoding of integers permits comparisons
		 * and addition of non-negative numbers.
		 */
		BINARY {
		
			BooleanValue[] encode(int number) {
				if (number < 0) throw new IllegalArgumentException("number < 0: " + number);
				final int width = 32 - Integer.numberOfLeadingZeros(number);
				final BooleanValue[] bits = new BooleanValue[width];
				for(int i = 0; i < width; i++) {
					bits[i] = (number & (1<<i)) == 0 ? FALSE : TRUE;
				}
				return bits;
			}
			
			BooleanValue lte(BooleanFactory factory, BooleanValue[] bits0, BooleanValue[] bits1) {
				final BooleanAccumulator cmp = BooleanAccumulator.treeGate(Operator.AND);
				BooleanValue prevEquals = TRUE;
				for(int i = StrictMath.max(bits0.length, bits1.length)-1; i >= 0; i--) {
					BooleanValue v0 = get(bits0, i), v1 = get(bits1, i);
					cmp.add(factory.implies(prevEquals, factory.implies(v0, v1)));
					prevEquals = factory.and(prevEquals, factory.iff(v0, v1));
				}
				return factory.fastAdopt(cmp);
			}
			
			BooleanValue[] plus(BooleanFactory factory, BooleanValue[] bits0, BooleanValue[] bits1) {
				final int width = 32 - Integer.numberOfLeadingZeros((1<<bits0.length)+(1<<bits1.length));
				final BooleanValue[] plus = new BooleanValue[width];
				BooleanValue carry = FALSE;
				for(int i = 0; i < width; i++) {
					BooleanValue v0 = get(bits0,i), v1 = get(bits1, i);
					plus[i] = factory.xor(carry, factory.xor(v0, v1));
					carry = factory.fastCompose(OR, factory.fastCompose(AND, v0, v1), factory.fastCompose(AND, carry, factory.fastCompose(OR, v0, v1)));
				}
				return plus;
			}
		},
		/**
		 * Two's-complement encoding of integers permits
		 * comparisons, addition, and subtraction.
		 */
		TWOS_COMPLEMENT {
	
			BooleanValue[] encode(int number) {
				final int width = 33 - Integer.numberOfLeadingZeros(number);
				final BooleanValue[] bits = new BooleanValue[width];
				for(int i = 0; i < width; i++) {
					bits[i] = (number & (1<<i)) == 0 ? FALSE : TRUE;
				}
				return bits;
			}
			
			BooleanValue[] encode(BooleanValue bit) {
				return new BooleanValue[] {bit, FALSE};
			}
			
			BooleanValue get(BooleanValue[] bits, int index) {
				return bits[StrictMath.min(index, bits.length-1)];
			}
			
			BooleanValue lte(BooleanFactory factory, BooleanValue[] bits0, BooleanValue[] bits1) {
				final BooleanAccumulator cmp = BooleanAccumulator.treeGate(Operator.AND);
				final int last = StrictMath.max(bits0.length, bits1.length)-1;
				cmp.add(factory.implies(get(bits1,last), get(bits0,last)));
				BooleanValue prevEquals = factory.iff(get(bits0,last), get(bits1,last));
				for(int i = last-1; i >= 0; i--) {
					BooleanValue v0 = get(bits0,i), v1 = get(bits1,i);
					cmp.add(factory.implies(prevEquals, factory.implies(v0, v1)));
					prevEquals = factory.and(prevEquals, factory.iff(v0, v1));
				}
				return factory.fastAdopt(cmp);
			}
						
			BooleanValue[] plus(BooleanFactory factory, BooleanValue[] bits0, BooleanValue[] bits1) {
				final int width = 33 - Integer.numberOfLeadingZeros((1<<(bits0.length-1))+(1<<(bits1.length-1)));
				final BooleanValue[] plus = new BooleanValue[width];
				BooleanValue carry = FALSE;
				for(int i = 0; i < width; i++) {
					BooleanValue v0 = get(bits0,i), v1 = get(bits1, i);
					plus[i] = factory.xor(carry, factory.xor(v0, v1));
					carry = factory.fastCompose(OR, factory.fastCompose(AND, v0, v1), factory.fastCompose(AND, carry, factory.fastCompose(OR, v0, v1)));
				}
				return plus;
			}
		};
		
		/**
		 * Encodes the given number into an array of boolean values,
		 * using this encoding.
		 * @return an array of boolean values that represents the given number in this encoding
		 * @requires this != TWOS_COMPLEMENT => number >= 0
		 */
		abstract BooleanValue[] encode(int number);
		
		/**
		 * Returns an array that represents 1 if the meaning of bit is TRUE and
		 * 0 otherwise, using this encoding.
		 * @return an array that represents 1 if the meaning of bit is TRUE and
		 * 0 otherwise, using this encoding.
		 */
		BooleanValue[] encode(BooleanValue bit) {
			return new BooleanValue[] {bit};
		}
		
		/**
		 * Returns the value at the specified index in the array, if index < bits.length.  
		 * Otherwise returns a default value; this implementation returns FALSE.
		 * @return this.bits[index]
		 * @throws ArrayIndexOutOfBoundsException - index < 0 
		 */
		BooleanValue get(BooleanValue[] bits, int index) {
			return index < bits.length ? bits[index] : FALSE;
		}
		
		/**
		 * Returns a BooleanValue encoding the comparator circuit
		 * that checks whether the integer represented by bits0 is 
		 * less than or equal to the integer represented by bits1.
		 * @requires bits0[int] + bits1[int] in factory.components
		 * @return BooleanValue encoding the comparator circuit
		 * that checks whether the integer represented by bits0 is 
		 * less than or equal to the integer represented by bits1.
		 */
		abstract BooleanValue lte(BooleanFactory factory, BooleanValue[] bits0, BooleanValue[] bits1);
		
		/**
		 * Returns an array of BooleanValues that encodes the sum of
		 * the integers represented by the given arrays using this
		 * encoding.  The default implementation treats the bits as 
		 * though they were binary numbers.
		 * @requires bits0[int] + bits1[int] in factory.components
		 * @return an array of BooleanValues that encodes the sum of
		 * the integers represented by the given arrays using this
		 * encoding.
		 */
		abstract BooleanValue[] plus(BooleanFactory factory, BooleanValue[] bits0, BooleanValue[] bits1);
	}	
}
