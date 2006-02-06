package kodkod.util;

import java.util.Collection;
import java.util.NoSuchElementException;

import kodkod.util.IntRange.OnePointRange;
import kodkod.util.IntRange.TwoPointRange;


/**
 * This class contains various utility methods for working with
 * integers, {@link kodkod.util.IntRange IntRanges}, and 
 * {@link kodkod.util.IntSet IntSets}.
 * 
 * The methods in this class all throw a NullPointerException if 
 * given a null reference unless otherwise specified.
 * 
 * @author Emina Torlak
 */
public final class Ints {
	/** An unmodifiable empty int set. */
	public static final IntSet EMPTY_SET = 
		new AbstractIntSet() {
			public boolean contains(int i) { return false; }
			public int min() { throw new NoSuchElementException(); }
			public int max() { throw new NoSuchElementException(); }
//			public int predecessor(int i) { throw new NoSuchElementException(); }
//			public int successor(int i) { throw new NoSuchElementException(); }
			public IntIterator iterator(int from, int to) {	
				return new IntIterator() {
					public boolean hasNext() { return false; }
					public int nextInt() { throw new NoSuchElementException(); }
					public Integer next() { throw new NoSuchElementException();	}
					public void remove() { throw new UnsupportedOperationException(); }
				};
			}
			public int size() {	return 0; }
			public IntSet copy() { return EMPTY_SET; }
	};
	
	private static final int SHORT = 0x0000ffff;
//	private static final int BYTE  = 0x000000ff;
	
	private Ints() {}

	/*-----------SETS AND RANGES-----------*/
	/**
	 * Returns an integer range from min, inclusive, to max, inclusive.
	 * @return { r: IntRange | r.min = min && r.max = max }
	 * @throws IllegalArgumentException - min > max
	 */
	public static IntRange range(int min, int max) {
		if (min < max) return new TwoPointRange(min,max);
		else if (min==max) return new OnePointRange(min);
		else throw new IllegalArgumentException("min > max");
	}
	
	/**
	 * Returns the smallest IntRange r that contains
	 * both r1 and r2.
	 * @return { r: IntRange | r.contains(r1) && r.contains(r2) && 
	 *           no r' : IntRange - r | r'.contains(r1) && r'.contains(r2) && r'.size() < r.size() }
	 * @throws NullPointerException - range = null
	 */
	public static IntRange merge(IntRange r1, IntRange r2) {
		if (r1.contains(r2)) return r1;
		else if (r2.contains(r1)) return r2;
		else return range(StrictMath.min(r1.min(),r2.min()), StrictMath.max(r1.max(), r2.max()));
	}
	
	/**
	 * Returns an unmodifiable view of the specified set. This method 
	 * allows modules to provide users with "read-only" access to internal int sets.
	 * Query operations on the returned set "read through" to the specified set, and 
	 * attempts to modify the returned set, whether direct or via its iterator, result 
	 * in an UnsupportedOperationException.
	 * @return an unmodifiable view of s
	 * @throws NullPointerException - s = null
	 */
	public static IntSet unmodifiableIntSet(final IntSet s) {
		if (s==null) throw new NullPointerException("s = null");
		return (s instanceof UnmodifiableIntSet) ? s : new UnmodifiableIntSet(s);
	}
	
	/**
	 * Returns an implementation of the int set interface
	 * that offers the best time/space trade-off for a 
	 * set that can store all elements in the half open
	 * range [0..max).  The returned instance may or may
	 * not admit elements out of the range [0..max).
	 * @return an int set that can store at least the 
	 * elements in [0..max).
	 */
	public static IntSet bestSet(int max) {
		// cut-off for using a bit map is 256
		return max > 256 ? new IntTreeSet() : new IntBitSet(max);
	}

	/**
	 * Returns an implementation of the int set interface
	 * that offers the best time/space trade-off for a 
	 * set that can store all elements in the closed range [min..max].  
	 * The returned instance may or may not admit elements 
	 * out of the specified range.
	 * @return an int set that can store at least the 
	 * elements in the given range.
	 * @throws IllegalArgumentException - min > max
	 */
	public static IntSet bestSet(int min, int max) {
		if (min > max) throw new IllegalArgumentException("min > max");
		return min < 0 ? new IntTreeSet() : bestSet(max+1);
	}
	
	/*-----------INTEGER MANIPULATION-----------*/
	
	/**
	 * An implementation of Paul Hsieh's hashing function, 
	 * described at http://www.azillionmonkeys.com/qed/hash.html.
	 * The method returns a 32 bit hash of the given integer.
	 * This function is very fast and collision resistent; e.g. 
	 * it hashes the four million integers in the range 
	 * [-2000000,...-1, 1,..., 2000000] to distinct values.
	 * @return a 32 bit hash of the given integer
	 */
	public static int superFastHash(int key) {
		int hash = 4; // the key has four bytes

		hash += key >>> 16;
		int tmp = ((key & SHORT) << 11) ^ hash;
		hash = (hash << 16) ^ tmp;
		hash += hash >> 11;
		
		// no end cases since we have exactly 4 bytes
		 hash ^= hash << 3;
		 hash += hash >> 5;
		 hash ^= hash << 2;
		 hash += hash >> 15;
		 hash ^= hash << 10;
		
		return hash;
	}
	
	/**
	 * An implementation of Paul Hsieh's hashing function, 
	 * described at http://www.azillionmonkeys.com/qed/hash.html.
	 * The method returns a 32 bit hash of the given integers,
	 * or 0 if the array is empty.
	 * @return a 32 bit hash of the given integers
	 */
	public static int superFastHash(int... key) {
		if (key.length==0) return 0;
		int hash = key.length << 2;

		for(int word : key) {
			hash += word >>> 16;
			int tmp = ((word & SHORT) << 11) ^ hash;
			hash = (hash << 16) ^ tmp;
			hash += hash >> 11;
		}
		// no end cases as we are dealing with ints
		 hash ^= hash << 3;
		 hash += hash >> 5;
		 hash ^= hash << 2;
		 hash += hash >> 15;
		 hash ^= hash << 10;
		
		return hash;
	}
	
	/**
	 * An implementation of Paul Hsieh's hashing function, 
	 * described at http://www.azillionmonkeys.com/qed/hash.html.
	 * The method returns a 32 bit hash of the given objects' hash codes,
	 * or zero if the array is empty.  Any null references in the array
	 * are taken to have 0 as their hash code value.
	 * @return a 32 bit hash of the given objects' hashCodes
	 */
	public static int superFastHash(Object... key) {
		if (key.length==0) return 0;
		int hash = key.length << 2; 

		for(Object o : key) {
			int word = (o == null ? 0 : o.hashCode());
			hash += word >>> 16;
			int tmp = ((word & SHORT) << 11) ^ hash;
			hash = (hash << 16) ^ tmp;
			hash += hash >> 11;
		}
		// no end cases as we are dealing with ints
		 hash ^= hash << 3;
		 hash += hash >> 5;
		 hash ^= hash << 2;
		 hash += hash >> 15;
		 hash ^= hash << 10;
		
		return hash;
	}
	
	/**
	 * An implementation of an unmodifiable view wrapper for an IntSet.
	 * @author Emina Torlak
	 */
	private static final class UnmodifiableIntSet extends AbstractIntSet {
		private final IntSet s;
		
		/**
		 * Constructs an unmodifiable wrapper for the given intset.
		 * @requires set != null
		 */
		UnmodifiableIntSet(IntSet set) {
			this.s = set;
		}
		public int size() { return s.size(); }
		public boolean contains(int i) { return s.contains(i); }
		public int min() { return s.min();	}
		public int max() { return s.max();	}
//		public int predecessor(int i) { return s.predecessor(i); }
//		public int successor(int i) { return s.successor(i); }
		public boolean containsAll(Collection<?> c) { return s.containsAll(c); }	
		public IntIterator iterator(final int from, final int to) { 	
			return new IntIterator() {
				IntIterator iter = s.iterator(from,to);
				public boolean hasNext() { return iter.hasNext(); }
				public int nextInt() { return iter.nextInt(); }
				public Integer next() { return iter.next(); }
				public void remove() {
					throw new UnsupportedOperationException();
				}	
			};
		}
		public IntSet copy() { return s.copy(); }
		public boolean equals(Object o) {
			if (this==o) return true;
			else if (o instanceof UnmodifiableIntSet) {
				return s.equals(((UnmodifiableIntSet) o).s);
			} else return s.equals(o); 	
		}
		public int hashCode() { 	return s.hashCode();	}	
	}
	
	public static void main(String[] args) {

	}
	
}
