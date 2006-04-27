package kodkod.util.ints;

import java.util.Collection;
import java.util.Iterator;
import java.util.NoSuchElementException;

import kodkod.util.ints.IntRange.OnePointRange;
import kodkod.util.ints.IntRange.TwoPointRange;


/**
 * This class contains various utility methods for working with
 * integers, {@link kodkod.util.ints.IntRange IntRanges}, and 
 * {@link kodkod.util.ints.IntSet IntSets}.
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
			public IntIterator iterator(int from, int to) {	
				return new IntIterator() {
					public boolean hasNext() { return false; }
					public int nextInt() { throw new NoSuchElementException(); }
					public Integer next() { throw new NoSuchElementException();	}
					public void remove() { throw new UnsupportedOperationException(); }
				};
			}
			public int size() {	return 0; }
	};
	
	private static final int SHORT = 0x0000ffff;
	
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
		if (s==null) 
			throw new NullPointerException("s = null");
		else if (s instanceof UnmodifiableIntSet || s instanceof SingletonIntSet || s instanceof RangeIntSet)
			return s;
		else 
			return new UnmodifiableIntSet(s);
	}
	
	/**
	 * Returns an unmodifiable IntSet whose sole
	 * element is the given integer.
	 * @return {s: IntSet | s.ints = i}
	 */
	public static IntSet singleton(final int i) {
		return new SingletonIntSet(i);
	}
	
	/**
	 * Returns an unmodifiable IntSet that contains
	 * all the elements in the given range.
	 * @return {s: IntSet | s.ints = [range.min()..range.max()] }
	 */
	public static IntSet rangeSet(IntRange range) {
		if (range==null)
			throw new NullPointerException();
		return new RangeIntSet(range);
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
	
	/*-----------SEQUENCES-----------*/
	/**
	 * Returns an unmodifiable view of the specified sparse sequence. This method 
	 * allows modules to provide users with "read-only" access to internal sparse sequences.
	 * Query operations on the returned sequence "read through" to the specified sequence, and 
	 * attempts to modify the returned sequence, whether direct or via its iterator, result 
	 * in an UnsupportedOperationException.
	 * @return an unmodifiable view of s
	 * @throws NullPointerException - s = null
	 */
	public static <V> SparseSequence<V> unmodifiableSequence(SparseSequence<V> s) {
		if (s==null) 
			throw new NullPointerException();
		if (s instanceof UnmodifiableSparseSequence)
			return s;
		else return new UnmodifiableSparseSequence<V>(s);
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
	 * An implementation of an IntSet wrapper for an IntRange.
	 */
	private static final class RangeIntSet extends AbstractIntSet {
		private final IntRange range;
		/**
		 * Constructs an unmodifiable IntSet wrapper for a range.
		 */
		RangeIntSet(IntRange range) {
			this.range = range;
		}
		public boolean contains(int i) { return range.contains(i); }
		public int min() { return range.min(); }
		public int max() { return range.max(); }
		public IntIterator iterator(final int from, final int to) {
			return new IntIterator() {
				final boolean ascending = (from <= to);
				int cursor = ascending ? StrictMath.max(range.min(), from) : StrictMath.min(range.max(), from);
				final int end = ascending ? StrictMath.min(range.max(), to) : StrictMath.max(range.min(), to);
				public boolean hasNext() {
					return ascending && cursor<=end || !ascending && cursor >= end;
				}
				public int nextInt() {
					if (!hasNext()) throw new NoSuchElementException();
					return ascending ? cursor++ : cursor--;
				}
				public Integer next() { return nextInt(); }
				public void remove() { throw new UnsupportedOperationException(); }
				
			};
		}
		public int size() {	return range.size(); }
		public IntSet copy() { return this; }
	}
	
	/**
	 * An implementation of an IntSet wrapper for a single integer.
	 */
	private static final class SingletonIntSet extends AbstractIntSet {
		private final int i;
		/**
		 * Constructs an unmodifiable intset wrapper for the given integer.
		 */
		SingletonIntSet(int i) {
			this.i = i;
		}
		public boolean contains(int j) { return i==j; }
		public int min() { return i; }
		public int max() { return i; }
		public IntIterator iterator(final int from, final int to) {	
			return new IntIterator() {
				boolean cursor = (from<=i && i<=to) || (to<=i && i<=from);
				public boolean hasNext() { return cursor; }
				public int nextInt() { 
					if (!hasNext()) throw new NoSuchElementException(); 
					cursor = false;
					return i;
				}
				public Integer next() { return nextInt();	}
				public void remove() { throw new UnsupportedOperationException(); }
			};
		}
		public int size() {	return 1; }
		public IntSet copy() { return this; }
		public boolean equals(Object o) {
			if (this==o) return true;
			else if (o instanceof IntSet) {
				final IntSet s = (IntSet) o;
				return s.size()==1 && s.min()==i;
			} else
				return super.equals(o);
		}
		public int hashCode() { return i; }
	}
	
	/**
	 * An implementation of an unmodifiable IntSet view.
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
		public boolean equals(Object o) {
			if (this==o) return true;
			else if (o instanceof UnmodifiableIntSet) {
				return s.equals(((UnmodifiableIntSet) o).s);
			} else return s.equals(o); 	
		}
		public int hashCode() { 	return s.hashCode();	}	
	}
	
	/**
	 * An implementation of an unmodifiable SparseSequence view.
	 * @author Emina Torlak
	 */
	private static final class UnmodifiableSparseSequence<V> extends AbstractSparseSequence<V> {
		private final SparseSequence<V> s;
		
		UnmodifiableSparseSequence(SparseSequence<V> s) {
			this.s = s;
		}
		
		@Override
		public Iterator<IndexedEntry<V>> iterator(final int from, final int to) {
			return new Iterator<IndexedEntry<V>>() {
				Iterator<IndexedEntry<V>> iter = s.iterator(from, to);
				public boolean hasNext() {
					return iter.hasNext();
				}

				public IndexedEntry<V> next() {
					return iter.next();
				}

				public void remove() {
					throw new UnsupportedOperationException();
				}
				
			};
		}

		public int size() {
			return s.size();
		}

		public void clear() {
			throw new UnsupportedOperationException();
		}

		public V put(int index, V value) {
			throw new UnsupportedOperationException();
		}

		public V get(int index) {
			return s.get(index);
		}

		public V remove(int index) {
			throw new UnsupportedOperationException();
		}

		public boolean containsIndex(int index) {
			return s.containsIndex(index);
		}

		public boolean contains(Object value) {
			return s.contains(value);
		}

		public IndexedEntry<V> first() {
			return s.first();
		}

		public IndexedEntry<V> last() {
			return s.last();
		}

		public IndexedEntry<V> successor(int index) {
			return s.successor(index);
		}

		public IndexedEntry<V> predecessor(int index) {
			return s.predecessor(index);
		}

		public IndexedEntry<V> ceil(int index) {
			return s.ceil(index);
		}

		public IndexedEntry<V> floor(int index) {
			return s.floor(index);
		}
		
	}
}
