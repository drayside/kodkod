package kodkod.util.collections;

import java.util.AbstractSet;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Set;

/**
 * This class provides utility methods for constructing
 * iterators over arrays and convenience wrappers for 
 * java.util.Arrays sorting and searching procedures.
 * 
 * @author Emina Torlak
 */
public final class Arrays {
	private static Comparator<Object> identityComparator;
	
	private Arrays() {}	
	/**
	 * Returns a new iterator over the given array of items.
	 * The iterator is backed by the given array.  The contents
	 * of the array are not modified by the iterator.  The effect
	 * of this method is the same as calling Iterators.iterator(0, items.length, items).
	 * @throws NullPointerException - items = null
	 */
	public static final <T, E extends T> Iterator<T> iterate(final E... items) {
		return new AscendingArrayIterator<T>(0, items.length, items);
	}
	
	/**
	 * Returns a new iterator over the given array of items.
	 * The iterator is backed by the given array.  The contents
	 * of the array are not modified by the iterator.
	 * The returned iterator enumerates the items located between
	 * indeces start, inclusive, and end, exclusive.  If start < end,
	 * the elements are returned in the ascending order; otherwise,
	 * they are returned in the descending order.
	 * @throws NullPointerException - items = null
	 * @throws IllegalArgumentException - start < end && (start < 0 || end > items.length) ||
	 *                                    start > end && (start >= items.length || end < -1)
	 */
	@SuppressWarnings("unchecked")
	public static final <T, E extends T> Iterator<T> iterate(int start, int end, final E... items) {
		if (start < end)
			return new AscendingArrayIterator<T>(start,end,items);
		else if (start > end)
			return new DescendingArrayIterator<T>(start,end,items);
		else 
			return emptyIterator();
	}
	
	/**
	 * Returns an iterator that has no elements.  That is, 
	 * calls to hasNext will return false, and all other
	 * calls will result in a runtime exception.
	 * @return an empty iterator
	 */
	@SuppressWarnings("unchecked")
	public static final <T> Iterator<T> emptyIterator() {
		return (Iterator<T>) Collections.emptySet().iterator();
	}
	
	/**
	 * Returns a comparator that compares objects according to their
	 * {@link System#identityHashCode(Object) identity hashcodes}.
	 * @return a comparator that compares objects according to their 
	 * {@link System#identityHashCode(Object) identity hashcodes}.
	 */
	public static final Comparator<Object> identityComparator() {
		if (identityComparator==null) {
			identityComparator = new Comparator<Object>() {
				public int compare(Object o1, Object o2) {
					final int c1 = System.identityHashCode(o1);
					final int c2 = System.identityHashCode(o2);
					return c1 == c2 ? 0 : (c1 < c2 ? -1 : 1);
				}
			};
		}
		return identityComparator;
	}
	
	
	/**
	 * Calls {@link java.util.Arrays#sort(Object[], Comparator)} on the 
	 * given array and returns it.  The elements are sorted in the ascending
	 * order of their identity hashcodes.
	 * @effects java.util.Arrays.sort(array, {@link #identityComparator()}) 
	 * @return the given array, with its elements sorted in the increasing order of identity hashcodes
	 */
	public static final <T> T[] identitySort(T[] array) {
		java.util.Arrays.sort(array, identityComparator());
		return array;
	}
	
	/**
	 * Searches the specified array for the specified object using the binary search algorithm 
	 * and reference equality. 
	 * The array must be sorted into ascending order according to the identity hashcodes of its elements 
	 * (as by {@link #identitySort(Object[])}) prior to making this call. If it is not sorted, 
	 * the results are undefined.  If the array contains multiple occurences of the specified object, 
	 * there is no guarantee which one will be found.
	 * @requires all i, j: [0..array.length) | i < j => System.identityHashCode(array[i]) <= System.identityHashCode(array[j])
	 * @return index of the search key, if it is contained in the array; otherwise, (-(insertion point) - 1). 
	 * The insertion point is defined as the point at which the key would be inserted into the array: the 
	 * index of the first element greater than the key, or array.length, if all elements in the array are less 
	 * than the specified key. Note that this guarantees that the return value will be >= 0 if and only if the 
	 * key is found.
	 */
	public static final int identityBinarySearch(Object[] array, Object key) {
		int low = 0;
		int high = array.length-1;
		int index = System.identityHashCode(key);

		while (low <= high) {
			int mid = (low + high) >>> 1;
			int midIndex = System.identityHashCode(array[mid]);		
			if (midIndex < index)
				low = mid + 1;
			else if (midIndex > index)
				high = mid - 1;
			else { // index found, now check that variables are the same
				if (array[mid]==key) return mid;
				// check all variables with the same index (if any)
				for(int i = mid+1; i < array.length && System.identityHashCode(array[i])==index; i++) {
					if (array[i]==key) return i;
				}
				for(int i = mid-1; i >= 0 && System.identityHashCode(array[i])==index; i--) {
					if (array[i]==key) return i;
				}
				return -(mid+1); // var not found
			}
		}

		return -(low + 1);  // key not found.
	}
	
	/**
	 * Returns an identity set backed by the given array.
	 * The array must contain no duplicates, its elements must be
	 * sorted in the increasing order of identity hashcodes (as by {@link #identitySort(Object[])}), and its contents
	 * must not be changed while it is in use by the returned set.
	 * @requires all i, j: [0..array.length) | i < j => System.identityHashCode(array[i]) <= System.identityHashCode(array[j])
	 * @return an unmodifiable identity Set view of the given array
	 */
	public static final <T> Set<T> asIdentitySet(final T[] array) {
		return new AbstractSet<T>() {
			public boolean contains(Object o) {
				return identityBinarySearch(array, o) >= 0;
			}
			public Iterator<T> iterator() {	return iterate(array); }
			public int size() { return array.length;	}		
			public int hashCode() {
				int result = 0;
		        for (Object o : array) { result += System.identityHashCode(o); }
		        return result; 
		     }
		};
	}
	
	/**
	 * An unmodifying iterator over an array.
	 */
	private static abstract class ArrayIterator<T> implements Iterator<T> {
		final T[] items;
		final int end;
		int cursor;
		
		/**
		 * Constructs a new iterator over the given array of items.
		 * The iterator is backed by the given array.  The contents
		 * of the array are not modified by the iterator.  The 
		 * constructed iterator returns the items located between the 
		 * indeces start, inclusive, and end, exclusive.
		 * @requires items != null && 
		 *           start < end => end in [0..items.length] && start in [0..end], 
		 *                          start in [0..items.length) && end in [-1..start]
		 */
		<E extends T> ArrayIterator(int start, int end, final E... items) {
			this.items = items;
			this.cursor = start;
			this.end = end;
		}
		
		public final void remove() {
			throw new UnsupportedOperationException();
		}
	}
	
	/**
	 * An ascending iterator over an array.
	 */
	private static final class AscendingArrayIterator<T> extends ArrayIterator<T> {

		/**
		 * Constructs a new iterator over the given array of items.
		 * @requires items != null && start < end 
		 * @throws IllegalArgumentException - start < 0 || end > items.length
		 */
		<E extends T> AscendingArrayIterator(int start, int end, E[] items) {
			super(start, end, items);
			if (start < 0 || end > items.length) {
				throw new IllegalArgumentException("start < end && (start < 0 || end > items.length)");
			}
		}

		public boolean hasNext() {
			return cursor < end;
		}

		public T next() {
			if (!hasNext()) throw new NoSuchElementException();
			return items[cursor++];
		}	
	}
	
	/**
	 * A descending iterator over an array.
	 */
	private static final class DescendingArrayIterator<T> extends ArrayIterator<T> {
		/**
		 * Constructs a new iterator over the given array of items.
		 * @requires items != null && start > end 
		 * @throws IllegalArgumentException - start >= items.length || end < -1
		 */
		<E extends T> DescendingArrayIterator(int start, int end, E[] items) {
			super(start, end, items);
			if (start >= items.length || end < -1) {
				throw new IllegalArgumentException("start > end && (start >= items.length || end < -1)");
			}
		}

		public boolean hasNext() {
			return cursor > end;
		}

		public T next() {
			if (!hasNext()) throw new NoSuchElementException();
			return items[cursor--];
		}
		
	}

}
