package kodkod.util.collections;

import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * This class provides utility methods for constructing
 * iterators over arrays and convenience wrappers for 
 * java.util.Arrays sorting procedures.
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
	private static final Comparator<Object> identityComparator() {
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
	 * given array and returns it.
	 * @effects java.util.Arrays.sort(array, comparator) 
	 * @return array
	 */
	public static final <T> T[] sort(T[] array, Comparator<? super T> comparator) {
		java.util.Arrays.sort(array, comparator);
		return array;
	}
	
	/**
	 * Calls {@link java.util.Arrays#sort(Object[], Comparator)} on the 
	 * given array and returns it.  The elements are sorted in the ascending
	 * order of their identity hashcodes.
	 * @effects java.util.Arrays.sort(array, {@link #identityComparator()}) 
	 * @return array
	 */
	public static final <T> T[] identitySort(T[] array) {
		java.util.Arrays.sort(array, identityComparator());
		return array;
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
