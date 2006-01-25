package kodkod.util;

import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * Implementation of an unmodifiable iterator over an
 * array. 
 * 
 * @author Emina Torlak
 */
public final class ArrayIterator<T> implements Iterator<T> {
	private final T[] items;
	private final int end;
	private int cursor;
	
	/**
	 * Constructs a new iterator over the given array of items.
	 * The iterator is backed by the given array.  The contents
	 * of the array are not modified by the iterator.  The 
	 * constructed iterator returns the items located between the 
	 * indeces start, inclusive, and end, exclusive.
	 * @requires items != null && end in [0..items.length] && start in [0..end]
	 */
	private <E extends T> ArrayIterator(int start, int end, final E... items) {
		this.items = items;
		this.cursor = start;
		this.end = end;
	}
	
	/**
	 * Returns a new iterator over the given array of items.
	 * The iterator is backed by the given array.  The contents
	 * of the array are not modified by the iterator.  The effect
	 * of this method is the same as calling ArrayIterator.iterator(0, items.length, items).
	 * @throws NullPointerException - items = null
	 */
	public static final <T, E extends T> Iterator<T> iterate(final E... items) {
		return new ArrayIterator<T>(0, items.length, items);
	}
	
	/**
	 * Returns a new iterator over the given array of items.
	 * The iterator is backed by the given array.  The contents
	 * of the array are not modified by the iterator.
	 * The returned iterator enumerates the items located between
	 * indeces start, inclusive, and end, exclusive.
	 * @throws NullPointerException - items = null
	 * @throws IllegalArgumentException - end !in [0..items.length] || start !in [0..end]
	 */
	public static final <T, E extends T> Iterator<T> iterate(int start, int end, final E... items) {
		if (end < 0 || end > items.length || start < 0 || start > end)
			throw new IllegalArgumentException("end !in [0..items.length] || start !in [0..end]");
		return new ArrayIterator<T>(start, end, items);
	}

	/**
	 * Returns true if there are more elements in the iteration.
	 * @return true if there are more elements in the iteration.
	 */
	public boolean hasNext() {
		return cursor < end;
	}

	/**
	 * Returns the next element in the iteration.  If there are
	 * no more elements left, throws a NoSuchElementException.
	 * @return the next element
	 * @throws java.util.NoSuchElementException - !this.hasNext()
	 */
	public T next() {
		if (!hasNext()) throw new NoSuchElementException();
		return items[cursor++];
	}

	/**
	 * The remove operation is not supported.
	 * @throws UnsupportedOperationException
	 */
	public void remove() {
		throw new UnsupportedOperationException();
	}

}
