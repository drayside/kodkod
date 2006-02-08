/**
 * ArraySequence.java
 * Created on 1:07:24 PM
 */
package kodkod.util;

import java.util.Iterator;

/**
 * An implementation of a sparse sequence based on an array.
 * This implementation can be used only when the indeces 
 * of the sequence are known in advance.  The indeces with
 * which an ArraySequence is construct remain fixed throughout.
 * The remove operation is therefore not supported, and the 
 * put operation fails whenever its index argument is not one
 * of the sequence's pre-set indeces.
 * 
 * @specfield indeces: set int
 * @specfield entries: indeces -> one V
 * @author Emina Torlak
 */
public final class ArraySequence<V> extends AbstractSparseSequence<V> {
	private final SimpleEntry<V>[] entries;
	/**
	 * Constructs an array sequence that contains 
	 * the given indeces.  All indeces are initially
	 * mapped to the specified defaultValue.
	 * @effects this.indeces' = indeces && this.entries' = indeces -> defaultValue
	 * @throws NullPointerException - indeces = null
	 */
	@SuppressWarnings("unchecked")
	public ArraySequence(IntSet indeces, V defaultValue) {
		this.entries = new SimpleEntry[indeces.size()];
		final IntIterator indexIter = indeces.iterator();
		for(int i = 0; indexIter.hasNext(); i++) {
			entries[i] = new SimpleEntry<V>(indexIter.nextInt(), defaultValue);
		}
	}
	
	/**
	 * Constructs a new array sequence with the same index/value mappings
	 * as the given sequence.
	 * @effects this.entries' = s.entries
	 * @throws NullPointerException - s = null
	 */
	@SuppressWarnings("unchecked")
	public ArraySequence(SparseSequence<? extends V> s) {
		this.entries = new SimpleEntry[s.size()];
		int i = 0;
		for(IndexedEntry<?> entry : s) {
			entries[i++] = new SimpleEntry<V>(entry.index(), (V)entry.value());
		}
	}
	/**
	 * Returns the number of entries in this sequence.
	 * @return #this.entries
	 * @see kodkod.util.SparseSequence#size()
	 */
	public int size() {
		return entries.length;
	}
	
	/**
	 * Returns true if this sequence is empty; otherwise returns false.
	 * @return no this.entries
	 * @see kodkod.util.SparseSequence#isEmpty()
	 */
	public boolean isEmpty() {
		return entries.length==0;
	}
	
	/** 
	 * This operation is not supported for array sequences.
	 * @throws UnsupportedOperationException
	 * @see kodkod.util.SparseSequence#clear()
	 */
	public void clear() {
		throw new UnsupportedOperationException();
	}
	
	/**  
	 * Searches this.entries for the specified index using the
	 * binary search algorithm.  If the index is not found, then
	 * -insertionPoint - 1 is returned, where insertionPoint is 
	 * the point at which the given index would be inserted into
	 * this.entries.  
	 * @return the position in this.entries where the entry with
	 * the given index is located, or -insertionPoint - 1 if 
	 * the index is not in this.indeces
	 */
	private final int search(int index) {
		int low = 0;
		int high = entries.length-1;
		
		while (low <= high) {
			int mid = (low + high) >> 1;
			int midIndex = entries[mid].index;		
			if (midIndex < index)
				low = mid + 1;
			else if (midIndex > index)
				high = mid - 1;
			else
				return mid; // key found
		}
		
		return -(low + 1);  // key not found.
	}
	
	/**
	 * Puts the given value at the specified index.  If the 
	 * sequence already mapped the index to a value, the 
	 * previous value is replaced with the new one and returned.
	 *
	 * @effects this.entries' = this.entries + index->value
	 * @return this.entries[index]
	 * @throws IndexOutOfBoundsException - index !in this.indeces
	 * @see kodkod.util.SparseSequence#put(int, Object)
	 */
	public V put(int index, V value) {
		final int position = search(index);
		if (position < 0)
			throw new IndexOutOfBoundsException(""+index);
		return entries[position].setValue(value);
	}
	
	/**
	 * Returns the value to which this sequence maps the given
	 * index.  If the index is not mapped, null is returned.
	 * @return this.entries[index]
	 * @see kodkod.util.SparseSequence#get(int)
	 */
	public V get(int index) {
		final int position = search(index);
		return position < 0 ? null : entries[position].value;
	}
	
	/** 
	 * This operation is not supported for array sequences.
	 * @see kodkod.util.SparseSequence#remove(int)
	 */
	public V remove(int index) {
		throw new UnsupportedOperationException();
	}
	
	/**
	 * Returns true if this sparse sequence has an entry for the
	 * given index; otherwise returns false.
	 * @return index in this.indeces
	 * @see kodkod.util.SparseSequence#containsIndex(int)
	 */
	public boolean containsIndex(int index) {
		return search(index) >= 0;
	}
	
	/**
	 * Returns true if this sequence has an entry with the given value;
	 * otherwise returns false.
	 * @return some this.entries.value
	 * @see kodkod.util.SparseSequence#contains(java.lang.Object)
	 */
	public boolean contains(Object value) {
		if (value==null) {
			for(SimpleEntry<V> entry : entries) {
				if (entry==null)
					return true;
			}
		} else {
			for(SimpleEntry<V> entry : entries) {
				if (value.equals(entry.value))
					return true;
			}
		}
		return false;
	}
	
	/**
	 * Returns an iterator over the entries in this sequence
	 * in the ascending order of indeces, starting at this.first().
	 * The returned iterator does not support removal.
	 * @return an iterator over this.entries starting at the entry
	 * with the smallest index
	 * @see kodkod.util.SparseSequence#iterator()
	 */
	public Iterator<IndexedEntry<V>> iterator() {
		return Iterators.iterate(entries);
	}
	
	/**
	 * Returns an iterator over the entries in this sequence,
	 * whose indeces are between from and to.  If from < to, 
	 * the entries are returned in the ascending order of 
	 * indeces.  Otherwise, they are returned in the descending
	 * order of indeces.
	 * @return an iterator over the entries in this sequence
	 * whose indeces are between from and to.  Formally, if 
	 * from < to, then the first and last entries returned
	 * by the iterator are this.ceil(from) and this.floor(to).
	 * Otherwise, they are this.floor(from) and this.ceil(to).
	 * @see kodkod.util.SparseSequence#iterator(int, int)
	 */
	public Iterator<IndexedEntry<V>> iterator(int from, int to) {
		final int fromPos = search(from);
		final int toPos = search(to);
		int start = entries.length, end = start;
		if (from==to && fromPos >= 0) {
			start = fromPos;
			end = toPos+1;
		} else if (from < to && fromPos < entries.length && fromPos > -entries.length-1) {
			start = fromPos < 0 ? -fromPos-1 : fromPos;
			end = toPos < 0 ? -toPos-1 : toPos + 1;
		} else if (from > to && (fromPos > 0 || fromPos < -1)) {
			start = fromPos > 0 ? fromPos : -fromPos-2;
			end = toPos < 0 ? -toPos-2 : toPos - 1;
		}
//		System.out.println(this + " start, end: " + start + ", " + end + " from, to: " + from + ", " + to + " fromPos, toPos: " + fromPos + ", " + toPos);
		return Iterators.iterate(start,end,entries);
	}
	
	/**
	 * Returns the entry with the smallest index.  If the sequence
	 * is empty, returns null.
	 * @return {e: IndexedEntry | e.index = min(this.entries.E) && 
	 *                            e.value = this.entries[e.index] }
	 * @see kodkod.util.SparseSequence#first()
	 */
	public IndexedEntry<V> first() {
		return entries.length==0 ? null : entries[0];
	}
	
	/**
	 * Returns the entry with the largest index.  If the sequence
	 * is empty, returns null.
	 * @return {e: IndexedEntry | e.index = max(this.entries.E) && 
	 *                            e.value = this.entries[e.index] }
	 * @see kodkod.util.SparseSequence#last()
	 */
	public IndexedEntry<V> last() {
		return entries.length==0 ? null : entries[entries.length-1];
	}
	
	/**
	 * Returns the entry whose index is the smallest number mapped by this sequence that is
	 * larger than the given index.  If no such entry exists, null is returned.
	 * @return {e: IndexedEntry | e.value = this.entries[e.index] && e.index in this.entries.E &&
	 *                            no i: this.entries.E - e.index | index < i < e.index }
	 * @see kodkod.util.SparseSequence#successor(int)
	 */
	public IndexedEntry<V> successor(int index) {
		final int position = search(index);
		if (position < entries.length-1 && position > -entries.length-1)
			return position < 0 ? entries[-position-1] : entries[position+1];
		else return null;
	}
	
	/**
	 * Returns the entry whose index is the largest number mapped by this sequence that is
	 * smaller than the given index.  If no such entry exists, null is returned.
	 * @return {e: IndexedEntry | e.value = this.entries[e.index] && e.index in this.entriess.E &&
	 *                            no i: this.entries.E - e.index | index > i > e.index }
	 * @see kodkod.util.SparseSequence#predecessor(int)
	 */
	public IndexedEntry<V> predecessor(int index) {
		final int position = search(index);
		if (position < -1) 
			return entries[-position-2];
		else if (position > 0) 
			return entries[position-1];
		else return null;
	}
		
	/**
	 * If an entry for the given index exists, it is returned.  Otherwise, 
	 * successor(index) is returned.
	 * @return this.containsIndex(index) => 
	 *          {e: IndexedEntry | e.index = index && e.value = this.entries[index] }, 
	 *          successor(index)
	 * @see kodkod.util.SparseSequence#ceil(int)
	 */
	public IndexedEntry<V> ceil(int index) {
		final int position = search(index);
		if (position < entries.length && position > -entries.length-1)
			return position < 0 ? entries[-position-1] : entries[position];
		else return null;
	}
	
	/**
	 * If an entry for the given index exists, it is returned.  Otherwise, 
	 * predecessor(index) is returned.
	 * @return this.containsIndex(index) => 
	 *          {e: IndexedEntry | e.index = index && e.value = this.entries[index] }, 
	 *          predecessor(index)
	 * @see kodkod.util.SparseSequence#floor(int)
	 */
	public IndexedEntry<V> floor(int index) {
		final int position = search(index);
		if (position < -1) 
			return entries[-position-2];
		else if (position > 0) 
			return entries[position];
		else return null;
	}
	
	/**
	 * A simple indexed entry that adds no additional 
	 * fields/functionality to the base implementation.
	 * @author Emina Torlak
	 */
	@SuppressWarnings("hiding")
	private static final class SimpleEntry<V> extends AbstractIndexedEntry<V> {
		protected SimpleEntry(int index, V value) {
			super(index, value);
		}
	}
}
