package kodkod.util.ints;

import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * A skeletal implementation of the SparseSequence interface.
 * The class provides an implementation for the <code>isEmpty</code>, 
 * <code>putAll</code>, <code>equals</code>, <code>hashCode</code>,
 * and <code>toString</code> methods.  All other methods must be
 * implemented by the subclasses. 
 * 
 * @specfield entries: int -> lone V
 * 
 * @author Emina Torlak
 */
public abstract class AbstractSparseSequence<V> implements SparseSequence<V> {

	/**
	 * Constructs a sparse sequence
	 * @effects no this.entries'
	 */
	protected AbstractSparseSequence() {}
	
	/**
	 * Returns true if the size of this sequence is 0.
	 * @return this.size()==0
	 */
	public boolean isEmpty() { 
		return size()==0; 
	}
	
	/**
	 * Returns an iterator over the entries in this sequence
	 * in the ascending order of indeces, starting at this.first().
	 * This method calls this.iterator(Integer.MIN_VALUE, Integer.MAX_VALUE).
	 * @return an iterator over this.entries starting at the entry
	 * with the smallest index
	 */
	public Iterator<IndexedEntry<V>> iterator() {
		return iterator(Integer.MIN_VALUE, Integer.MAX_VALUE);
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
	 */
	public abstract Iterator<IndexedEntry<V>> iterator(int from, int to);

	/**
	 * Returns the set of all indices mapped by this sparse sequence.
	 * The returned set supports removal iff this is not an unmodifiable
	 * sparse sequence.
	 * @return {s: IntSet | s.ints = this.entries.V}
	 */
	public IntSet indices() {
		return new AbstractIntSet() {

			@Override
			public IntIterator iterator(final int from, final int to) {
				return new IntIterator() {
					Iterator<IndexedEntry<V>> iter = AbstractSparseSequence.this.iterator(from, to);
					public boolean hasNext() {
						return iter.hasNext();
					}

					public int nextInt() {
						return iter.next().index();
					}

					public Integer next() {
						return nextInt();
					}

					public void remove() {
						iter.remove();
					}
				};
			}

			@Override
			public int size() {
				return AbstractSparseSequence.this.size();
			}
			
			public boolean contains(int i) {
				return containsIndex(i);
			}
			
			public int min() {
				final IndexedEntry<V> first = AbstractSparseSequence.this.first();
				if (first==null) 
					throw new NoSuchElementException();
				return first.index();
			}
			
			public int max() {
				final IndexedEntry<V> last = AbstractSparseSequence.this.last();
				if (last==null) 
					throw new NoSuchElementException();
				return last.index();
			}
			
			public boolean remove(int i) {
				final boolean isMapped = containsIndex(i);
				AbstractSparseSequence.this.remove(i);
				return isMapped;
			}
		};
	}
	
	/*---------- adapted from java.util.AbstractMap -----------*/
	
	/**
	 * Copies all of the entries from the specified sparse sequence to 
	 * this sequence. This implementation calls put(e.index, e.value) on 
	 * this sequence once for each entry e in the specified sequence. 
     * @effects this.entries' = this.entries ++ s.entries
	 */
	public void putAll(SparseSequence<? extends V> s) {
		Iterator<? extends IndexedEntry<? extends V>> i = s.iterator();
		while (i.hasNext()) {
			IndexedEntry<? extends V> e = i.next();
			put(e.index(), e.value());
		}
	}
	
	/**
	 * Returns true if both o1 and o2 are null, or 
	 * o1.equals(o2)
	 * @return o1 and o2 are equal
	 */
	static boolean equal(Object o1, Object o2) {
		return o1==null ? o2==null : o1.equals(o2);
	}
	
	/**
	 * Compares the specified object with this sequence for equality.  Returns
	 * <tt>true</tt> if the given object is also a sequence and the two sequences
	 * represent the same function from integers to E.<p>
	 *
	 * This implementation first checks if the specified object is this sequence;
	 * if so it returns <tt>true</tt>.  Then, it checks if the specified
	 * object is a sequence whose size is identical to the size of this set; if
	 * not, it returns <tt>false</tt>.  If so, it iterates over this sequences's
	 * entries, and checks that the specified sequence
	 * contains each entry that this sequence contains.  If the specified sequence
	 * fails to contain such an entry, <tt>false</tt> is returned.  If the
	 * iteration completes, <tt>true</tt> is returned.
	 * @return  o in SparseSequence && o.entries = this.entries
	 */
	@SuppressWarnings("unchecked")
	public boolean equals(Object o) {
		if (o == this) return true;
		
		if (!(o instanceof SparseSequence)) return false;
		
		SparseSequence<V> s = (SparseSequence<V>) o;
		if (s.size() != size()) return false;
		
		try {
			final Iterator<IndexedEntry<V>> i1 = iterator(), i2 = s.iterator();
			while (i1.hasNext()) {
				IndexedEntry<V> e1 = i1.next(), e2 = i2.next();
				if (e1.index() != e2.index() || !equal(e1.value(), e2.value())) 
					return false;
			}
		} catch(ClassCastException unused) {
			return false;
		} catch(NullPointerException unused) {
			return false;
		}
		
		return true;
	}
	
	/**
	 * Returns the hash code value for this sparse sequence. 
	 * The hash code of a sparse sequence is defined to be the sum of the 
	 * hashCodes of each entry of its entries. This ensures that t1.equals(t2) 
	 * implies that t1.hashCode()==t2.hashCode() for any two sequences t1 and t2, 
	 * as required by the general contract of Object.hashCode.
	 * This implementation iterates over this.entries, calling
	 * <tt>hashCode</tt> on each IndexedEntry in the sequence, and adding
	 * up the results.
	 * @return sum(this.entries.hashCode())
	 */
	public int hashCode() {
		int h = 0;
		for (IndexedEntry<V> e : this)
			h += e.hashCode();
		return h;
	}
	
	/**
	 * Returns a string representation of this sequence.  The string representation
	 * consists of a list of index-value mappings in the order returned by the
	 * sequences <tt>iterator</tt>, enclosed in brackets
	 * (<tt>"[]"</tt>).  Adjacent entries are separated by the characters
	 * <tt>", "</tt> (comma and space).  Each index-value mapping is rendered as
	 * the index followed by an equals sign (<tt>"="</tt>) followed by the
	 * associated value.  Elements are converted to strings as by
	 * <tt>String.valueOf(Object)</tt>.<p>
	 *
	 * This implementation creates an empty string buffer, appends a left
	 * bracket, and iterates over the map's entries, appending
	 * the string representation of each <tt>IndexedEntry</tt> in turn.  After
	 * appending each entry except the last, the string <tt>", "</tt> is
	 * appended.  Finally a right bracket is appended.  A string is obtained
	 * from the stringbuffer, and returned.
	 *
	 * @return a String representation of this map.
	 */
	public String toString() {
		final StringBuffer buf = new StringBuffer();
		buf.append("[");
		
		final Iterator<IndexedEntry<V>> i = iterator();
		boolean hasNext = i.hasNext();
		while (hasNext) {
			IndexedEntry<V> e = i.next();
			buf.append(e.index());
			buf.append("=");
			if (e.value() == this)
				buf.append("(this sequence)");
			else
				buf.append(e.value());
			hasNext = i.hasNext();
			if (hasNext) buf.append(", ");
		}
		
		buf.append("]");
		return buf.toString();
	}

	
}
