/**
 * 
 */
package kodkod.util.ints;

import java.util.Iterator;

/**
 * An implementation of a sparse sequence based on an {@link kodkod.util.ints.IntSet IntSet}.
 * This implementation can be used only when all entries in the sequence map to the same value.  
 * 
 * <p><b>Important Implementation Note</b>: As this implementation does not actually store any 
 * {@link kodkod.util.ints.IndexedEntry indexed entries},
 * the methods {@link #first()}, {@link #last()}, {@link #ceil(int)}, and {@link #floor(int)}
 * <b>may re-use the same IndexedEntry object</b>.  For example, suppose that an entry <tt>e</tt> with <tt>e.index = 1</tt> 
 * is returned by a call to <tt>predecessor(2)</tt> on a homogenous sequence <tt>h = {<0,v>, <1,v>, <2,v>}</tt>.
 * A subsequent call to <tt>h.predecessor(1)</tt> may return the same object <tt>e</tt>, with its index set to 0.  Hence,
 * the following assertion may fail:   </p>
 * <pre>
 * IndexedEntry<V> e1 = h.predecessor(2);
 * assert e1.index()==1; // this will work
 * IndexedEntry<V> e2 = h.predecessor(1);
 * assert e1.index()==1; // this may fail, as e1 may be == to e2
 * </pre>  
 * <p>The entries returned by this implementation's {@link #iterator()} are unique
 * to that iterator (but not necessarily independent of each other).  For example, </p>
 * <pre>
 * // let s be a range sequence abstractly represented as { 0->v, 1->v, 2->v }
 * Iterator<IndexedEntry<V>> iter1 = s.iterator();
 * IndexedEntry<V> e1 = iter1.next();
 * assert e1.index()==0; // this will work
 * iter1.next();
 * assert e1.index()==0; // this may fail, as the previous call may have changed the state of e1
 * Iterator<IndexedEntry<V>> iter2 = s.iterator();
 * IndexedEntry<V> e2 = iter2.next();
 * iter1.next();
 * assert e2.index()==0; // this will work
 * </pre>  
 * @specfield indeces: set int
 * @specfield entries: indeces -> one V
 * @author Emina Torlak
 */
public final class HomogenousSequence<V> extends AbstractSparseSequence<V> {
	private final IntSet indices;
	private final V value;
	private final EntryView view;
	
	/**
	 * Constructs a new homogenous sequence for the given value, backed
	 * by a {@link IntTreeSet IntTreeSet} instance.
	 * @effects this.value' = value && no this.indices'
	 */
	public HomogenousSequence(V value) {
		this.value = value;
		this.indices = new IntTreeSet();
		this.view = new EntryView();
	}

	/**
	 * Constructs a new homogenous sequence for the given value, backed
	 * by the specified intset.  Any changes to the provided set will 
	 * be reflected in this sequence, and vice versa.  This sequence will
	 * be unmodifiable if the given index set is unmodifiable.
	 * @requires indices is cloneable
	 * @effects this.value' = value &&  this.indices' = indices
	 */
	public HomogenousSequence(V value, IntSet indices) {
		this.value = value;
		this.indices = indices;
		this.view = new EntryView();
	}
	
	/**
	 * Copy constructor
	 * @effects constructs a deep copy of the original
	 */
	private HomogenousSequence(HomogenousSequence<V> original) {
		this.value = original.value;
		this.view = new EntryView();
		try {
			this.indices = original.indices.clone();
		} catch (CloneNotSupportedException e) {
			throw new InternalError(); // unreachable code.
		}
	}
	
	/**
	 * Constructs a new homogeneous sequence from the provided sequence. The
	 * returned sequence is backed by a {@link IntTreeSet IntTreeSet} instance.
	 * @requires one seq.entries[int]
	 * @effects this.value' = seq.entries[int] && this.indices' = seq.indices()
	 * @throws NullPointerException - seq = null
	 * @throws IllegalArgumentException - seq.isEmpty()
	 * @throws IllegalArgumentException - #seq.entries[int] > 1
	 */
	public HomogenousSequence(SparseSequence<? extends V> seq) {
		if (seq.isEmpty())
			throw new IllegalArgumentException();
		this.indices = new IntTreeSet();
		this.value = seq.first().value();
		this.view = new EntryView();
		for(IndexedEntry<?> e : seq) {
			if (!value.equals(e.value()))
				throw new IllegalArgumentException();
			indices.add(e.index());
		}	
	}

	/**
	 * Returns the set of all indices mapped by this sparse sequence.
	 * The returned set supports removal and addition iff this is not
	 * backed by an unmodifiable intset.
	 * @return {s: IntSet | s.ints = this.entries.V}
	 */
	public IntSet indices() {
		return indices;
	}
	
	/**
	 * Returns an iterator over the entries in this sequence,
	 * whose indeces are between from and to.  If from < to, 
	 * the entries are returned in the ascending order of 
	 * indeces.  Otherwise, they are returned in the descending
	 * order of indeces.
	 * <p>While the returned iterator <i>i</i> re-uses the same IndexedEntry 
	 * object, it is not shared with other iterators or other method calls.
	 * In particular, no other call except <tt>i.next()</tt> can change the
	 * value of the re-used object.</p>
	 * @return an iterator over the entries in this sequence
	 * whose indeces are between from and to.  Formally, if 
	 * from < to, then the first and last entries returned
	 * by the iterator are this.ceil(from) and this.floor(to).
	 * Otherwise, they are this.floor(from) and this.ceil(to).
	 * @see kodkod.util.ints.AbstractSparseSequence#iterator(int, int)
	 */
	@Override
	public Iterator<IndexedEntry<V>> iterator(int from, int to) {
		return new HomogenousIterator(indices.iterator(from, to));
	}

	/**
	 * Returns the number of entries in this sequence.
	 * @return #this.entries
	 * @see kodkod.util.ints.SparseSequence#size()
	 */
	public int size() {
		return indices.size();
	}

	/**
	 * Removes all entries from this sequences.
	 * @effects no this.entries'
	 * @see kodkod.util.ints.SparseSequence#clear()
	 */
	public void clear() {
		indices.clear();	
	}

	/**
	 * Puts the given value at the specified index.  If the 
	 * sequence already mapped the index to a value, the 
	 * previous value is replaced with the new one and returned.
	 *
	 * @requires this.value = value
	 * @effects this.indices' = this.indices + index
	 * @return this.entries[index]
	 * @throws IllegalArgumentException - this.value != value
	 * @see kodkod.util.ints.SparseSequence#put(int, V)
	 */
	public V put(int index, V value) {
		if (!equal(this.value, value))
			throw new IllegalArgumentException();
		return indices.add(index) ? null : value;
	}

	/**
	 * Returns the value to which this sequence maps the given
	 * index.  If the index is not mapped, null is returned.
	 * @return this.entries[index]
	 * @see kodkod.util.ints.SparseSequence#get(int)
	 */
	public V get(int index) {
		return indices.contains(index) ? value : null;
	}

	/**
	 * Removes the entry with the given index, if it exists, and
	 * returns the value previously stored at the index.  If the
	 * sequence had no previous mapping for the index, null is returned.
	 * @effects this.entries' = this.entries - index->E
	 * @return this.entries[index]
	 * @see kodkod.util.ints.SparseSequence#remove(int)
	 */
	public V remove(int index) {
		return indices.remove(index) ? value : null;
	}

	/**
	 * Returns true if this sparse sequence has an entry for the
	 * given index; otherwise returns false.
	 * @return some this.entries[index]
	 * @see kodkod.util.ints.SparseSequence#containsIndex(int)
	 */
	public boolean containsIndex(int index) {
		return indices.contains(index);
	}

	/**
	 * Returns true if this sequence has an entry with the given value;
	 * otherwise returns false.
	 * @return some this.indices && this.value = value
	 * @see kodkod.util.ints.SparseSequence#contains(java.lang.Object)
	 */
	public boolean contains(Object value) {
		return !indices.isEmpty() && equal(this.value, value);
	}

	/**
	 * Returns the entry with the smallest index.  If the sequence
	 * is empty, returns null.
	 * @return {e: IndexedEntry | e.index = min(this.entries.E) && 
	 *                            e.value = this.entries[e.index] }
	 * @see kodkod.util.ints.SparseSequence#first()
	 */
	public IndexedEntry<V> first() {
		return indices.isEmpty() ? null : view.toIndex(indices.min());
	}

	/**
	 * Returns the entry with the largest index.  If the sequence
	 * is empty, returns null.
	 * @return {e: IndexedEntry | e.index = max(this.entries.E) && 
	 *                            e.value = this.entries[e.index] }
	 * @see kodkod.util.ints.SparseSequence#last()
	 */
	public IndexedEntry<V> last() {
		return indices.isEmpty() ? null : view.toIndex(indices.max());
	}

	/**
	 * If an entry for the given index exists, it is returned.  Otherwise, 
	 * successor(index) is returned.
	 * @return this.containsIndex(index) => 
	 *          {e: IndexedEntry | e.index = index && e.value = this.entries[index] }, 
	 *          successor(index)
	 * @see kodkod.util.ints.SparseSequence#ceil(int)
	 */
	public IndexedEntry<V> ceil(int index) {
		if (indices.isEmpty() || index > indices.max())
			return null;
		else 
			return view.toIndex(indices.ceil(index));
	}

	/**
	 * If an entry for the given index exists, it is returned.  Otherwise, 
	 * predecessor(index) is returned.
	 * @return this.containsIndex(index) => 
	 *          {e: IndexedEntry | e.index = index && e.value = this.entries[index] }, 
	 *          predecessor(index)
	 * @see kodkod.util.ints.SparseSequence#floor(int)
	 */
	public IndexedEntry<V> floor(int index) {
		if (indices.isEmpty() || index < indices.min())
			return null;
		else 
			return view.toIndex(indices.floor(index));
	}

	/**
	 * {@inheritDoc}
	 * @see java.lang.Object#clone()
	 */
	public HomogenousSequence<V> clone() {
		// ok to clone a final class using a copy constructor
		return new HomogenousSequence<V>(this);
	}
	
	private class EntryView implements IndexedEntry<V> {
		private int index;
		
	
		final EntryView toIndex(int newIndex) {
			this.index = newIndex;
			return this;
		}
		
		public final int index() {
			return index;
		}

		public final V value() {
			return value;
		}
		
		public final String toString() {
			return index + "=" + value;
		}
		
		public final boolean equals(Object o) {
			if (o==this) return true;
			if (!(o instanceof IndexedEntry)) return false;
			return AbstractSparseSequence.equal(this, (IndexedEntry<?>)o);
		}
		
		public final int hashCode() {
			return AbstractSparseSequence.hashCode(this);
		}	
		
	}
	
	/**
	 * An iterator over a homogenous sequence.
	 * @author Emina Torlak
	 */
	private final class HomogenousIterator extends EntryView implements Iterator<IndexedEntry<V>> {
		private final IntIterator iter;
		
		/**
		 * Constructs a wrapper for the given IntIterator and value
		 */
		HomogenousIterator(IntIterator iter) {
			this.iter = iter;
		}
		
		public boolean hasNext() {
			return iter.hasNext();
		}

		public IndexedEntry<V> next() {
			return toIndex(iter.nextInt());
		}

		public void remove() {
			iter.remove();
		}
		
	}
	
}
