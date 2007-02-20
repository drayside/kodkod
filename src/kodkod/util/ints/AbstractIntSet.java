package kodkod.util.ints;

import java.util.NoSuchElementException;

/**
 * A skeletal implementation of the IntSet interface.  
 * @author Emina Torlak
 */
public abstract class AbstractIntSet implements IntSet {
	
	/**
	 * Constructs an empty int set.
	 * @effects no this.ints'
	 */
	protected AbstractIntSet() {}
	
	/**
	 * Throws a NoSuchElementException if this is an empty set.
	 * @throws NoSuchElementException - this.isEmpty()
	 */
	final void checkNonEmpty() {
		if  (isEmpty()) throw new NoSuchElementException("no this.ints");
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.util.ints.IntSet#isEmpty()
	 */
	public boolean isEmpty() { return size()==0; }
	
	/**
	 * Returns an ascending iterator over all elements in this set.
	 * This method calls this.iterator(Integer.MIN_VALUE, Integer.MAX_VALUE).
	 * @return an ascending iterator over all elements in this set.
	 */
	public IntIterator iterator() {
		return iterator(Integer.MIN_VALUE, Integer.MAX_VALUE);
	}
	
	/**
	 * Returns an iterator over the elements of this set that
	 * are in the closed range [from..to].  If from < to, 
	 * the elements are returned in the ascending order.  
	 * Otherwise, they are returned in the descending order.
	 * @return an iterator over the elements in this sequence
	 * that are in the closed range [from..to]. 
	 */
	public abstract IntIterator iterator(int from, int to);
	
	/**
	 * Iterates through this.ints and returns true if it
	 * finds i, otherwise returns false.
	 * @return i in this.ints
	 */
	public boolean contains(int i) {
		for(IntIterator iter = iterator(); iter.hasNext(); ) {
			if (i==iter.nextInt()) return true;
		}
		return false;
	}
	
	/**
	 * Returns the first element returned by this.iterator().
	 * If this set is empty, throws a NoSuchElementException().
	 * @return min(this.ints)
	 * @throws NoSuchElementException - no this.ints
	 */
	public int min() {
		return iterator().nextInt();
	}
	
	/**
	 * Returns the first element returned by this.iterator(Integer.MAX_VALUE, Integer.MIN_VALUE).
	 * If this set is empty, throws a NoSuchElementException().
	 * @return max(this.ints)
	 * @throws NoSuchElementException - no this.ints
	 */
	public int max() {
		final IntIterator iter = iterator(Integer.MAX_VALUE, Integer.MIN_VALUE);
		return iter.nextInt();
	}
	
	/**
	 * Throws an UnsupportedOperationException.
	 * @throws UnsupportedOperationException
	 */
	public boolean add(int i) {
		throw new UnsupportedOperationException();
	}
	
	/**
	 * Iterates through the elements of this, removes
	 * i if it finds it and returns true.  Otherwise
	 * returns false.  Throws an UnsupportedOperationException
	 * if this.intIterator() does not support removal.
	 * @effects this.ints' = this.ints - i
	 * @return i in this.ints
	 * @throws UnsupportedOperationException - this.intIterator() does not support removal
	 */
	public boolean remove(int i) {
		for(IntIterator iter = iterator(); iter.hasNext(); ) {
			if (i==iter.nextInt()) {
				iter.remove();
				return true;
			}
		}
		return false;
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.util.ints.IntSet#containsAll(kodkod.util.ints.IntSet)
	 */
	public boolean containsAll(IntSet s) {
		if (size()>=s.size()) {
			for(IntIterator itr = s.iterator(); itr.hasNext(); ) {
				if (!contains(itr.nextInt()))
					return false;
			}
			return true;
		}
		return false;
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.util.ints.IntSet#addAll(kodkod.util.ints.IntSet)
	 */
	public boolean addAll(IntSet s) {
		boolean modified = false;
		for(IntIterator itr = s.iterator(); itr.hasNext(); ) {
			if (add(itr.nextInt()))
				modified = true;
		}
		return modified;
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.util.ints.IntSet#retainAll(kodkod.util.ints.IntSet)
	 */
	public boolean retainAll(IntSet s) {
		boolean modified = false;
		for(IntIterator itr = iterator(); itr.hasNext(); ) {
			if (!s.contains(itr.nextInt())) {
				modified = true;
				itr.remove();
			}
		}
		return modified;
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.util.ints.IntSet#removeAll(kodkod.util.ints.IntSet)
	 */
	public boolean removeAll(IntSet s) {
		boolean modified = false;
		for(IntIterator itr = iterator(); itr.hasNext();) {
			if (s.contains(itr.nextInt())) {
				modified = true;
				itr.remove();
			}
		}
		return modified;
	}
	
	 /**

     * This implementation iterates over this set, removing each
     * element using the <tt>Iterator.remove</tt> operation.  Most
     * implementations will probably choose to override this method for
     * efficiency.<p>
     *
     * Note that this implementation will throw an
     * <tt>UnsupportedOperationException</tt> if the iterator returned by this
     * collection's <tt>iterator</tt> method does not implement the
     * <tt>remove</tt> method and this collection is non-empty.
     *
     * @throws UnsupportedOperationException if the <tt>clear</tt> method is
     * 		  not supported by this collection.
     */
	public void clear() {
		for(IntIterator itr = iterator(); itr.hasNext();) {
			itr.nextInt();
			itr.remove();
		}
	}
		
	/**
	 * Returns the result of calling super.clone().
	 * @see java.lang.Object#clone()
	 */
	public IntSet clone() throws CloneNotSupportedException {
		return (IntSet) super.clone();
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.util.ints.IntSet#toArray()
	 */
	public int[] toArray() {
		final int[] ret = new int[size()];
		final IntIterator itr = iterator();
		for(int i = 0; i < ret.length; i++) {
			ret[i] = itr.nextInt();
		}
		return ret;
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.util.ints.IntSet#copyInto(int[])
	 */
	public void copyInto(int[] array) {
		if (array.length < size()) 
			throw new IndexOutOfBoundsException();
		int i = 0;
		for(IntIterator itr = iterator(); itr.hasNext(); ) {
			array[i++] = itr.nextInt();
		}
	}
	
	/**
	 * {@inheritDoc}
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object o) {
		if (o==this) return true;
		else if (o instanceof IntSet) {
			final IntSet s = (IntSet) o;
			return size()==s.size() && containsAll(s);
		} else return false;
	}
	
	/**
	 * {@inheritDoc}
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() { 
		return Ints.superFastHash(toArray());
	}
	
	/**
	 * {@inheritDoc}
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		final StringBuilder buf = new StringBuilder("{");
		final IntIterator itr = iterator();
		if (itr.hasNext()) buf.append(itr.nextInt());
		while(itr.hasNext()) {
			buf.append(", ");
			buf.append(itr.nextInt());
		}
		buf.append("}");
		return buf.toString();
	}
}
