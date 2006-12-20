/**
 * 
 */
package kodkod.util.ints;

import java.util.Iterator;

/**
 * An immutable sparse sequence implementation designed to store a 
 * non-homogenous sequence in as little space as possible.  A compressed sequence
 * can only be created from another sparse sequence.
 * @author Emina Torlak
 */
final class CompressedSequence<V> extends AbstractSparseSequence<V> implements Cloneable {

	/**
	 * @see kodkod.util.ints.SparseSequence#ceil(int)
	 */
	public IndexedEntry<V> ceil(int index) {
		// TODO Auto-generated method stub
		return null;
	}

	/**
	 * Throws an UnsupportedOperationException.
	 * @throws UnsupportedOperationException
	 */
	public void clear() {
		throw new UnsupportedOperationException();
	}

	/**
	 * @see kodkod.util.ints.SparseSequence#contains(java.lang.Object)
	 */
	public boolean contains(Object value) {
		// TODO Auto-generated method stub
		return false;
	}

	/**
	 * @see kodkod.util.ints.SparseSequence#containsIndex(int)
	 */
	public boolean containsIndex(int index) {
		// TODO Auto-generated method stub
		return false;
	}

	/**
	 * @see kodkod.util.ints.SparseSequence#first()
	 */
	public IndexedEntry<V> first() {
		// TODO Auto-generated method stub
		return null;
	}

	/**
	 * @see kodkod.util.ints.SparseSequence#floor(int)
	 */
	public IndexedEntry<V> floor(int index) {
		// TODO Auto-generated method stub
		return null;
	}

	/**
	 * @see kodkod.util.ints.SparseSequence#get(int)
	 */
	public V get(int index) {
		// TODO Auto-generated method stub
		return null;
	}

	/**
	 * @see kodkod.util.ints.SparseSequence#indices()
	 */
	public IntSet indices() {
		// TODO Auto-generated method stub
		return null;
	}

	/**
	 * @see kodkod.util.ints.SparseSequence#isEmpty()
	 */
	public boolean isEmpty() {
		// TODO Auto-generated method stub
		return false;
	}

	/**
	 * @see kodkod.util.ints.SparseSequence#iterator()
	 */
	public Iterator<IndexedEntry<V>> iterator() {
		// TODO Auto-generated method stub
		return null;
	}

	/**
	 * @see kodkod.util.ints.SparseSequence#iterator(int, int)
	 */
	public Iterator<IndexedEntry<V>> iterator(int from, int to) {
		// TODO Auto-generated method stub
		return null;
	}

	/**
	 * @see kodkod.util.ints.SparseSequence#last()
	 */
	public IndexedEntry<V> last() {
		// TODO Auto-generated method stub
		return null;
	}

	/**
	 * Throws an UnsupportedOperationException.
	 * @throws UnsupportedOperationException
	 */
	public V put(int index, V value) {
		throw new UnsupportedOperationException();
	}	

	/**
	 * Throws an UnsupportedOperationException.
	 * @throws UnsupportedOperationException
	 */
	public V remove(int index) {
		throw new UnsupportedOperationException();
	}

	/**
	 * @see kodkod.util.ints.SparseSequence#size()
	 */
	public int size() {
		// TODO Auto-generated method stub
		return 0;
	}
	
	/**
	 * @see java.lang.Object#clone()
	 */
	public SparseSequence<V> clone() throws CloneNotSupportedException {
		return null;
	}

}
