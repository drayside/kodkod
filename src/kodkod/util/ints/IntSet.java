package kodkod.util.ints;

import java.util.Collection;
import java.util.Set;

/**
 * An ordered set of integers.  
 *
 * @specfield ints: set int
 * @author Emina Torlak
 */
public interface IntSet extends Set<Integer> {
	
	/**
	 * Returns true if i is in this set.
	 * @return i in this.ints
	 */
	public abstract boolean contains(int i);

	/**
	 * Returns the smallest element in this set.
	 * Throws a NoSuchElementException if this set is empty.
	 * @return min(this.ints)
	 * @throws java.util.NoSuchElementException - no this.ints
	 */
	public abstract int min();
	
	/**
	 * Returns the largest element in this set.
	 * Throws a NoSuchElementException if this set is empty.
	 * @return max(this.ints)
	 * @throws java.util.NoSuchElementException - no this.ints
	 */
	public abstract int max();
	
	/**
	 * Returns the largest element in this set that 
	 * is smaller than or equal to i.  If this is emtpy or i is less than this.min(),
	 * NoSuchElementException is thrown.
	 * @return {j: this.ints | j <= i && no k: this.ints - j | k > j && k <= i}
	 * @throws NoSuchElementException - no this.ints || i < this.min()
	 */
	public abstract int floor(int i);
	
	/**
	 * Returns the smallest element in this set that 
	 * is greater than or equal to i.  If this is emtpy or i is greater than this.max(),
	 * NoSuchElementException is thrown.
	 * @return {j: this.ints | j >= i && no k: this.ints - j | k < j && k >= i}
	 * @throws NoSuchElementException - no this.ints || i > this.max()
	 */
	public abstract int ceil(int i);
	
	/**
	 * Returns an iterator over the integers in this set,
	 * in the ascending element order.
	 * @return an IntIterator over the integers in this set.
	 */
	public abstract IntIterator iterator();
	
	/**
	 * Returns an iterator over the elements of this set that
	 * are in the closed range [from..to].  If from < to, 
	 * the elements are returned in the ascending order.  
	 * Otherwise, they are returned in the descending order.
	 * @return an iterator over the elements in this set
	 * that are in the closed range [from..to]. 
	 */
	public abstract IntIterator iterator(int from, int to);
	
	/**
	 * Adds the given integer to this set if not already present
	 * and returns true.  Otherwise does nothing and returns false.
	 * @effects this.ints' = this.ints + i
	 * @return i in this.ints'
	 * @throws IllegalArgumentException - this is a bounded set
	 * and i is out of bounds
	 */
	public abstract boolean add(int i);

	/**
	 * Removes the given integer from this set if already present and
	 * returns true.  Otherwise does nothing and returns false.
	 * @effects this.ints' = this.ints - i
	 * @return i !in this.ints'
	 */
	public abstract boolean remove(int i);
	
	/**
	 * Adds all of the elements in the specified collection to this set 
	 * if they're not already presen. If the specified 
	 * collection is also a set, the addAll operation effectively modifies 
	 * this set so that its value is the union of the two sets.
	 * @effects this.ints' = this.ints + c.elements 
	 * @return this.ints' != this.ints
	 * @throws NullPointerException - c = null || null in c.elements
	 * @throws IllegalArgumentException - if this is a bounded set and
	 * c contains an element that is out of bounds
	 */
	public abstract boolean addAll(Collection<? extends Integer> c);
	
	/**
	 * Returns a copy of this IntSet.  The copy is independent of this 
	 * IntSet unless this is a singleton or an immutable set, in which case
	 * clone() may return this.  An implementing class that does not support
	 * cloning may throw a CloneNotSupportedException.
	 * @return a copy of this IntSet.
	 * @throws CloneNotSupportedException - this is not cloneable
	 */
	public abstract IntSet clone() throws CloneNotSupportedException;
	
	 /**
     * Returns an array containing all of the elements in this set in the
     * ascending order.
     *
     * @return an array containing all of the elements in this set in the
     * ascending order.
     */
	public abstract int[] toIntArray();

}