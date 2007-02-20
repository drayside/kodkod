package kodkod.util.ints;

import java.util.NoSuchElementException;

/**
 * An ordered set of integers.  
 *
 * @specfield ints: set int
 * @author Emina Torlak
 */
public interface IntSet extends Cloneable, Iterable<Integer> {
	
	/**
	 * Returns the cardinality of this set.
	 * @return #this.ints
	 */
	public abstract int size();
	
	/**
	 * Returns true if this set has no elements; 
	 * otherwise returns false.
	 * @return no this.ints
	 */
	public abstract boolean isEmpty();
	
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
	 * Returns true if s is a subset of this set.
	 * @return s.ints in this.ints
	 * @throws NullPointerException - s = null
	 */
	public abstract boolean containsAll(IntSet s);
	
	/**
	 * Adds all of the elements in the specified set to this set 
	 * if they're not already present. The addAll operation effectively modifies 
	 * this set so that its value is the union of the two sets.
	 * @effects this.ints' = this.ints + s.ints
	 * @return this.ints' != this.ints
	 * @throws NullPointerException - s = null
	 * @throws IllegalArgumentException - if this is a bounded set and
	 * s contains an element that is out of bounds
	 */
	public abstract boolean addAll(IntSet s);
	
	/**
	 * Removes from this set all of its elements that are contained in the 
	 * specified set. This operation effectively modifies this set so that 
	 * its value is the asymmetric set difference of the two sets.
	 * @effects this.ints' = this.ints - s.ints
	 * @return this.ints' != this.ints
	 * @throws NullPointerException - s = null
	 */
	public abstract boolean removeAll(IntSet s);
	
	/**
	 * Retains only the elements in this set that are contained in the 
	 * specified set. This operation effectively modifies this set so that 
	 * its value is the intersection of the two sets.
	 * @effects this.ints' = this.ints & s.ints
	 * @return this.ints' != this.ints
	 * @throws NullPointerException - s = null
	 */
	public abstract boolean retainAll(IntSet s);
	
	/**
	 * Removes all elements from this set. 
	 * @effects no this.ints'
	 */
	public abstract void clear();
	
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
	public abstract int[] toArray();
	
	/**
     * Copies the elements of this set into the specified array, in the ascending
     * order. The array must be big enough 
     * to hold all the elements in this set, else an IndexOutOfBoundsException is thrown.
     * @effects all i: [0..this.size()) | array[i] in this.ints and #{e: this.ints | e < array[i]} = i
     * @throws IndexOutOfBoundsException - array.length < this.size()
     */
    public abstract void copyInto(int[] array);
    
    /**
     * Compares the specified object with this set for equality. 
     * Returns true if the specified object is also an IntSet, 
     * the two sets have the same size, and every member of the 
     * specified set is contained in this set (or equivalently, 
     * every member of this set is contained in the specified set). 
     * This definition ensures that the equals method works properly 
     * across different implementations of the IntSet interface.
     * @return o instanceof IntSet and o.size() = this.size() and this.containsAll(o)
     */
    public abstract boolean equals(Object o);
    
    /**
     * Returns the hash code value for this set. The hash code of a set is 
     * defined to be the {@link Ints#superFastHash(int[])} of the elements in the set, 
     * taken in the ascending order of values.  
     * This ensures that s1.equals(s2) implies that s1.hashCode()==s2.hashCode() 
     * for any two IntSets s1 and s2, as required by the general contract of the Object.hashCode method.
     * @return Ints.superFastHash(this.toArray())
     */
    public abstract int hashCode();
}