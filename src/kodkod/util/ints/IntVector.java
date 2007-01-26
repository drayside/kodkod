package kodkod.util.ints;

import java.util.NoSuchElementException;

/**
 * A resizable array of integers.
 * 
 * @specfield length: int
 * @specfield elements: [0..size) ->one int
 * 
 * @author Emina Torlak
 */
public interface IntVector extends Iterable<Integer>{
	/**
     * Returns the number of elements in this vector.       
     * @return this.length
     */
    public int length();

    /**
     * Returns <tt>true</tt> if this vector contains no elements.
     *
     * @return no this.elements
     */
    public boolean isEmpty();
    
    /**
     * Returns <tt>true</tt> if this vector contains the specified element.
     * @return element in this.elements[int]
     */
    public boolean contains(int element);
    
    /**
     * Returns the element at the specified position in this vector.
     *
     * @return this.elements[index]
     * 
     * @throws IndexOutOfBoundsException if the index is out of range (index
     * 		  &lt; 0 || index &gt;= length()).
     */
    public int get(int index);	
    
    /**
     * Returns an iterator over the elements in this vector in proper sequence.
     *
     * @return an iterator over the elements in this vector in proper sequence.
     */
    public IntIterator iterator();
    
    /**
     * Returns an iterator over the elements in this vector in proper sequence, 
     * starting <tt>fromIndex<\tt>, inclusive, and ending at <tt>toIndex<\tt>, exclusive.
     * If <tt>fromIndex<\tt> is less than <tt>toIndex<\tt>, then the iterator will return
     * the elements in the descending order.
     * @return an iterator over the elements in this vector in proper sequence, 
     * starting at <tt>fromIndex<\tt>, inclusive, and ending at <tt>toIndex<\tt>.
     * @throws IndexOutOfBoundsException - fromIndex !in [0..this.length) || toIndex !in [-1..this.length]
     */
    public IntIterator iterator(int fromIndex, int toIndex);
    
    /**
     * Replaces the element at the specified position in this vector with the
     * specified element, and returns the previous element (optional operation).
     *
     * @effects this.elements' = this.elements' ++ index -> element
     * @return this.elements[index]
     * 
     * @throws UnsupportedOperationException if the <tt>set</tt> method is not
     *		  supported by this vector.
     * @throws    IllegalArgumentException if some aspect of the specified
     *		  element prevents it from being added to this vector.
     * @throws    IndexOutOfBoundsException if the index is out of range
     *		  (index &lt; 0 || index &gt;= length()).
     */
    public int set(int index, int element);
    
    /**
     * Removes all of the elements from this vector (optional operation).  This
     * vector will be empty after this call returns (unless it throws an
     * exception).
     * @effects this.length' = 0 && no this.elements'
     * @throws UnsupportedOperationException if the <tt>clear</tt> method is
     * 		  not supported by this vector.
     */
    public void clear();
    
    /**
     * Returns the index in this vector of the first occurrence of the specified
     * element, or -1 if this vector does not contain this element.
     * 
     * @return element in this.elements[int] => min(this.elements.element), -1
     */
    public int indexOf(int element);
    
    /**
     * Returns the index in this vector of the last occurrence of the specified
     * element, or -1 if this vector does not contain this element.
     * 
     * @return element in this.elements[int] => max(this.elements.element), -1
     */
    public int lastIndexOf(int element);
    
    /**
     * Returns the smallest element in this vector.  Throws NoSuchElementException if 
     * this is empty
     * @return min(this.elements[int])
     * @throws NoSuchElementException - no this.elements
     */
    public int min();
    
    /**
     * Returns the largest element in this vector.  Throws NoSuchElementException if 
     * this is empty
     * @return max(this.elements[int])
     * @throws NoSuchElementException - no this.elements
     */
    public int max();
    
    /**
     * Sorts the elements in this vector in the ascending order (optional operation).
     * @effects all i, j: [0..this.length) | i < j => this.elements'[i] <= this.elements'[j]
     * @throws UnsupportedOperationException - if the <tt>sort<\tt> method is not supported by this vector
     */
    public void sort();
    
    /**
     * Searches this.elements for the specified value using the binary search algorithm. 
     * This vector must be sorted (as by the {@link #sort()}}) prior to making this call. 
     * If it is not sorted, the results are undefined. 
     * If the vector contains multiple elements with the specified value, there is no 
     * guarantee which one will be found.
     * @return index of the search key, if it is contained in the vector; 
     * otherwise, (-(insertion point) - 1). The insertion point is defined as the point 
     * at which the key would be inserted into the vector: 
     * the index of the first element greater than the key, or this.length(), 
     * if all elements in the vector are less than the specified key. Note that this 
     * guarantees that the return value will be >= 0 if and only if the key is found.
     */
    public int binarySearch(int element);
    
    
    /**
     * Appends the specified element to the end of this vector (optional
     * operation). 
     * @effects this.length' = this.length + 1 && this.elements' = this.elements + this.length -> element
     * 
     * @throws UnsupportedOperationException if the <tt>add</tt> method is not
     * 		  supported by this vector.
     * @throws IllegalArgumentException if some aspect of this element
     *            prevents it from being added to this vector.
     */
    public void append(int element);
    
    /**
     * Inserts the specified element at the specified position in this vector
     * (optional operation).  Shifts the element currently at that position
     * (if any) and any subsequent elements to the right (adds one to their
     * indices).
     *
     * @effects this.length' = this.length + 1 && 
     *  this.elements' = { i: [0..this.length'), e: int | i < index => e = this.elements[i], 
     *    i = index => e = element, e = this.elements[i-1] }
     * @throws UnsupportedOperationException if the <tt>add</tt> method is not
     *		  supported by this vector.
     * @throws    IllegalArgumentException if some aspect of the specified
     *		  element prevents it from being added to this vector.
     * @throws    IndexOutOfBoundsException if the index is out of range
     *		  (index &lt; 0 || index &gt; length()).
     */
    public void insert(int index, int element);
    
    /**
     * Appends the specified elements to the end of this vector (optional
     * operation). 
     * @effects this.length' = this.length + vector.length && 
     *  this.elements' = this.elements + { i: [this.length..this.length+vector.length), e: int | e = vector.elements[i-this.length] }
     * 
     * @throws UnsupportedOperationException if the <tt>add</tt> method is not
     * 		  supported by this vector.
     * @throws IllegalArgumentException if some aspect of an element in the given vector
     *            prevents it from being added to this vector.
     */
    public void append(IntVector vector);
    
    /**
     * Inserts the specified elements at the specified position in this vector
     * (optional operation).  Shifts the element currently at that position
     * (if any) and any subsequent elements to the right.
     *
     * @effects this.length' = this.length + vector.length && 
     *  this.elements' = [0..index)<:this.elements + 
     *   { i: [index..index+vector.length), e: int | e = vector.elements[i-index] } + 
     *   { i: [index+vector.length, this.length'), e: int | e = this.elements[i-index-vector.length] } 
     * @throws UnsupportedOperationException if the <tt>add</tt> method is not
     *		  supported by this vector.
     * @throws    IllegalArgumentException if some aspect of an element in the specified
     *		  vector prevents it from being added to this vector.
     * @throws    IndexOutOfBoundsException if the index is out of range
     *		  (index &lt; 0 || index &gt; length()).
     */
    public void insert(int index, IntVector vector);
    
    /**
     * Removes the element at the specified position in this vector (optional
     * operation).  Shifts any subsequent elements to the left (subtracts one
     * from their indices).  Returns the element that was removed from the
     * vector.
     * @return this.elements[index]
     * @effects this.length' = this.length - 1 &&
     *  this.elements' = { i: [0..this.length'), e: int | i < index => e = this.elements[i], 
     *    e = this.elements[i+1] }
     * @throws UnsupportedOperationException if the <tt>remove</tt> method is
     *		  not supported by this vector.
     * @throws IndexOutOfBoundsException if the index is out of range (index
     *            &lt; 0 || index &gt;= length()).
     */
    public int remove(int index);
    
    /**
     * Removes the elements between <tt>fromIndex</tt>, inclusive, 
     * and <tt>toIndex</tt>, exclusive.
     * @effects this.length' = this.length - (toIndex - fromIndex) &&
     * this.elements' = [0..fromIndex)<:this.elements + 
     *  { i: [toIndex..this.length), e: int | this.elements[i] }
     * @throws IndexOutOfBoundsException if fromIndex is out of range (fromIndex
     *            &lt; 0 || fromIndex &gt;= length()) or toIndex is out of range
     *            (toIndex &lt; fromIndex || to &gt; length())
     */
    public void remove(int fromIndex, int toIndex);
    
    /**
     * Compares the specified object with this vector for equality.  Returns
     * <tt>true</tt> if and only if the specified object is also an int vector, both
     * vectors have the same size, and all corresponding pairs of elements in
     * the two vectors are <i>equal</i>.  
     * 
     * @return <tt>true</tt> if the specified object is equal to this vector.
     */
    public boolean equals(Object o);

    /**
     * Returns the hash code value for this vector.  
     *
     * @return the hash code value for this vector.
     * @see Object#hashCode()
     * @see Object#equals(Object)
     * @see #equals(Object)
     */
    public int hashCode();
    
    /**
     * Returns an array containing all of the elements in this vector in proper
     * sequence.
     *
     * @return an array containing all of the elements in this vector in proper
     *	       sequence.
     */
    public int[] toArray();
    
    /**
     * Copies the components of this vector into the specified array. The item at index 
     * k in this vector is copied into component k of the given array. The array must be big enough 
     * to hold all the objects in this vector, else an IndexOutOfBoundsException is thrown.
     * @effects all i: [0..this.length) | array[i] = this.elements[i]
     * @throws IndexOutOfBoundsException - array.length < this.length
     */
    public void copyInto(int[] array);
}
