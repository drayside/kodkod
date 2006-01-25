package kodkod.util;

import java.util.Iterator;

/**
 * Represents an iterator over int primitives. 
 * 
 * @author Emina Torlak
 */
public interface IntIterator extends Iterator<Integer> {

	/**
	 * Returns true if this iteration has more elements.
	 * @return true if this iteration has more elements.
	 */
	public abstract boolean hasNext();
	
	/**
	 * Returns the next int in the iteration, if any.
	 * Otherwise throws a NoSuchElementException.
	 * @return the next element in the iteration
	 * @throws java.util.NoSuchElementException - the iteration is empty. 
	 */
	public abstract int nextInt();
	
	/**
	 * Returns the next integer in the iteration, if any.
	 * Otherwise throws a NoSuchElementException.
	 * @return the next element in the iteration
	 * @throws java.util.NoSuchElementException - the iteration is empty. 
	 */
	public abstract Integer next();
	
	/**
	 * Removes the last returned element from the underlying collection.
	 * @effects removes the last returned element from the underlying collection.
	 * @throws UnsupportedOperationException - removal is not supported
	 * @throws IllegalStateException - next() has not been called yet or remove()
	 * has already been called since the last call to next().
	 */
	public abstract void remove();
	
}
