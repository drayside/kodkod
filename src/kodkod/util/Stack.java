package kodkod.util;

import java.util.EmptyStackException;
import java.util.Iterator;


/**
 * Represents a last-in-first-out (LIFO) stack of objects. 
 * The usual push and pop operations are provided, as well as a method to peek at the top item on the stack, 
 * a method to test for whether the stack is empty, and an iterator over the elements in the stack 
 * (that does not support removal).  When a stack is first created, it contains no items. 
 * 
 * @specfield size: int
 * @specfield elems: [0..size)->one T
 * @author Emina Torlak
 */
public abstract class Stack<T> implements Iterable<T> {

	/**
	 * Constructs a new stack.
	 */
	protected Stack() {}
	
	/**
	 * Returns the size of this stack.
	 * @return this.size
	 */
	public abstract int size();
	
	/**
	 * Pushes an item onto the top of this stack and returns it. 
	 * @effects this.size' = this.size + 1 && this.elems'[0] = item &&
	 *          all i: [0..this.size) | this.elems'[i+1] = this.elems[i]
	 * @return item
	 */
	public abstract T push(T item);
	
	/**
	 * Removes the object at the top of this stack and returns that object as the value of this function.
	 * @effects this.size' = this.size - 1 && 
	 *          all i: [1..this.size) | this.elems'[i-1] = this.elems[i]
	 * @return this.elems[0]
	 * @throws EmptyStackException - no this.elems
	 */
	public abstract T pop();

	/**
	 * Looks at the object at the top of this stack without removing it from the stack.
	 * @return this.elems[0]
	 * @throws EmptyStackException - no this.elems
	 */
	public abstract T peek();

	/**
	 * Returns the 1-based position where an object is on this stack. 
	 * If the object o occurs as an item in this stack, this method 
	 * returns the distance from the top of the stack of the occurrence 
	 * nearest the top of the stack; the topmost item on the stack is 
	 * considered to be at distance 1. The equals method is used to 
	 * compare o to the items in this stack.
	 * @return o in this.elems[int] => min(this.elems.o) + 1, -1
	 */
	public abstract int search(Object o);

	/**
	 * Returns true if the stack is empty; otherwise returns false.
	 * @return no this.elems
	 */
	public abstract boolean empty();

	/**
	 * Iterates over the items in this LinkedStack, starting
	 * at the top of the stack and working its way down.
	 * @return iterator over the elements in this stack.
	 */
	public abstract Iterator<T> iterator();
	
	/**
	 * Returns true if both o1 and o2 are null, or 
	 * if they are non-null and 'equals' to each other.
	 * @return o1=null && o2=null || o1.equals(o2)
	 */
	static boolean equal(Object o1, Object o2) {
		return (o1==null ? o2==null : o1.equals(o2));
	}
	
	/**
	 * Returns true if o is a stack containing the same elements
	 * as this stack, in the same order.
	 * @return o in Stack  && this.elems = o.elems
	 */
	@SuppressWarnings("unchecked")
	public boolean equals(Object o) {
		if (this==o) return true;
		else if (o instanceof Stack) {
			final Stack<T> s = (Stack<T>) o;
			if (size() != s.size()) return false;
			final Iterator<T> iter0 = iterator(), iter1 = s.iterator();
			while(iter0.hasNext()) {
				if (!equal(iter0.next(), iter1.next()))
					return false;
			}
			return true;
		}
		return false;
	}
	
	/**
	 * Returns the hashcode for this stack.
	 * @return the hashcode for this stack.
	 */
	public int hashCode() {
		int code = 0;
		for(T item : this) {
			if (item!=null) code += item.hashCode(); 
		}
		return code;
	}
	
	/**
	 * Returns a string represention of this stack.
	 */
	public String toString() {
		final StringBuilder buffer = new StringBuilder("[ ");
		final Iterator<T> elems = iterator();
		if (elems.hasNext()) buffer.append(elems.next());
		while(elems.hasNext()) {
			buffer.append(", ");
			buffer.append(elems.next());
		}
		buffer.append(" ]");
		return buffer.toString();
	}
	

}