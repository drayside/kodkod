/**
 * 
 */
package kodkod.util.collections;

import java.util.AbstractSet;
import java.util.Collection;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Set;

/**
 * <p>This is an immutable singleton set implementation that tests for 
 * membership using reference-equality in place of object-equality when comparing elements.  </p>
 * @specfield element: V
 * @author Emina Torlak
 */
public final class SingletonIdentitySet<V> extends AbstractSet<V> {
	private final V element;
	
	/**
	 * Constructs a SingletonIdentitySet that will hold the given element.
	 * @effects this.element' = element
	 */
	public SingletonIdentitySet(V element) {
		this.element = element;
	}
	
	/**
	 * Constructs a SingletonIdentitySet that will hold the first element
	 * returned by the given collection's iterator.
	 * @effects this.element' = collection.iterator().next()
	 * @throws NoSuchElementException - collection.isEmpty()
	 */
	public SingletonIdentitySet(Collection<? extends V> collection) {
		this.element = collection.iterator().next();
	}
	

	/**
	 * Returns true iff this.element and obj are the same object.
	 * @return this.element == obj
	 */
	public boolean contains(Object obj) {
		return element == obj;
	}

	/**
	 * @see java.util.Set#containsAll(java.util.Collection)
	 */
	public boolean containsAll(Collection<?> collection) {
		if (collection.isEmpty()) return true;
		else if (collection.size()==1) return collection.iterator().next()==this.element;
		else return false;
	}

	/**
	 * Returns false.
	 * @return false.
	 */
	public boolean isEmpty() {
		return false;
	}

	/**
	 * @see java.util.Set#iterator()
	 */
	@SuppressWarnings("unchecked")
	public Iterator<V> iterator() {
		return Iterators.iterate(this.element);
	}

	/**
	 * Returns 1.
	 * @return 1
	 */
	public int size() {
		return 1;
	}

	/**
     * Compares the specified object with this set for equality.  Returns
     * <tt>true</tt> if the given object is also a set and the two sets
     * contain identical object-references.  
     *
     * <p><b>Owing to the reference-equality-based semantics of this set it is
     * possible that the symmetry and transitivity requirements of the
     * <tt>Object.equals</tt> contract may be violated if this set is compared
     * to a normal set.  However, the <tt>Object.equals</tt> contract is
     * guaranteed to hold among <tt>SingletonHashSet</tt> instances.</b>
     * 
     * @return <tt>true</tt> if the specified object is equal to this set.
     * @see Object#equals(Object)
     */
    public boolean equals(Object o) {
        if (o == this) {
            return true;
        } else if (o instanceof Set) {
            final Set s = (Set) o;
            return s.size()==1 && s.iterator().next()==this.element;
        } else {
            return false;  
        }
    }
    
    /**
     * Returns 0 if this.element is null; otherwise returns this.element.hashCode().
     * @return this.element = null ? 0 : this.element.hashCode()
     * @see java.util.AbstractSet#hashCode()
     */
    public int hashCode() {
    	return this.element==null ? 0 : element.hashCode();
    }
}
