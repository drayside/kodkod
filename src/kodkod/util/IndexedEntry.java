package kodkod.util;

/**
 * Represents an entry in a {@link SparseSequence sparse sequence}.
 * 
 * @specfield index: int
 * @specfield value: E
 * 
 * @author Emina Torlak
 */
public interface IndexedEntry<E> {
	
	/**
	 * Returns the index of this entry.
	 * @return this.index
	 */
	public abstract int index();
	
	/**
	 * Returns the value stored in this entry.
	 * @return this.value
	 */
	public abstract E value();
	
	/**
	 * Compares the specified object with this entry for equality. Returns true if
	 * the given object is also an indexed entry and the two entries 
	 * have the same indeces and values. More formally, two entries e1 and e2 
	 * are equal if e1.index = e2.index && e1.value = e2.value.  This ensures 
	 * that the equals method works properly across different implementations of 
	 * the IndexedEntry interface.
	 * @return o in IndexedEntry && o.index = this.index && o.value = this.value
	 */
	public abstract boolean equals(Object o);
	
	/**
	 * Returns the hash code value for this indexed entry. The hash code of an 
	 * indexed entry e is defined to be:
	 * e.index ^ (e.value=null ? 0 : e.value.hashCode()).
	 * This ensures that e1.equals(e2) implies that e1.hashCode()==e2.hashCode() 
	 * for any two IndexedEntries e1 and e2, as required by the general contract of 
	 * Object.hashCode.
	 */
	public abstract int hashCode();
}
