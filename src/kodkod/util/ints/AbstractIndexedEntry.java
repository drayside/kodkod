/**
 * AbstractIndexedEntry.java
 * Created on 8:00:49 PM
 */
package kodkod.util.ints;

/**
 * A skeletal implementation of the IndexedEntry interface.
 * 
 * @specfield index: int
 * @specfield value: V
 * 
 * @author Emina Torlak
 */
public abstract class AbstractIndexedEntry<V> implements IndexedEntry<V> {

	final int index;
	V value;
	
	/**
	 * Constructs an indexed entry with the given index and value
	 * @effects this.index' = index && this.value' = value
	 */
	protected AbstractIndexedEntry(int index, V value) {
		this.index = index;
		this.value = value;
	}
	
	/**
	 * @see kodkod.util.ints.IndexedEntry#index()
	 */
	public int index() {
		return index;
	}

	/**
	 * @see kodkod.util.ints.IndexedEntry#value()
	 */
	public V value() {
		return value;
	}
	

	/**
	 * Sets this.value to the given value and returns the previous value.
	 * @effects this.value' = value
	 * @requires this.value
	 */
	V setValue(V value) {
		V oldValue = this.value;
		this.value = value;
		return oldValue;
	}
	
	/**
	 * Compares the specified object with this entry for equality. Returns true if
	 * the given object is also an indexed entry and the two entries 
	 * have the same indeces and values. More formally, two entries e1 and e2 
	 * are equal if e1.index = e2.index && e1.value = e2.value.  This ensures 
	 * that the equals method works properly across different implementations of 
	 * the IndexedEntry interface.
	 * @return o in IndexedEntry && o.index = this.index && o.value = this.value
	 */
	public boolean equals(Object o) {
		if (!(o instanceof IndexedEntry)) return false;
		if (o==this) return true;
		final IndexedEntry<?> e = (IndexedEntry<?>) o;
		if (index==e.index()) {
			return value==null ? e.value()==null : value.equals(e.value());
		}
		return false;
	}
	
	/**
	 * Returns the hash code value for this indexed entry. The hash code of an 
	 * indexed entry e is defined to be:
	 * e.index ^ (e.value=null ? 0 : e.value.hashCode()).
	 * This ensures that e1.equals(e2) implies that e1.hashCode()==e2.hashCode() 
	 * for any two IndexedEntries e1 and e2, as required by the general contract of 
	 * Object.hashCode.
	 */
	public int hashCode() {
		return index ^ (value==null ? 0 : value.hashCode());
	}
	
	public String toString() {
		return index + "=" + value;
	}

}
