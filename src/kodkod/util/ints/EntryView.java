/**
 * 
 */
package kodkod.util.ints;

/**
 * A mutable IndexedEntry.  This class provides
 * various convience method for changing the entry state.
 * @author Emina Torlak
 */
class EntryView<V> implements IndexedEntry<V> {
	private int index;
	private V value;
	
	/**
	 * Constructs a new entry view with the given index and value.
	 * @effects this.index' = index and this.value' = value
	 */
	EntryView(int index, V value) {
		this.index = index;
		this.value = value;
	}
	
	
	/**
	 * Sets this.index to the given index, 
	 * and returns the old index.
	 * @effects this.index' = newIndex
	 * @return this.index
	 */
	int setIndex(int newIndex) {
		final int oldIndex = this.index;
		this.index = newIndex;
		return oldIndex;
	}
	
	/**
	 * Sets this.value to the given value, 
	 * and returns the old value.
	 * @effects this.value' = newValue
	 * @return this.value
	 */
	V setValue(V newValue) {
		final V oldValue = this.value;
		this.value = newValue;
		return oldValue;
	}
	
	/**
	 * Sets this.index and this.value to the given
	 * index and value, and returns this.
	 * @effects this.index' = newIndex && this.value' = newValue
	 * @return this
	 */
	IndexedEntry<V> setView(int newIndex, V newValue) {
		this.index = newIndex;
		this.value = newValue;
		return this;
	}
	
	/**
	 * Sets this.index to the given
	 * index, and returns this.
	 * @effects this.index' = newIndex 
	 * @return this
	 */
	IndexedEntry<V> setIndexView(int newIndex) {
		this.index = newIndex;
		return this;
	}

	/**
	 * Sets this.value to the given
	 * value, and returns this.
	 * @effects this.value' = newValue
	 * @return this
	 */
	IndexedEntry<V> setValueView(V newValue) {
		this.value = newValue;
		return this;
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
	 * @see java.lang.Object#toString()
	 */
	public final String toString() {
		return index + "=" + value;
	}
	
	/**
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public final boolean equals(Object o) {
		if (o==this) return true;
		if (!(o instanceof IndexedEntry)) return false;
		return AbstractSparseSequence.equal(this, (IndexedEntry<?>)o);
	}
	
	/**
	 * @see java.lang.Object#hashCode()
	 */
	public final int hashCode() {
		return AbstractSparseSequence.hashCode(this);
	}	
}
