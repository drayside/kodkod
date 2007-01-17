/**
 * 
 */
package kodkod.util.collections;


/**
 * An index generator for a set of keys.  An indexer maps each key in its keyset 
 * to a unique integer.  
 * @specfield keys: set K
 * @specfield indices: keys lone->one int
 * @author Emina Torlak
 */
public interface Indexer<K> {

	/**
	 * Returns the index of the given key, if it is in this.keys.
	 * Otherwise returns a negative number.
	 * @return key in this.keys => this.indices[key], {i: int | i < 0 }
	 */
	public abstract int indexOf(K key);
	
	/**
	 * Returns the key at the given index.
	 * @return this.indices.index
	 * @throws IndexOutOfBoundsException - index !in this.indices[this.keys]
	 */
	public abstract K keyAt(int index);
	
	/**
	 * Returns the number of keys in this.indexer.
	 * @return #this.keys
	 */
	public abstract int size();
	
	
}
