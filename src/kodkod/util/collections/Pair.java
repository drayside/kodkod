/**
 * 
 */
package kodkod.util.collections;

/**
 * A container class consisting of two elements of types.
 * @specfield first: F
 * @specfield second: S
 * @author Emina Torlak
 */
public final class Pair<F, S> {
	private final F first;
	private final S second;
	
	/**
	 * Constructs a pair out of the given elements.
	 * @effects this.first' = first and this.second' = second
	 */
	public Pair(F first, S second) {
		this.first = first;
		this.second = second;
	}
	
	/**
	 * Returns this.first.
	 * @return this.first
	 */
	public F first() {
		return first;
	}
	
	/**
	 * Returns this.second.
	 * @return this.second
	 */
	public S second() {
		return second;
	}
	
	/**
	 * Returns true if o is a Pair with first and last
	 * <tt>equals</tt> to this first and second.
	 * @return this.first.equals(o.first) and this.second.equals(o.second)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object o) {
		if (this==o) return true;
		else if (o instanceof Pair) {
			final Pair p = (Pair) o;
			return (first==null ? p.first==null : first.equals(p.first)) &&
			       (second==null ? p.second==null : second.equals(p.second));
		} else return false;
	}
	/**
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		return (first==null ? 0 : first.hashCode()) + (second==null ? 0 : second.hashCode());
	}
	
	/**
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return "<" + first + ", " + second + ">";
	}
}
