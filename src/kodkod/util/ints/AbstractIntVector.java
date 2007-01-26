/**
 * 
 */
package kodkod.util.ints;

import java.util.NoSuchElementException;


/**
 * A skeleton implementation of the IntVector interface.   
 *
 * 
 * @specfield length: int
 * @specfield elements: [0..size) ->one int
 * 
 * @author Emina Torlak
 */
public abstract class AbstractIntVector implements IntVector {
	
    /**
     * {@inheritDoc}
     * @see kodkod.util.ints.IntVector#isEmpty()
     */
    public boolean isEmpty() { return length()==0; }

    /**
     * {@inheritDoc}
     * @see kodkod.util.ints.IntVector#contains(int)
     */
    public boolean contains(int element) {
    	for(int i = 0, max = length(); i < max; i++) {
    		if (get(i)==element) return true;
    	}
    	return false;
    }
	
    /**
     * Throws {@link UnsupportedOperationException}.
     * @see kodkod.util.ints.IntVector#set(int,int)
     */
    public int set(int index, int element) {
    	throw new UnsupportedOperationException();
    }
    
   /**
    * Calls this.remove(0, length()).
    * @see kodkod.util.ints.IntVector#clear()
    * @see kodkod.util.ints.IntVector#remove(int, int)
    */
    public void clear() { remove(0, length()); }
    
    /**
     * {@inheritDoc}
     * @see kodkod.util.ints.IntVector#indexOf(int)
     */
    public int indexOf(int element) {
    	for(int i = 0, length=length(); i < length; i++) {
			if (get(i)==element) return i;
		}
		return -1;
    }

   /**
    * {@inheritDoc}
    * @see kodkod.util.ints.IntVector#lastIndexOf(int)
    */
    public int lastIndexOf(int element) {
    	for(int i = length()-1; i >= 0; i--) {
			if (get(i)==element) return i;
		}
		return -1;
    }
    
    /**
     * {@inheritDoc}
     * @see kodkod.util.ints.IntVector#min()
     */
    public int min() {
    	final int length = length();
    	if (length==0) throw new NoSuchElementException();
		int min = Integer.MAX_VALUE;
		for(int i = 0; i < length; i++) {
			int element = get(i);
			if (element < min) { 
				min = element;
			}
		}
		return min;
    }
    
    /**
     * {@inheritDoc}
     * @see kodkod.util.ints.IntVector#max()
     */
    public int max() {
    	final int length = length();
    	if (length==0) throw new NoSuchElementException();
		int max = Integer.MIN_VALUE;
		for(int i = 0; i < length; i++) {
			int element = get(i);
			if (element > max) { 
				max = element;
			}
		}
		return max;
    }

    /**
     * Throws {@link UnsupportedOperationException}
     * @see kodkod.util.ints.IntVector#sort()
     */
    public void sort() { throw new UnsupportedOperationException(); }
    
   /**
    * {@inheritDoc}
    * @see kodkod.util.ints.IntVector#binarySearch(int)
    */
    public int binarySearch(int element) {
    	int low = 0;
		int high = length()-1;

		while (low <= high) {
			int mid = (low + high) >>> 1;
			int midVal = get(mid);		
			if (midVal < element)
				low = mid + 1;
			else if (midVal > element)
				high = mid - 1;
			else { 
				return mid;
			}
		}

		return -(low + 1);  // key not found.
    }
    
    /**
     * Calls this.insert(this.length(), element)
     * @see kodkod.util.ints.IntVector#append(int)
     * @see kodkod.util.ints.IntVector#insert(int,int)
     */
    public void append(int element) { insert(length(),element); }
    
    /**
     * Calls this.insert(this.length(), array)
     * @see kodkod.util.ints.IntVector#append(kodkod.util.ints.IntVector)
     * @see kodkod.util.ints.IntVector#insert(int, kodkod.util.ints.IntVector)
     */
    public void append(IntVector array) { insert(length(), array); }
    
   /**
    * Throws {@link UnsupportedOperationException}
    * @see kodkod.util.ints.IntVector#insert(int, int)
    */
    public void insert(int index, int element) { throw new UnsupportedOperationException(); }
    
    /**
     * Throws {@link UnsupportedOperationException}
     * @see kodkod.util.ints.IntVector#insert(int, kodkod.util.ints.IntVector)
     */
    public void insert(int index, IntVector array) { throw new UnsupportedOperationException(); }
    
    /**
     * Stores this.elements[index] in a temporary variables.  Calls this.remove(index, index+1),
     * and returns the value of the temporary variable.
     * @see kodkod.util.ints.IntVector#remove(int)
     */
    public int remove(int index) { 
    	final int oldValue = get(index);
    	remove(index, index+1);
    	return oldValue;
    }
    
    /**
     * Throws {@link UnsupportedOperationException}.
     * @see kodkod.util.ints.IntVector#remove(int, int)
     */
    public void remove(int offset, int length) { throw new UnsupportedOperationException(); }
    
    /**
     * Calls this.iterator(0, length())
     * @see kodkod.util.ints.IntVector#iterator()
     * @see kodkod.util.ints.IntVector#iterator(int,int)
     */
    public IntIterator iterator() {
    	return iterator(0, length());
    }
    
    /**
     * {@inheritDoc}
     * @see kodkod.util.ints.IntVector#iterator(int, int)
     */
    public IntIterator iterator(int fromIndex, int toIndex) {
    	return fromIndex<=toIndex ? new AscendingIntVectorIterator(fromIndex,toIndex) : 
			new DescendingIntVectorIterator(fromIndex,toIndex);
    }
    
    /**
     * Creates a temporary array <tt>t<\tt> of length this.length,
     * calls this.copyInto(t), and returns <tt>t<\tt>.
     * @see kodkod.util.ints.IntVector#toArray()
     * @see kodkod.util.ints.IntVector#copyInto(int[])
     */
    public int[] toArray() {
    	final int[] ret = new int[length()];
    	copyInto(ret);
    	return ret;
    }
    
    /**
     * {@inheritDoc}
     * @see kodkod.util.ints.IntVector#copyInto(int[])
     */
    public void copyInto(int[] array) {
    	for(int i = 0, length = length(); i < length; i++) {
    		array[i] = get(i);
    	}
    }
    
    /**
	 * {@inheritDoc}
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		int hashCode = 1;
		for(int i = 0, length = length(); i < length; i++) {
			hashCode = 31*hashCode + get(i);
		}
		return hashCode;
	}

	/**
	 * {@inheritDoc}
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object o) {
		if (o==this) return true;
		if (o instanceof IntVector) {
			final IntVector l = (IntVector) o;
			final int length = length();
			if (l.length()==length) {
				for (int i = 0; i < length; i++) {
					if (get(i) != l.get(i)) return false;
				}
				return true;
			}
		}
		return false;
	}
	
	/**
	 * {@inheritDoc}
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		final StringBuilder buf = new StringBuilder();
		buf.append("[");
		IntIterator itr = iterator();
		if (itr.hasNext()) buf.append(itr.nextInt());
		while(itr.hasNext()) {
			buf.append(", ");
			buf.append(itr.nextInt());
		}
		buf.append("]");
		return buf.toString();
	}

    
    private abstract class IntVectorIterator implements IntIterator {
		int next, end, last;
		IntVectorIterator(int fromIndex, int toIndex) {
			next = fromIndex;
			end = toIndex;
			last = -1;
		}
		public final Integer next() { return nextInt(); }
		public final void remove() {
			if (last < 0) throw new IllegalStateException();
			AbstractIntVector.this.remove(last);
			next = last;
			last = -1;
		}
	}
	
	private final class AscendingIntVectorIterator extends IntVectorIterator {
		/**
		 * Constructs a new AscendingIntArrayIterator.
		 * @requires fromIndex <= toIndex
		 */
		AscendingIntVectorIterator(int fromIndex, int toIndex) {
			super(fromIndex, toIndex);
		}
		public boolean hasNext() { return next < end; }
		public int nextInt() {
			if (!hasNext()) throw new NoSuchElementException();
			last = next++;
			return get(last);
		}
	}
	
	private final class DescendingIntVectorIterator extends IntVectorIterator {
		/**
		 * Constructs a new AscendingIntArrayIterator.
		 * @requires fromIndex >= toIndex
		 */
		DescendingIntVectorIterator(int fromIndex, int toIndex) {
			super(fromIndex, toIndex);
		}
		public boolean hasNext() { return next > end; }
		public int nextInt() {
			if (!hasNext()) throw new NoSuchElementException();
			last = next--;
			return get(last);
		}
	}
}
