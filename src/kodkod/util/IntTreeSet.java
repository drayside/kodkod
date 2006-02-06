/**
 * IntTreeSet.java
 * Created on 12:27:00 PM
 */
package kodkod.util;

import java.util.Collection;
import java.util.Iterator;
import java.util.NoSuchElementException;

import kodkod.util.TreeSequence.Entry;


/**
 * An implementation of the IntTreeSet interface based 
 * on a balanced binary search tree.
 * 
 * @specfield ints: set int
 * @author Emina Torlak
 */
public final class IntTreeSet extends AbstractIntSet {
	/* @invariant all e: ints.entries | e.index <= e.value && 
	 *            all e': ints.entries - e | e'.value < e.index - 1 ||
	 *                                       e'.index > e.value + 1
	 */
	private final TreeSequence<MutableInteger> ints;
	private int size;
	
	/**
	 * Constructs an empty int set.
	 * @effects no this.ints'
	 */
	public IntTreeSet() {
		this.ints = new TreeSequence<MutableInteger>();
		this.size = 0;
	}
	
	/**
	 * Constructs a new int set containing the elements
	 * in the specified collection.
	 * @effects this.ints' = c.elements
	 * @throws NullPointerException - c = null
	 */
	public IntTreeSet(Collection<? extends Integer> c) {
		this();
		addAll(c);
	}
	
	/**
	 * Returns the number of integers in this int set.
	 * @return #this.size
	 */
	@Override
	public int size() {
		return size;
	}

	/**
	 * Returns true if this set is empty, otherwise returns false.
	 * @return no this.ints
	 */
	@Override
	public boolean isEmpty() {
		return ints.isEmpty();
	}

	/**
	 * {@inheritDoc}
	 * @see kodkod.util.IntSet#contains(int)
	 */
	public boolean contains(int i) {
		final Entry<MutableInteger> range = ints.floor(i);
		return (range != null && i <= range.value.intValue);
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.util.IntSet#min()
	 */
	public int min() {
		checkNonEmpty();
		return ints.min(ints.root()).index;
	}

	/**
	 * {@inheritDoc}
	 * @see kodkod.util.IntSet#max()
	 */
	public int max() {
		checkNonEmpty();
		return ints.max(ints.root()).value.intValue;
	}

	/**
	 * Returns the smallest element in this set that 
	 * is greater than i.  If this is emtpy or i is greater than this.max(),
	 * NoSuchElementException is thrown.
	 * @return {j: this.ints | j > i && no k: this.ints - j | k < j && k > i}
	 * @throws NoSuchElementException - no this.ints || i >= this.max()
	 * @see kodkod.util.IntSet#successor(int)
	 */
//	public int successor(int i) {
//		Entry<MutableInteger> e = ints.floor(i);
//		if (e==null) 
//			return min();
//		else if (i >= e.value.intValue) {
//			e = ints.successor(e);
//			if (e==null)
//				throw new NoSuchElementException();
//			return e.index;
//		}
//		return i+1;
//	}
	
	/**
	 * Returns the largest element in this set that 
	 * is smaller than i.  If this is emtpy or i is less than this.min(),
	 * NoSuchElementException is thrown.
	 * @return {j: this.ints | j < i && no k: this.ints - j | k > j && k < i}
	 * @throws NoSuchElementException - no this.ints || i <= this.min()
	 * @see kodkod.util.IntSet#predecessor(int)
	 */
//	public int predecessor(int i) {
//		Entry<MutableInteger> e = ints.floor(i);
//		if (e != null && i==e.index) {
//			e = ints.predecessor(e);
//		}
//		if (e==null)
//			throw new NoSuchElementException();
//		return e.value.intValue < i ? e.value.intValue : i-1;
//	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public IntIterator iterator() {
		return new AscendingIterator(Integer.MIN_VALUE,Integer.MAX_VALUE);
	}
	
	/**
	 * Returns the elements in this.set that are in the closed range
	 * [from..to].
	 * The returned view supports all the mutating operations as this
	 * set, with the exception that an attempt to add to it an element
	 * out of the the range [from..to] will cause an IllegalArgumentException.
	 * @return s: IntSet | s.ints = { j: this.ints | from <= j <= to }
	 * @throws IllegalArgumentException - from > to
	 */
	public IntIterator iterator(int from, int to) {
		return from > to ? new DescendingIterator(from, to) : new AscendingIterator(from,to);
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.util.IntSet#copy()
	 */
	public IntSet copy() {
		return new IntTreeSet(this);
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.util.IntSet#add(int)
	 */
	public boolean add(int i) {
		final Entry<MutableInteger> floor = ints.floor(i);

		if (floor==null || floor.value.intValue < i) {
			final Entry<MutableInteger> ceil = ints.ceil(i);
			int key = i, value = i;
			
			if (floor != null && floor.value.intValue == i - 1)  {
				key = floor.index;
			} 		
			if (ceil != null && ceil.index == i + 1) {
				value = ints.remove(ceil.index).intValue;
			} 
		
			if (key != i) floor.value.intValue = value;
			else ints.put(key, new MutableInteger(value));

			size++;
			return true;
		}	
		
		return false;
		
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.util.IntSet#remove(int)
	 */
	public boolean remove(int i) {
		final Entry<MutableInteger> floor = ints.floor(i);
		
		if (floor!=null && i <= floor.value.intValue) {
			if (floor.index==floor.value.intValue) {
				ints.remove(floor.index);
			} else if (i==floor.index) {
				ints.put(i+1, ints.remove(floor.index));
			} else if (i==floor.value.intValue) {
				floor.value.intValue = i-1;
			} else {
				ints.put(i+1, new MutableInteger(floor.value.intValue));
				floor.value.intValue = i-1;
			}
			size--;
			return true;
		}
		return false;
	}
	
	/**
	 * Returns true if c is a collection of integers and
	 * all its elements are contained in this.
	 * @return c.elements in this.ints
	 * @throws NullPointerException - c = null || null in c.elements
	 * @throws ClassCastException - some e: c.elements | c !in Integer
	 */
	public boolean containsAll(Collection<?> c) {
		if (c instanceof IntTreeSet) {
			IntTreeSet s = (IntTreeSet) c;
			if (size>=s.size) {
				for(IndexedEntry<MutableInteger> srange : s.ints) {
					Entry<MutableInteger> floor = ints.floor(srange.index());
					if (floor==null || floor.value.intValue<srange.value().intValue) return false;
				}
				return true;
			}
			return false;
		}
		return super.containsAll(c);
	}

	/**
	 * Adds the ints from min to max, inclusive, to this intset.
	 * @requires min <= max 
	 * @effects this.ints' = this.ints + {i: int |  min<=t.index()<=max }
	 */
	private void addRange(int min, int max) {
		final Entry<MutableInteger> minFloor = ints.floor(min);
		
		if (minFloor==null || minFloor.value.intValue < max) {
			
			int key = min, value = max, sizeDelta = value - key + 1;
			
			if (minFloor != null && min <= minFloor.value.intValue + 1) {
				key = minFloor.index;
				sizeDelta -= (minFloor.value.intValue - min + 1);
			}
			
			Entry<MutableInteger> succ = ints.successor(key);
			if (succ!=null) {
				while(succ!=ints.NIL && succ.value.intValue < max) {
					ints.remove(succ.index);
					sizeDelta -= (succ.value.intValue - succ.index + 1);
					succ = ints.successor(succ);
				}
				
				if (succ!=ints.NIL && succ.index <= max + 1) {
					value = succ.value.intValue;
					sizeDelta -= (max - succ.index + 1);
					ints.remove(succ.index);
				}	
			}
			
			if (minFloor!=null && minFloor.index==key) 
				minFloor.value.intValue=value;
			else 
				ints.put(key, new MutableInteger(value));		
			size += sizeDelta;
		}	
		
	}
	/**
	 * Returns true if all ints in this are smaller than then
	 * the smallest tuple in s.  Otherwise returns false.
	 * @requires s != null && s.size > 0 && this.size > 0
	 * @return all t: Tuple | t in this.ints => t.index() < min(s.ints.index())
	 */
	private boolean precedes(IntTreeSet s) {
		return ints.last().value.intValue < s.ints.first().index;
	}
	
	/**
	 * Adds all of the elements in the specified collection to this set 
	 * if they're not already presen. If the specified 
	 * collection is also a set, the addAll operation effectively modifies 
	 * this set so that its intValue is the union of the two sets.
	 * @effects this.ints' = this.ints + c.elements 
	 * @return this.ints' != this.ints
	 * @throws NullPointerException - c = null || null in c.elements
	 */
	public boolean addAll(Collection<? extends Integer> c) {
		if (c instanceof IntTreeSet) {
			IntTreeSet s = (IntTreeSet) c;
			if (!s.isEmpty()) {
				if (isEmpty() || this.precedes(s) || s.precedes(this)) {
					ints.putAll(s.ints);
					size += s.size();
					return true;
				} else {
					final int oldSize = size;
					for(IndexedEntry<MutableInteger> srange : s.ints) {
						addRange(srange.index(), srange.value().intValue);
					}
					return oldSize != size;
				}
			}
			return false;
		}
		return super.addAll(c);
	}

	/**
	 * Retains only the elements in this set that are contained in the specified 
	 * collection. In other words, removes from this set all of its elements that
	 * are not contained in the specified collection. If the specified collection 
	 * is also a set, this operation effectively modifies this set so that its intValue 
	 * is the intersection of the two sets.
	 * @effects this.ints' = this.ints & c.elements
	 * @return this.ints != this.ints'
	 * @throws NullPointerException - c = null || null in c.elements
	 * @throws ClassCastException - some e: c.elements | e !in Integer
	 */
	public boolean retainAll(Collection<?> c) {
		if (c instanceof IntTreeSet) {
			IntTreeSet s = (IntTreeSet) c;
			if (!isEmpty()) {
				if (s.isEmpty() || this.precedes(s) || s.precedes(this)) {
					clear();
					return true;
				} else {
					final int oldSize = size;
					final Entry<MutableInteger> first = s.ints.first();
					int lastMax = first.index==Integer.MIN_VALUE ? 
							      first.value.intValue + 1 : Integer.MIN_VALUE;
					for(Iterator<IndexedEntry<MutableInteger>> sIter = s.ints.iterator(lastMax, Integer.MAX_VALUE); 
					    sIter.hasNext();) {
						IndexedEntry<MutableInteger> srange = sIter.next();
						removeRange(lastMax, srange.index()-1);
						lastMax = srange.value().intValue + 1;
					}
					if (s.ints.last().value.intValue < Integer.MAX_VALUE) 
						removeRange(lastMax, Integer.MAX_VALUE);
					return oldSize != size;
				}
			}
			return false;
		}
		return super.retainAll(c);
	}

	/**
	 * Removes the ints whose indices range from min to max, inclusive, from this tupleset.
	 * @requires min <= max 
	 * @effects this.ints' = this.ints - {i: int |  min<=i<=max }
	 */
	private void removeRange(int min, int max) {
		int sizeDelta = 0;
		
		final Entry<MutableInteger> minFloor = ints.floor(min);
		
		if (minFloor!=null && min<=minFloor.value.intValue) {
			sizeDelta += (minFloor.value.intValue - min + 1);
			if (minFloor.index==minFloor.value.intValue) 
				ints.remove(minFloor.index);
			else 
				minFloor.value.intValue = min-1;
		}
		
		Entry<MutableInteger> succ = ints.successor(min);
		if (succ != null) {
			while(succ!=ints.NIL && succ.value.intValue <= max) {
				sizeDelta += (succ.value.intValue - succ.index + 1);
				ints.remove(succ.index);
				succ = ints.successor(succ);
			}
			
			if (succ!=ints.NIL && succ.index<=max) {
				sizeDelta += (max - succ.index + 1);
				ints.put(max + 1, ints.remove(succ.index));
			}
		}
		
		size -= sizeDelta;
	}
	
	/**
	 * Removes all elements in the given collection from this.  If
	 * c is also a set, this method implements the set difference
	 * operator.
	 * @effects this.ints' = this.ints - c.elements
	 * @return this.ints != this.ints'
	 * @throws NullPointerException - c = null || null in c.elements
	 * @throws ClassCastException - some e: c.elements | e !in Integer
	 */
	public boolean removeAll(Collection<?> c) {
		if (c instanceof IntTreeSet) {
			IntTreeSet s = ((IntTreeSet) c);
			if (!s.isEmpty() && !isEmpty()) {
				if (this.precedes(s) || s.precedes(this)) return false;
				else {
					final int oldSize = size;
					for(IndexedEntry<MutableInteger> srange : s.ints) {
						removeRange(srange.index(), srange.value().intValue);
					}
					return oldSize != size;
				}
			}
			return false;
		}
		return super.removeAll(c);
	}

	/**
	 * Removes all integers from this set.
	 * @effects no this.ints'
	 */
	public void clear() {
		ints.clear();
		size = 0;
	}
	
	/**
	 * Implementation of an ascending iterator over (a subset of) this set.
	 */
	private final class AscendingIterator implements IntIterator {
		Entry<MutableInteger> next;
		final int endIndex;
		int endpoint, cursor, lastReturned;
		
		/**
		 * Constructs an ascending iterator that returns elements between
		 * from and to.  
		 * @requires from <= to
		 */
		AscendingIterator(int from, int to) {
			endIndex = to;
			lastReturned = -1;
			next = ints.floor(from);
			if (next != null && from <= next.value.intValue) {
				cursor = from;
				endpoint = next.value.intValue;
				next = ints.successor(next);
			} else {
				next = ints.ceil(from);
				if (next==null) next = ints.NIL;
				cursor = 0;
				endpoint = -1;
			}
		}
		
		public boolean hasNext() {
			if (cursor > endpoint) {
				if (next==ints.NIL) return false;
				this.cursor = next.index;
				this.endpoint = next.value.intValue; 
				next = ints.successor(next);
			}
			return cursor <= endIndex;
		}

		public int nextInt() {
			if (!hasNext()) throw new NoSuchElementException();
			lastReturned = cursor++;
			return lastReturned;
		}

		public Integer next() {
			return nextInt();
		}

		public void remove() {
			if (lastReturned < 0) throw new IllegalStateException();
			IntTreeSet.this.remove(lastReturned);
			next = ints.successor(cursor);
			if (next==null) next = ints.NIL;
			lastReturned = -1;
		}
		
	}
	
	/**
	 * Implementation of a descending iterator over (a subset of) this set.
	 */
	private final class DescendingIterator implements IntIterator {
		Entry<MutableInteger> next;
		final int endIndex;
		int endpoint, cursor, lastReturned;
		
		/**
		 * Constructs a descending iterator that returns elements between
		 * from and to.  
		 * @requires from >= to
		 */
		DescendingIterator(int from, int to) {
			endIndex = to;
			lastReturned = -1;
			next = ints.floor(from);
			if (next==null) next = ints.NIL;
			else if (from <= next.value.intValue) {
				cursor = from;
				endpoint = next.index;
				next = ints.predecessor(next);
			} else {
				cursor = -1;
				endpoint = 0;
			}
		}
		
		public boolean hasNext() {
			if (cursor < endpoint) {
				if (next==ints.NIL) return false;
				this.cursor = next.value.intValue;
				this.endpoint = next.index; 
				next = ints.predecessor(next);
			}
			return cursor >= endIndex;
		}

		public int nextInt() {
			if (!hasNext()) throw new NoSuchElementException();
			lastReturned = cursor--;
			return lastReturned;
		}

		public Integer next() {
			return nextInt();
		}

		public void remove() {
			if (lastReturned < 0) throw new IllegalStateException();
			IntTreeSet.this.remove(lastReturned);
			next = ints.predecessor(cursor);
			if (next==null) next = ints.NIL;
			lastReturned = -1;
		}
		
	}
	
	/**
	 * A mutable wrapper for a primitive int.
	 */
	private static final class MutableInteger {
		int intValue;
		
		/**
		 * Constructs a mutable wrapper for the given intValue.
		 */
		MutableInteger(int i) {
			intValue = i;
		}
		
		public boolean equals(Object o) {
			if (o==this) return true;
			if (o instanceof MutableInteger) {
				return ((MutableInteger)o).intValue==intValue;
			}
			return false;
		}
		
		public int hashCode() {
			return intValue;
		}
		
		public String toString() {
			return String.valueOf(intValue);
		}
	}
}
