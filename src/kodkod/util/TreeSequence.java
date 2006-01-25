/**
 * TreeSequence.java
 * Created on 9:51:24 AM
 */
package kodkod.util;

import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * An implementation of a sparse sequence based on a
 * balanced binary search tree. 
 * 
 * @author Emina Torlak
 */
public final class TreeSequence<V> extends AbstractSparseSequence<V> {
	final Entry<V> NIL;
	private static final boolean BLACK = true, RED = false;
	private Entry<V> root;
	private int size;
	
	/**
	 * Constructs an empty tree sequence.
	 * @effects no this.entries'
	 */
	public TreeSequence() {
		this.NIL = new Entry<V>();
		this.root = NIL;
		this.size = 0;
	}
	
	/**
	 * Constructs a new tree sequence with the same index/value mappings
	 * as the given sequence.
	 * @effects this.entries' = s.entries
	 * @throws NullPointerException - s = null
	 */
	public TreeSequence(SparseSequence<? extends V> s) {
		this();
		putAll(s);
	}
	
	/**
	 * Returns the root of this tree.
	 * @return this.root
	 */
	Entry<V> root() { return root; }
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.util.SparseSequence#size()
	 */
	public int size() {
		return size;
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.util.SparseSequence#clear()
	 */
	public void clear() {
		root = NIL;
		size = 0;
	}
	
	/**
	 * Searches for a node with the given key in the subtree
	 * rooted at the given node.  If no such node exists, returns NIL.
	 * The behavior of this method is unspecified if the given node
	 * is not in this tree.
	 * @requires node in this.root.*(left + right)
	 * @return node.*(left + right).key
	 * @throws NullPointerException - node = null
	 */
	Entry<V> search(Entry<V> node, int index) {
		while(node != NIL) {
			if (index==node.index) break;
			else if (index<node.index) node = node.left;
			else node = node.right;
		}
		return node;
	}
	
	/**
	 * Returns the node with the smallest index in the subtree rooted at start.
	 * The behavior of this method is unspecified if the given node
	 * is not in this tree.
	 * @requires node in this.root.*(left + right)
	 * @return {n: start.*left | no n.left }
	 */
	Entry<V> min(Entry<V> start) {
		if (start != NIL) {
			while(start.left != NIL) {
				start = start.left;
			}
		}
		return start;
	}
	
	/**
	 * Returns the node with the largest index in the subtree rooted at start.
	 * The behavior of this method is unspecified if the given node
	 * is not in this tree.
	 * @requires node in this.root.*(left + right)
	 * @return {n: start.*left | no n.right }
	 */
	Entry<V> max(Entry<V> start) {
		if (start != NIL) {
			while(start.right != NIL) {
				start = start.right;
			}
		}
		return start;
	}
	
	/**
	 * Implementation of the tree-predecessor algorithm from CLR.
	 * Returns the node containing the given node's predecessor, if it exists.  
	 * Otherwise returns NIL.  The behavior of this method is unspecified if the
	 * given node is not in this tree.
	 * @requires node in this.root.*(left + right)
	 * @return {n: Node | n.value = this.entries[n.index] && 
	 *                    n.index in this.entries.E && entry.index > n.index &&
	 *                    no i: this.entries.E - n.index | entry.index > i > n.index }
	 */
	Entry<V> predecessor(Entry<V> entry) {
		if (entry==NIL) return NIL;
		if (entry.left != NIL) {
			return max(entry.left);
		} else {
			Entry<V> ancestor = entry.parent;
			while (ancestor != NIL && entry == ancestor.left) {
				entry = ancestor;
				ancestor = ancestor.parent;
			}
			return ancestor;
		}
	}	
	
	/**
	 * Implementation of the tree-successor algorithm from CLR.
	 * Returns the node containing the given entry's successor, if it exists.  
	 * Otherwise returns NIL.  The behavior of this method is not defined if the
	 * given node is not in this tree.
	 * @requires node in this.root.*(left + right)
	 * @return {n: Node | n.value = this.entries[n.index] && 
	 *                    n.index in this.entries.E && entry.index < n.index &&
	 *                    no i: this.entries.E - n.index | entry.index < i < n.index }
	 */
	Entry<V> successor(Entry<V> entry) {
		if (entry==NIL) return NIL;
		if (entry.right != NIL) {
			return min(entry.right);
		} else {
			Entry<V> ancestor = entry.parent;
			while (ancestor != NIL && entry == ancestor.right) {
				entry = ancestor;
				ancestor = ancestor.parent;
			}
			return ancestor;
		}
	}
	
	/**
	 * If entry is NIL, returns <code>null</code>,
	 * otherwise returns the entry itself.
	 * @return entry=NIL => null, entry
	 */
	Entry<V> unwrap(Entry<V> entry) {
		return (entry==NIL ? null : entry);
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.util.SparseSequence#get(int)
	 */
	public V get(int index) {
		final Entry<V> node = search(root, index);
		return node==NIL ? null : node.value;
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.util.SparseSequence#containsIndex(int)
	 */
	public boolean containsIndex(int index) {
		return search(root, index) != NIL;
	}
	
	/**
	 * Searches for an entry with a null value, starting at the given entry.
	 * @return null in start.*(left+right).value
	 */
	private boolean containsNullValue(Entry<V> start) {
		if (start==NIL) return false;
		else if (start.value==null) return true;
		else return containsNullValue(start.left) || containsNullValue(start.right);
	}
	
	/**
	 * Searches for an entry with the given value, starting at the 
	 * given entry.  Assumes that the value is not null.
	 * @requires value != null
	 * @return value in start.*(left+right).value
	 */
	private boolean containsValue(Entry<V> start, Object value) {
		if (start==NIL) return false;
		else if (value.equals(start.value)) return true;
		else return containsValue(start.left, value) || containsValue(start.right, value);
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.util.SparseSequence#contains(java.lang.Object)
	 */
	public boolean contains(Object value) {
		return value==null ? containsNullValue(root) : containsValue(root, value);
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.util.SparseSequence#iterator()
	 */
	public Iterator<IndexedEntry<V>> iterator() {
		return new AscendingIterator(Integer.MIN_VALUE, Integer.MAX_VALUE);
	}
	
	/**
	 * Returns an iterator over the entries in this sequence,
	 * whose indeces are between from and to.  If from < to, 
	 * the entries are returned in the ascending order of 
	 * indeces.  Otherwise, they are returned in the descending
	 * order of indeces.
	 * @return an iterator over the entries in this sequence
	 * whose indeces are between from and to.
	 */
	public Iterator<IndexedEntry<V>> iterator(int from, int to) {
		return from > to ? new DescendingIterator(from,to) : new AscendingIterator(from,to);
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.util.SparseSequence#first()
	 */
	public Entry<V> first() {
		return unwrap(min(root));
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.util.SparseSequence#last()
	 */
	public Entry<V> last() {
		return unwrap(max(root));
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.util.SparseSequence#successor(int)
	 */
	public Entry<V> successor(int index) {
		final Entry<V> c = ceil(index);
		return c==null || c.index > index ? c : 
			   unwrap(successor(c));
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.util.SparseSequence#predecessor(int)
	 */
	public Entry<V> predecessor(int index) {
		final Entry<V> f = floor(index);
		return f==null || f.index < index ? f : 
			   unwrap(predecessor(f));
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.util.SparseSequence#ceil(int)
	 */
	public Entry<V> ceil(int index) {
		Entry<V> ceilEntry = root;
		if (ceilEntry==NIL) return null;
		while (true) {
			if (index == ceilEntry.index) {
				return ceilEntry;
			} else if (index < ceilEntry.index) {
				if (ceilEntry.left != NIL)
					ceilEntry = ceilEntry.left;
				else
					return ceilEntry;
			} else {
				if (ceilEntry.right != NIL) 
					ceilEntry = ceilEntry.right;
				else 
					return unwrap(successor(ceilEntry));
			}
		}
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.util.SparseSequence#floor(int)
	 */
	public Entry<V> floor(int index) {
		Entry<V> floorEntry = root;
		if (floorEntry==NIL) return null;
		while (true) {
			if (index == floorEntry.index) {
				return floorEntry;
			} else if (index < floorEntry.index) {
				if (floorEntry.left != NIL)
					floorEntry = floorEntry.left;
				else
					return unwrap(predecessor(floorEntry));
			} else {
				if (floorEntry.right != NIL) 
					floorEntry = floorEntry.right;
				else 
					return floorEntry;
			}
		}
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.util.SparseSequence#put
	 */
	public V put(int index, V value) {
		// insertion algorithm from CLR modified to disallow duplicate keys
		Entry<V> y = NIL;
		for (Entry<V> x = root; x != NIL;) {
			y = x;
			if (index == x.index) 
				return x.setValue(value); 
			else if (index < x.index) 
				x = x.left; 
			else 
				x = x.right; 
		}
		
		size++;
		if (y==NIL) {	
			root = new Entry<V>(index, value, BLACK, y, NIL);
		} else {
			Entry<V> z = new Entry<V>(index, value, RED, y, NIL);
			if (index < y.index)	{ y.left = z; }
			else 				{ y.right = z; }
			
			insertFixUp(z);
		}
		
		NIL.parent = NIL.left = NIL.right = NIL;
		return null;
	}
	
	/**
	 * {@inheritDoc}
	 */
	@SuppressWarnings("unchecked")
	public void putAll(SparseSequence<? extends V> s) {
		if (size==0 && s.size()!=0) {
			buildFromSorted((SparseSequence<V>) s);
		} else {
			super.putAll(s);
		}
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.util.SparseSequence#remove(int)
	 */
	public V remove(int index) {
		Entry<V> z = search(root, index);
		if (z==NIL) return null;
		
		size--;
		Entry<V> y = (z.left==NIL || z.right==NIL ? z : successor(z));
		Entry<V> x = (y.left != NIL ? y.left : y.right);
			
		x.parent = y.parent;
	
		if (y.parent == NIL)			{ root = x; }
		else if (y == y.parent.left)	{ y.parent.left = x; }
		else							{ y.parent.right = x; }
		
		final boolean ycolor = y.color;
		
		if (y != z) {
			// Instead of copying y's data into z, as is done in CLR,
			// we splice y into z's slot.  Otherwise, anyone holding an external pointer to z
			// will have the contents of z change on them!
			y.color = z.color;
			y.left = z.left;
			y.right = z.right;
			y.parent = z.parent;
			z.left.parent = z.right.parent = y; // we know that both z's children are non-NIL
			if (z.parent==NIL) root = y; // z is actually the root
			else if (z == z.parent.left) z.parent.left = y;
			else z.parent.right = y;
		}
		
		if (ycolor==BLACK) {
			deleteFixUp(x);
		}
		
		NIL.parent = NIL.left = NIL.right = NIL;
		z.left = z.right = z.parent = null; // cut z out of the tree by nulling out its pointers		
		return z.value;
	}
	
	/*---------balancing operations (CLR, pp.278-289)---------*/
	private void insertFixUp(Entry<V> z) {
		while (z != NIL && z != root && z.parent.color==RED) {
			if (z.parent == z.parent.parent.left) {
				Entry<V> y = z.parent.parent.right;
				if (y.color == RED) {
					z.parent.color = BLACK;
					y.color = BLACK;
					z.parent.parent.color = RED;
					z = z.parent.parent;
				} else {
					if (z == z.parent.right) {
						z = z.parent;
						rotateLeft(z);
					}
					z.parent.color = BLACK;
					z.parent.parent.color = RED;
					rotateRight(z.parent.parent);
				}
			} else { // symmetric with left and right exchanged
				Entry<V> y = z.parent.parent.left;
				if (y.color == RED) {
					z.parent.color = BLACK;
					y.color = BLACK;
					z.parent.parent.color = RED;
					z = z.parent.parent;
				} else {
					if (z == z.parent.left) {
						z = z.parent;
						rotateRight(z);
					}
					z.parent.color = BLACK;
					z.parent.parent.color = RED;
					rotateLeft(z.parent.parent);
				}
			}
		}
		root.color = BLACK;
	}
	
	private void deleteFixUp(Entry<V> x) {
		while (x != root && x.color==BLACK) {
			if (x == x.parent.left) {
				Entry<V> w = x.parent.right;
				if (w.color == RED) {
					w.color = BLACK;
					x.parent.color = RED;
					rotateLeft(x.parent);
					w = x.parent.right;
				}
				if (w.left.color==BLACK && w.right.color==BLACK) {
					w.color = RED;
					x = x.parent;
				} else {
					if (w.right.color == BLACK) {
						w.left.color = BLACK;
						w.color = RED;
						rotateRight(w);
						w = x.parent.right;
					}
					w.color = x.parent.color;
					x.parent.color = BLACK;
					w.right.color = BLACK;
					rotateLeft(x.parent);
					x = root;
				}
			} else {// symmetric with left and right exchanged
				Entry<V> w = x.parent.left;
				if (w.color == RED) {
					w.color = BLACK;
					x.parent.color = RED;
					rotateRight(x.parent);
					w = x.parent.left;
				}
				if (w.left.color==BLACK && w.right.color==BLACK) {
					w.color = RED;
					x = x.parent;
				} else {
					if (w.left.color == BLACK) {
						w.right.color = BLACK;
						w.color = RED;
						rotateLeft(w);
						w = x.parent.left;
					}
					w.color = x.parent.color;
					x.parent.color = BLACK;
					w.left.color = BLACK;
					rotateRight(x.parent);
					x = root;
				}
			}	
		}
		x.color = BLACK;
		
	}
	
	private void rotateLeft(Entry<V> x) {
		Entry<V> y = x.right;
		x.right = y.left;
        // must not change NIL's parent pointer since it may be used by deleteFixUp
		if (y.left != NIL) 
			y.left.parent = x;
		y.parent = x.parent;
		if (x.parent == NIL)
			root = y;
		else if (x.parent.left == x)
			x.parent.left = y;
		else
			x.parent.right = y;
		y.left = x;
		x.parent = y;
	}
	
	private void rotateRight(Entry<V> x) {
		Entry<V> y = x.left;
		x.left = y.right;
        // must not change NIL's parent pointer since it may be used by deleteFixUp
		if (y.right != NIL) 
			y.right.parent = x;
		y.parent = x.parent;
		if (x.parent == NIL)
			root = y;
		else if (x.parent.right == x)
			x.parent.right = y;
		else 
			x.parent.left = y;
		y.right = x;
		x.parent = y;
	}
	
	/*---------adapted from java.util.TreeMap---------*/
	/**
	 * Linear time tree building algorithm from sorted data.  
	 *
	 * @param seq sparse sequence from which to copy entries to this sequence
	 */
	private void buildFromSorted(SparseSequence<V> s) {
		this.size = s.size();
		root = buildFromSorted(0, 0, size-1, computeRedLevel(size), s.iterator());
	}
	
	/**
	 * Recursive "helper method" that does the real work of the
	 * of the previous method.  Identically named parameters have
	 * identical definitions.  Additional parameters are documented below.
	 * It is assumed that the comparator and size fields of the TreeMap are
	 * already set prior to calling this method.  (It ignores both fields.)
	 *
	 * @param level the current level of tree. Initial call should be 0.
	 * @param lo the first element index of this subtree. Initial should be 0.
	 * @param hi the last element index of this subtree.  Initial should be
	 *              size-1.
	 * @param redLevel the level at which nodes should be red.
	 *        Must be equal to computeRedLevel for tree of this size.
	 */
	private final Entry<V> buildFromSorted(int level, int lo, int hi,
			int redLevel, Iterator<IndexedEntry<V>> it) {
		/*
		 * Strategy: The root is the middlemost element. To get to it, we
		 * have to first recursively construct the entire left subtree,
		 * so as to grab all of its elements. We can then proceed with right
		 * subtree.
		 *
		 * The lo and hi arguments are the minimum and maximum
		 * indices to pull out of the iterator or stream for current subtree.
		 * They are not actually indexed, we just proceed sequentially,
		 * ensuring that items are extracted in corresponding order.
		 */
		
		if (hi < lo) return NIL;
		final int mid = (lo + hi) / 2;
		
		final Entry<V> left = buildFromSorted(level+1, lo, mid - 1, redLevel, it);
		
		final IndexedEntry<V> itEntry = it.next();
		final Entry<V> middle = new Entry<V>(itEntry.index(), itEntry.value(),
				(level==redLevel ? RED : BLACK),
				NIL, NIL);

		final Entry<V> right = buildFromSorted(level+1, mid+1, hi, redLevel, it);
		
		middle.left = left;
		middle.right = right;
		right.parent = middle;
		left.parent = middle;
		
		return middle;
	}
	
	/**
	 * Find the level down to which to assign all nodes BLACK.  This is the
	 * last `full' level of the complete binary tree produced by
	 * buildTree. The remaining nodes are colored RED. (This makes a `nice'
	 * set of color assignments wrt future insertions.) This level number is
	 * computed by finding the number of splits needed to reach the zeroeth
	 * node.  (The answer is ~lg(N), but in any case must be computed by same
	 * quick O(lg(N)) loop.)
	 */
	private static int computeRedLevel(final int sz) {
		int level = 0;
		for (int m = sz - 1; m >= 0; m = m / 2 - 1)
			level++;
		return level;
	}
	
	
	/**
	 * Represents a node in a red-black tree. 
	 * @specfield index: int
	 * @specfield value: V
	 * @specfield left, right, parent: Entry<V>
	 * @specfield color: boolean
	 */
	@SuppressWarnings("hiding")
	static final class Entry<V> extends AbstractIndexedEntry<V> {
		@SuppressWarnings("unused")
		private Entry<V> parent, left, right;
		@SuppressWarnings("unused")
		private boolean color;
		
		/**
		 * Constructs an entry with the given index, value, parent and color.  The entry's  
		 * children are set to nil.
		 * @effects this.index' = index && this.value' = value && this.left' = this.right' = nil && this.parent' = parent &&
		 *          this.color' = color.value()
		 */
		Entry(int index, V value, boolean color, Entry<V> parent, Entry<V> nil) {
			super(index, value);
			this.parent = parent;
			this.left = this.right = nil;
			this.color = color;
		}
		
		/**
		 * Constructs a NIL entry.
		 * @effects this.index' = 0 && this.value' = null &&
		 *          this.parent' = this.left' = this.right' = this &&
		 *          this.color' = BLACK
		 */
		Entry() {
			super(0, null);
			this.parent = this.left = this.right = this;
			this.color = BLACK;
		}
		
//		public String toString() {
//			if (this==left || this==right) return "NIL:" + (color==BLACK ? "black" : "red");
//			else return "[" + (color==BLACK ? "black" : "red") + ", " + left + 
//			             ", " + super.toString() + ", " + right + "]";
//		}
	}
	
	
	private abstract class EntryIterator implements Iterator<IndexedEntry<V>> {
		final int endIndex;
		Entry<V> lastReturned = NIL;
		Entry<V> next;
		
		/**
		 * Constructs a tree iterator which traverses the tree starting at
		 * the given Entry in either descending or ascending order, depending 
		 * on whether start.index is greater than endIndex. 
		 */
		EntryIterator(Entry<V> start, int endIndex) {
			this.next = start==null ? NIL : start;
			this.lastReturned = NIL;
			this.endIndex = endIndex;
		}
		
		/**
		 * Advances the next pointer to its successor,
		 * if this is an ascending iterator or to 
		 * its predecessor, if this is a descending iterator.
		 * @requires next != NIL
		 */
		abstract void advance();
		
		public abstract boolean hasNext();
		
		public IndexedEntry<V> next() {
			if (!hasNext())
				throw new NoSuchElementException();
			lastReturned = next;
			advance();
			return lastReturned;
		}
		
		public final void remove() {
			if (lastReturned == NIL)
				throw new IllegalStateException();
			final int nextIndex = next.index;
			TreeSequence.this.remove(lastReturned.index);
			// necessary since the structural modifications made by the delete
			// procedure may affect the next pointer
			next = search(root,nextIndex);
			lastReturned = NIL;
		}
	}
		
	private final class AscendingIterator extends EntryIterator {
		/**
		 * Constructs an ascending iterator over the entries with
		 * indeces between from and to.
		 * @requires from <= to
		 */
		AscendingIterator(int from, int to) {
			super(ceil(from),to);
		}
		/**
		 * Sets next to its successor.
		 */
		final void advance() {
			next = successor(next);
		}
		
		/**
		 * Returns true if next != NIL and its index is less 
		 * than or equal to the ending index.
		 */
		public boolean hasNext() { 
			return next != NIL && next.index<=endIndex; 
		}
	}
	
	private final class DescendingIterator extends EntryIterator {
		/**
		 * Constructs a descending iterator over the entries with
		 * indeces between from and to.
		 * @requires from >= to
		 */
		DescendingIterator(int from, int to) {
			super(floor(from),to);
		}
		/**
		 * Sets next to its predecessor.
		 */
		final void advance() {
			next = predecessor(next);
		}
		
		/**
		 * Returns true if next != NIL and its index is greater 
		 * than or equal to the ending index.
		 */
		public boolean hasNext() { 
			return next != NIL && next.index>=endIndex; 
		}
	}
}
