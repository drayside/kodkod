package kodkod.util.ints;

/**
 * A tree with integer keys.  
 * 
 * @specfield root: lone N
 * @specfield nodes: root.*(left + right)
 * @author Emina Torlak
 */
final class IntTree<N extends IntTree.Node<N>> implements Cloneable {
	private static final boolean BLACK = true, RED = false;
	
	/**
	 * NIL is a singleton.
	 */
	private static final Node NIL = new Node() {
		protected Node clone() { return this; }
	};
	
	private Node root;
	
	/**
	 * Creates an empty IntTree.
	 * @effects no this.root'
	 */
	IntTree() {
		root = NIL;
	}
	
	/**
	 * Discards all elements from this tree.
	 * @effects  no this.root' 
	 **/
	final void clear() {
		root = NIL;
	}
	
	/**
	 * Returns the node with the given key, or null no such node exists.
	 * @return this.nodes & key.index 
	 */
	final N search(int k) {	
		Node node = root;
		while(node != NIL) {
			if (node.key==k) break;
			else if (node.key>k) node = node.left;
			else node = node.right;
		}
		return unwrap(node);
	}
	
	/**
	 * Returns the node whose key is the ceiling of <tt>k</tt> in this tree, or 
	 * null if no such node exists.
	 * @return {n: this.nodes | n.key >= k &&
	 *           no n': this.nodes - n | n'.key >= k && n'.key < n.key }
	 */
	@SuppressWarnings("unchecked")
	final N searchGTE(int k) {
		if (root==NIL) return null;
		Node c = root;
		while (true) {
			if (c.key==k) {
				return (N)c;
			} else if (c.key>k) {
				if (c.left != NIL)
					c = c.left;
				else
					return (N)c;
			} else {
				if (c.right != NIL) 
					c = c.right;
				else 
					return successor((N)c);
			}
		}
	}
	
	/**
	 * Returns the node whose key is the floor of <tt>k</tt> in this tree, or 
	 * null if no such node exists.
	 * @return {n: this.nodes | n.key <= k &&
	 *           no n': this.nodes - n | n'.key <= k && n'.key > n.key }
	 */
	@SuppressWarnings("unchecked")
	final N searchLTE(int k) {
		if (root==NIL) return null;
		Node f = root;
		while(true) {
			if (f.key==k)
				return (N) f;
			else if (f.key>k) {
				if (f.left != NIL)
					f = f.left;
				else 
					return predecessor((N)f);
			} else {
				if (f.right != NIL) 
					f = f.right;
				else 
					return (N)f;
			}
		}
	}
	
	/**
	 * Implementation of the tree-predecessor algorithm from CLR.
	 * Returns the given node's predecessor, if it exists.  
	 * Otherwise returns null.  
	 * @return the given node's predecessor
	 * @throws NullPointerException - node = null
	 */
	final N predecessor(N node) {
		if (node.left != NIL) {
			return unwrap(max(node.left));
		} else {
			Node n = node;
			Node ancestor = n.parent;
			while (ancestor != NIL && n == ancestor.left) {
				n = ancestor;
				ancestor = ancestor.parent;
			}
			return unwrap(ancestor);
		}
	}	
	
	/**
	 * Implementation of the tree-successor algorithm from CLR.
	 * Returns the given node's successor, if it exists.  
	 * Otherwise returns null.
	 * @return the given node's successor
	 * @throws NullPointerException - node = null
	 */
	final N successor(N node) {
		if (node.right != NIL) {
			return unwrap(min(node.right));
		} else {
			Node n = node;
			Node ancestor = n.parent;
			while (ancestor != NIL && n == ancestor.right) {
				n = ancestor;
				ancestor = ancestor.parent;
			}
			return unwrap(ancestor);
		}
	}
	
	/**
	 * Returns the node with the smallest key.
	 * @return key.(min(this.nodes.key))
	 */
	final N min() {
		return unwrap(min(root));
	}
	
	/**
	 * Returns the node with the largest key.
	 * @return key.(max(this.nodes.key))
	 */
	final N max() {
		return unwrap(max(root));
	}
	
	/**
	 * Returns the leftmost node in the subtree rooted at start.
	 * The behavior of this method is unspecified if the given node
	 * is not in this tree.
	 * @requires node in this.nodes
	 * @return {n: start.*left | no n.left }
	 */
	private final Node min(Node start) {
		if (start != NIL) {
			while(start.left != NIL) {
				start = start.left;
			}
		}
		return start;
	}
	
	/**
	 * Returns the rightmost in the subtree rooted at start.
	 * The behavior of this method is unspecified if the given node
	 * is not in this tree.
	 * @requires node in this.nodes
	 * @return {n: start.*left | no n.right }
	 */
	private final Node max(Node start) {
		if (start != NIL) {
			while(start.right != NIL) {
				start = start.right;
			}
		}
		return start;
	}
	
	/**
	 * Replaces the old node, o, with the given new node, n, in this tree.
	 * @requires no n.(left + right + parent)
	 * @requires o = o.parent.left => n.key < o.parent.key
	 * @requires o = o.parent.right => n.key > o.parent.key
	 * @requires some o.left => n.key > o.left.key
	 * @requires some o.right => n.key < o.right.key
	 * @effects this.nodes' = this.nodes - o + n
	 * @effects o.parent = o.left = o.right = null
	 */
	@SuppressWarnings("unchecked")
	final void replace(N o, N n) {
		n.color = o.color;
		n.parent = o.parent;
		n.left = o.left;
		n.right = o.right;
		if (o.left != NIL) 			{ o.left.parent = n; }
		if (o.right != NIL)			{ o.right.parent = n;	 }	 
		if (o.parent==NIL)			{ root = n; }
		else if (o == o.parent.left) 	{ o.parent.left = n; }
		else 						{ o.parent.right = n; }
		o.parent = o.left = o.right = null;
	}
	
	/**
	 * Returns null if n is NIL, otherwise casts
	 * n to type N and returns it.
	 * @requires n in N
	 * @return if (n=NIL) then null else n
	 * @throws ClassCastException - n !in N
	 */
	@SuppressWarnings("unchecked")
	private static <N> N unwrap(Node n) {
		return n==NIL ? null : (N) n;
	}
	
	/**
	 * Inserts all nodes in the given tree into this tree.
	 * @requires no this.nodes
	 * @effects this.nodes' = original.nodes
	 */
	final void insertAll(IntTree<N> original) {
		assert root == NIL;
		try {
			this.root = original.root.clone();
		} catch (CloneNotSupportedException e) {
			throw new InternalError(); // unreachable code
		}
	}
	
	/**
	 * Implementation of the CLR insertion algorithm.
	 * @requires no z.key & this.nodes.key
	 * @effects this.nodes' = this.nodes + z
	 */
	@SuppressWarnings("unchecked")
	final void insert(N z) {
		Node y = NIL;
		for (Node x = root; x != NIL;) {
			y = x;
			if (x.key>z.key) 
				x = x.left; 
			else 
				x = x.right; 
		}
		
		z.parent = y;
		z.left = z.right = NIL;
		if (y==NIL) {	
			root = z;
		} else {
			z.color = RED;
			if (y.key>z.key)	{ y.left = z; }
			else 			{ y.right = z; }
			
			insertFixUp(z);
		}
		
		assert NIL.color && NIL.parent == NIL && NIL.left == NIL && NIL.right == NIL;
	}
	
	/**
	 * A slightly modified implementation of the CLR deletion algorithm.
	 * @requires z in this.nodes
	 * @effects this.nodes' = this.nodes - z
	 */
	@SuppressWarnings("unchecked")
	final void delete(N z) {
				
		Node y = (z.left==NIL || z.right==NIL ? z : successor(z));
		Node x = (y.left != NIL ? y.left : y.right);
		
		Node yparent = y.parent;
		final boolean yleft = (y==y.parent.left);
		final boolean ycolor = y.color;
				
		if (x!=NIL)						{ x.parent = yparent; }
		
		if (yparent == NIL)				{ root = x; }
		else if (yleft)					{ yparent.left = x; }
		else								{ yparent.right = x; }
		
		if (y != z) {
			// Instead of copying y's data into z, as is done in CLR,
			// we splice y into z's slot
			y.color = z.color;
			y.left = z.left;
			y.right = z.right;
			y.parent = z.parent;
			z.left.parent = z.right.parent = y; 
			if (z.parent==NIL) 			{ root = y; }
			else if (z == z.parent.left) 	{ z.parent.left = y; }
			else 						{ z.parent.right = y; }
		}
		
		if (ycolor==BLACK) {
			if (x!=NIL)					{ deleteFixUp(x); }
			else { // use z as a phantom child of y, since we cannot change pointers of NIL
				if (z==yparent) yparent = y; // y, z's successor, is z's right child
				z.color = BLACK;
				z.left = z.right = NIL;
				z.parent = yparent;
				if (yleft)				{ yparent.left = z; }
				else 					{ yparent.right = z; }
				deleteFixUp(z);
				if (z==z.parent.left)		{ z.parent.left = NIL; }
				else 					{ z.parent.right = NIL; }
			}
		}
		
		z.left = z.right = z.parent = null; // cut z out of the tree by nulling out its pointers			
		assert NIL.color && NIL.parent == NIL && NIL.left == NIL && NIL.right == NIL;
	}
	
	/**
	 * {@inheritDoc}
	 * @see java.lang.Object#clone()
	 * @throws CloneNotSupportedException - nodes contained in this tree are not cloneable
	 */
	@SuppressWarnings("unchecked")
	protected IntTree clone() throws CloneNotSupportedException {
		final IntTree<N> ret = (IntTree<N>) super.clone();
		ret.root = clone(root, NIL);
		return ret;
	}
	
	/**
	 * Recursively clones the given node.
	 */
	@SuppressWarnings("unchecked")
	private Node clone(Node n, Node parent) throws CloneNotSupportedException {
		if (n==NIL) return NIL;
		Node clone = (Node) n.clone();
		clone.parent = parent;
		clone.left = clone(n.left, clone);
		clone.right = clone(n.right, clone);
		return clone;
	}
	
	/*---------balancing operations (CLR, pp.278-289)---------*/
	/**
	 * From CLR.
	 */
	@SuppressWarnings("unchecked")
	private void insertFixUp(Node z) {
		while (z != NIL && z != root && z.parent.color==RED) {
			if (z.parent == z.parent.parent.left) {
				Node y = z.parent.parent.right;
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
				Node y = z.parent.parent.left;
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
	
	/**
	 * From CLR.
	 */
	@SuppressWarnings("unchecked")
	private void deleteFixUp(Node x) {
		while (x != root && x.color==BLACK) {
			if (x == x.parent.left) {
				Node w = x.parent.right;
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
				Node w = x.parent.left;
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
	
	/**
	 * From CLR.
	 */
	@SuppressWarnings("unchecked")
	private void rotateLeft(Node x) {
		Node y = x.right;
		x.right = y.left;
		if (y.left != NIL) // must not change NIL's pointers
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
	
	/**
	 * From CLR.
	 */
	@SuppressWarnings("unchecked")
	private void rotateRight(Node x) {
		Node y = x.left;
		x.left = y.right;  
		if (y.right != NIL) // must not change NIL's pointers
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
	
	public String toString() {
		return root.toString();
	}
	
	/**
	 * A node an in an int tree.  Subclasses need to 
	 * implement the clone method iff IntTree.clone will
	 * be called on the tree containing the nodes.
	 * @specfield key: int
	 * @specfield parent: lone N
	 * @specfield left: lone N
	 * @specfield right: lone N
	 * @author Emina Torlak
	 */
	abstract static class Node<N extends Node<N>> implements Cloneable {
		private Node parent, left, right;
		private boolean color;
		/**
		 * Subclasses are required to maintain the following invariant:
		 * @invariant 	this = this.parent.left => this.key < this.parent.key &&
		 *  				this = this.parent.right => this.key > this.parent.key &&
		 *  				some this.left => this.key > this.left.key &&
		 *  				some this.right => this.key < this.right.key
		 */
		protected int key;
		
		/**
		 * NIL node constructor
		 * @effects this.color' = BLACK && this.left' = this.right' = this.parent' = this
		 */
		private Node() {
			this.parent = this.left = this.right = this;
			this.color = BLACK;
			this.key = Integer.MIN_VALUE;
		}
		
		/**
		 * Constructs an empty node with the given key.
		 * @effects no this.(parent' + left' + right') && this.key' = key 
		 */
		Node(int key) {
			this.parent = this.left = this.right = NIL;
			this.color = BLACK;
			this.key = key;
		}
	
		/**
		 * Returns the left child of this node.
		 * @return this.left
		 */
		final N left() { return unwrap(left); }
		
		/**
		 * Returns the right child of this node.
		 * @return this.right
		 */
		final N right() { return unwrap(right); }
		
		/**
		 * Return the parent of this node.
		 * @return this.parent
		 */
		final N parent() { return unwrap(parent); }
		
		/**
		 * Clones this node.  Subclasses must override
		 * this method (and call super.clone()) in order
		 * for IntTree.clone() to function properly.
		 * @throws CloneNotSupportedException 
		 * @see java.lang.Object#clone()
		 */
		@SuppressWarnings("unchecked")
		protected Node<N> clone() throws CloneNotSupportedException {
			Node<N> ret = (Node<N>) super.clone();
			ret.parent = ret.left = ret.right = NIL;
			return ret;
		}
		
		public String toString() {
			if (this==NIL) 
				return "NIL";
			else 
				return "[" + key + " " + (color ? "b" : "r") + " " + (left==this ? key : left) + " "+ 
				        (right==this ? key : right)  + "]";
				      
		}
	}

}
