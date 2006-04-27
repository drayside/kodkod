package kodkod.engine.bool;

import static kodkod.engine.bool.BooleanConstant.FALSE;
import static kodkod.engine.bool.BooleanConstant.TRUE;
import static kodkod.engine.bool.Operator.*;
import java.util.Iterator;

import kodkod.util.IndexedEntry;
import kodkod.util.SparseSequence;
import kodkod.util.TreeSequence;


/** 
 * Represents an n-dimensional matrix of {@link kodkod.engine.bool.BooleanValue boolean values}.  
 * Boolean matrices are indexed using flat integer indeces.  For example,
 * let m be a the 2 x 3 matrix of boolean variables identifed by labels [0 4 1; 5 10 2].  
 * Then, m[0] = 0, m[3] = 5, m[5] = 2, etc.  
 * 
 * All values stored in the same matrix must be created by the same {@link kodkod.engine.bool.BooleanFactory circuit factory}.  
 * All methods that accept another BooleanMatrix as an input will throw an 
 * IllegalArgumentException if the values in the input matrix do not belong 
 * to the same factory as the values in the receiver matrix.
 * 
 * @specfield dimensions: Dimensions
 * @specfield zero: BooleanConstant
 * @specfield factory: BooleanFactory
 * @specfield elements: [0..dimensions.capacity) -> one BooleanValue
 *
 * @author Emina Torlak  
 */
public final class BooleanMatrix implements Iterable<IndexedEntry<BooleanValue>> {
	private final Dimensions dimensions;
	private final BooleanConstant zero; 
	private final BooleanFactory factory;
	private final SparseSequence<BooleanValue> cells;
	

	/**  
	 * Constructs a new matrix with the given dimensions, default value, and factory.
	 * 
	 * @requires dimensions != null && zero != null && factory != null
	 * @effects this.dimensions' = dimensions &&
	 *          this.zero' = zero && 
	 *          this.elements' = [0..dimensions.capacity)->zero 
	 */
	BooleanMatrix(final Dimensions dimensions, final BooleanConstant zero, final BooleanFactory factory) {
		this.dimensions = dimensions;
		this.zero = zero;
		this.factory = factory;
		this.cells = new TreeSequence<BooleanValue>();
	}
	
	/**
	 * Returns the dimensions of this matrix.
     * @return this.dimensions
     */
	public Dimensions dimensions() { return dimensions; }
	
	/**
	 * Returns the default (zero) value for this matrix.
	 * @return this.zero
	 */
	public BooleanConstant zero() { return zero; }
	
	/**
	 * Returns the factory used to construct all the non-constant
	 * entries in this matrix.
	 * @return this.factory
	 */
	public BooleanFactory factory() { return factory; }
	
	/**
     * Returns the number of entries in this matrix that are not set to this.zero
     * @return #(this.elements.BooleanValue - this.elements.(this.zero))
     */
	public int density() { return cells.size(); }
	
	/**
	 * Returns an IndexedEntry-based view of the non-zero entries in this matrix.  The returned
	 * iterator enumerates indexed entries that represent the non-zero entries in the matrix, in the ascending
	 * order of indeces.  For example, suppose that this.zero is the BooleanConstant false,
	 * and the elements of this are 0->FALSE, 1->(a & b), 2->FALSE, 3->(c | d).  Then,
	 * the Iterator will return two IndexedEntries, c1 then c2, such that c1.index=1 && c1.value = a & b and
	 * c2.index=3 && c.value = c | d.  Calling {@link Iterator#remove()} on the returned iterator has the same effect
	 * as setting the entry obtained through the last call to {@link Iterator#next()} to this.zero.
	 * @return an iterator over IndexedEntries representing the non-zero entries in this matrix.
	 */
	public Iterator<IndexedEntry<BooleanValue>> iterator() {
		return cells.iterator();
	}
	
	/**
	 * Returns an IndexedEntry-based view of the non-zero entries in this matrix, starting
	 * at the first dense entry whose index is greater than or equal to <code>from</code>
	 * and ending at the last dense entry whose index is less than or equal to <code>to</code>.  
	 * If from < to, the entries are returned in the ascending order of 
	 * indeces.  Otherwise, they are returned in the descending
	 * order of indeces.  Calling {@link Iterator#remove()} on the returned iterator has the same effect
	 * as setting the entry obtained through the last call to {@link Iterator#next()} to this.zero.
	 * @return an iterator over IndexedEntries representing the non-zero entries in this matrix.
	 * @throws IndexOutOfBoundsException - !dimensions.validate(from) || !dimensions.validate(to)
	 */
	public Iterator<IndexedEntry<BooleanValue>> iterator(int from, int to) {
		if (!dimensions.validate(from)) throw new IndexOutOfBoundsException(from + " is not a valid index.");
		if (!dimensions.validate(to)) throw new IndexOutOfBoundsException(to + " is not a valid index.");
		return cells.iterator(from, to);
	}
	
	/**
	 * Return this.zero is value is null; otherwise return value itself.
	 */
	private BooleanValue maskNull(BooleanValue value) {
		return value == null ? zero : value;
	}
	
	/**
	 * Returns the value at the given index, without checking that the index is in bounds.
	 * @return this.elements[index]
	 */
	private BooleanValue fastGet(final int index) {
		return maskNull(cells.get(index));
	}
	
	/**
     * Returns the element at the specified index.
     * 
     * @return this.elements[index]
     * @throws IndexOutOfBoundsException - index < 0 || index >= this.dimensions.capacity
     */
	public BooleanValue get(final int index) {
		if (!dimensions.validate(index)) throw new IndexOutOfBoundsException(index + " is not a valid index.");
		return maskNull(cells.get(index));
	}
	
	
	/**
     * Returns a new matrix each of whose entries is a negation of the 
     * corresponding entry in this matrix.
     * 
     * @return { m: BooleanMatrix | m.dimensions=this.dimensions && m.factory = this.factory && m.zero = !this.zero &&
     *                              all i: [0..m.dimensions.capacity) | m.elements[i] = !this.elements[i] }
     */
	public BooleanMatrix not() {
		final BooleanMatrix negation = new BooleanMatrix(dimensions, zero==FALSE ? TRUE : FALSE, factory);
		
		for (IndexedEntry<BooleanValue> thisCell : cells) {
			negation.cells.put(thisCell.index(), factory.not(thisCell.value()));
		}
		
		return negation;
	}
	
	/**
	 * Returns a new matrix such that an entry in the returned matrix represents a 
	 * combination of the corresponding entries in m0 and m1, using the 
	 * specified operator.  The method assumes that m0.zero = m1.zero = op.identity,
	 * that neither matrix is empty, and that the matrices have the same factory and dimensions.
	 */
	private static BooleanMatrix composeUnion(Operator.Nary op, BooleanMatrix m0, BooleanMatrix m1) {
		final BooleanMatrix ret = new BooleanMatrix(m0.dimensions, op.identity(), m0.factory);
		
		IndexedEntry<BooleanValue> e0 = m0.cells.first(), e1 = m1.cells.first();
	
		while (e0 != null || e1 != null) {
			int i0 = e0==null ? Integer.MAX_VALUE : e0.index();
			int i1 = e1==null ? Integer.MAX_VALUE : e1.index();
			if (i0==i1) {
				ret.fastSet(i0, m0.factory.fastCompose(op, e0.value(), e1.value()));
				e0 = m0.cells.successor(i0);
				e1 = m1.cells.successor(i1);
			} else if (i0 < i1) {
				ret.fastSet(i0, e0.value());
				e0 = m0.cells.successor(i0);
			} else { // i0 > i1
				ret.fastSet(i1, e1.value());
				e1 = m1.cells.successor(i1);
			}
		}
		
		return ret;
	}
	
	/**
	 * Returns a new matrix such that an entry in the returned matrix represents a 
	 * combination of the corresponding entries in m0 and m1, using the 
	 * specified operator.  The method assumes that m0.zero = m1.zero = op.shortCircuit,
	 * that neither matrix is empty, and that the matrices have the same factory and dimensions.
	 */
	private static BooleanMatrix composeIntersection(Operator.Nary op, BooleanMatrix m0, BooleanMatrix m1) {
		final BooleanMatrix ret = new BooleanMatrix(m0.dimensions, op.shortCircuit(), m0.factory);
		
		IndexedEntry<BooleanValue> e0 = m0.cells.first(), e1 = m1.cells.first();
		
		while (e0 != null && e1 != null) {
			int i0 = e0.index(), i1 = e1.index();
			if (i0==i1) {
				ret.fastSet(i0, m0.factory.fastCompose(op, e0.value(), e1.value()));
				e0 = m0.cells.successor(i0);
				e1 = m1.cells.successor(i1);
			} else if (i0 < i1) {
				e0 = m0.cells.ceil(i1);
			} else { // i0 > i1
				e1 = m1.cells.ceil(i0);
			}
		}
		
		return ret;
	}
	
	/**
	 * Returns a new matrix such that an entry in the returned matrix represents a 
	 * combination of the corresponding entries in m0 and m1, using the 
	 * specified operator.  The method assumes that m0.zero = op.identity && m1.zero = op.shortCircuit,
	 * that neither matrix is empty, and that the matrices have the same factory and dimensions.
	 */
	private static BooleanMatrix composeMixed(Operator.Nary op, BooleanMatrix m0, BooleanMatrix m1) {
		final BooleanMatrix ret = new BooleanMatrix(m0.dimensions, op.shortCircuit(), m0.factory);
		IndexedEntry<BooleanValue> e0 = m0.cells.first(), e1 = m1.cells.first();
		
		while (e1 != null) {
			int i0 = e0==null ? Integer.MAX_VALUE : e0.index();
			int i1 = e1.index();
			if (i0==i1) {
				ret.fastSet(i0, m0.factory.fastCompose(op, e0.value(), e1.value()));
				e0 = m0.cells.successor(i0);
				e1 = m1.cells.successor(i1);
			} else if (i0 < i1) {
				e0 = m0.cells.ceil(i1);
			} else { // i0 > i1
				ret.fastSet(i1, e1.value());
				e1 = m1.cells.successor(i1);
			}
		}
		return ret;
	}
	
	/**
	 * Returns a new matrix such that an entry in the returned matrix represents a 
	 * combination of the corresponding entries in this and other matrix, using the 
	 * specified operator. 
	 * 
	 * @return { m: BooleanMatrix | m.dimensions = this.dimensions && m.factory = this.factory &&
	 *                              m.zero = this.zero op other.zero &&
	 *                              all i: [0..m.dimensions.capacity) | 
	 *                               m.elements[i] = this.elements[i] op other.elements[i] }
	 * @throws NullPointerException - other = null || op = null
	 * @throws IllegalArgumentException - !other.dimensions.equals(this.dimensions) || this.factory != other.factory
	 */
	public BooleanMatrix compose(Operator.Nary op, BooleanMatrix other) {
		if (factory != other.factory) throw new IllegalArgumentException();
		if (!dimensions.equals(other.dimensions)) throw new IllegalArgumentException();
		
		final boolean id0 = (zero == op.identity()), id1 = (other.zero == op.identity());
		
		if (cells.isEmpty()) return id0 ? other.copy() : this.copy();
		else if (other.cells.isEmpty()) return id1 ? this.copy() : other.copy();
		else return id0 ? (id1 ? composeUnion(op, this, other) : composeMixed(op, this, other)) :
			              (id1 ? composeMixed(op, other, this) : composeIntersection(op, this, other));
	}
	
	/**
	 * Returns a new matrix such that an entry in the returned matrix represents a 
	 * conjunction of the corresponding entries in this and other matrix.  The effect 
	 * of this method is the same as calling this.compose(Operator.Binary.AND, other).
	 * 
	 * @return { m: BooleanMatrix | m.dimensions = this.dimensions && m.factory = this.factory &&
	 *                              m.zero = this.zero AND other.zero &&
	 *                              all i: [0..m.dimensions.capacity) | 
	 *                               m.elements[i] = this.elements[i] AND other.elements[i] }
	 * @throws NullPointerException - other = null
	 * @throws IllegalArgumentException - !other.dimensions.equals(this.dimensions) || this.factory != other.factory
	 */
	public BooleanMatrix and(BooleanMatrix  other) {
		return compose(AND, other);
	}
	
	/**
	 * Returns a new matrix such that an entry in the returned matrix represents a 
	 * combination of the corresponding entries in this and other matrix.  The effect 
	 * of this method is the same as calling this.compose(Operator.Binary.OR, other).
	 * 
	 * @return { m: BooleanMatrix | m.dimensions = this.dimensions && m.factory = this.factory &&
	 *                              m.zero = this.zero OR other.zero &&
	 *                              all i: [0..m.dimensions.capacity) | 
	 *                               m.elements[i] = this.elements[i] OR other.elements[i] }
	 * @throws NullPointerException - other = null 
	 * @throws IllegalArgumentException - !other.dimensions.equals(this.dimensions) || this.factory != other.factory
	 */
	public BooleanMatrix or(BooleanMatrix  other) {
		return compose(OR, other);
	}
	
	/**
     * Returns the cross product of this and other matrix, using conjunction instead of 
     * multiplication.
     * 
     * @return { m: BooleanMatrix | m = this x other }
     * @throws NullPointerException - other = null
     * @throws IllegalArgumentException - this.factory != other.factory
     */
	public BooleanMatrix cross(final BooleanMatrix other) {
		if (factory != other.factory) throw new IllegalArgumentException();
		final boolean t0 = zero==TRUE, t1 = other.zero==TRUE;
		final boolean p0 = cells.isEmpty(), p1 = other.cells.isEmpty();
		final BooleanMatrix ret =  new BooleanMatrix(dimensions.cross(other.dimensions), 
                                                     t0 && t1 ? TRUE : FALSE, factory);
		final int c0 = dimensions.capacity(), c1 = other.dimensions.capacity();
		
		if (p0 && p1 || p0 && !t0 || p1 && !t1) return ret;
		
		if (!t0 && !t1 && !p1) { 
			for(IndexedEntry<BooleanValue> e0 : cells) {
				for (IndexedEntry<BooleanValue> e1: other.cells) {
					ret.fastSet(e0.index() * c1 + e1.index(), factory.fastCompose(AND, e0.value(), e1.value()));
				}
			}
		} else if (!t0 && t1) {
			for(IndexedEntry<BooleanValue> e0 : cells) {
				for (int j = 0; j < c1; j++) {
					ret.fastSet(e0.index() * c1 + j, factory.fastCompose(AND, e0.value(), other.fastGet(j)));
				}
			}
		} else if (t0 && !t1) {
			for (IndexedEntry<BooleanValue> e1 : other.cells) {
				for (int i = 0; i < c0; i++) {
					ret.fastSet(i * c1 + e1.index(), factory.fastCompose(AND, fastGet(i), e1.value()));
				}
			}
		} else { // t0 && t1
			for(int i = 0; i < c0; i++) {
				for (int j = 0; j < c1; j++) {
					ret.fastSet(i * c1 + j, factory.fastCompose(AND, fastGet(i), other.fastGet(j)));
				}
			}
		}
		
		return ret;
	}
	/**
     * Sets the value at the specified index to the given value;
     * returns the value previously at the specified position.  
     * It performs no index or null checking.
     * 
     * @effects this.elements'[index] = formula
     */
	private final void fastSet(final int index, final BooleanValue formula) {
		if (formula==zero) cells.remove(index);
		else cells.put(index,formula);
	}
	
	/**
	 * Adds the given value to the mutable gate at index k and 
	 * sets the element at index k to the resulting value.
	 */
	private final void dotAccumulate(final int k, final BooleanValue value) {
		if (value!=FALSE) {
			if (value==TRUE) fastSet(k, TRUE);
			else { // value in BooleanFormula
				BooleanValue kVal = fastGet(k);
				if (kVal != TRUE) {
					if (kVal==FALSE) {
						kVal = BooleanAccumulator.treeGate(OR);
					} 
					fastSet(k, ((BooleanAccumulator) kVal).add(value));
				}
			}
		}
	}
	
	/**
     * Returns the dot product of this and other matrix, using conjunction instead of 
     * multiplication and disjunction instead of addition.
     * 
     * @return { m: BooleanMatrix | m = this*other }
     * @throws NullPointerException - other = null
     * @throws IllegalArgumentException - !this.dimensions.multiplicationCompatible(other.dimensions) ||
     *                                    this.factory != other.factory
     */
	public BooleanMatrix dot(final BooleanMatrix other) {  
		if (factory != other.factory) throw new IllegalArgumentException();
		final boolean t0 = zero==TRUE, t1 = other.zero==TRUE;
		final boolean p0 = cells.isEmpty(), p1 = other.cells.isEmpty();
		final BooleanMatrix ret =  new BooleanMatrix(dimensions.dot(other.dimensions), 
                                                     t0 && t1 ? TRUE : FALSE, factory);

		if (p0 && p1 || p0 && !t0 || p1 && !t1) return ret;
		
		final int b = other.dimensions.dimension(0); 
		final int a = dimensions.capacity() / b; 
		final int c = other.dimensions.capacity() / b; 
		
		if (!p1 || t1) {
			for(IndexedEntry<BooleanValue> e0 : cells) {
				int i = e0.index();
				BooleanValue iVal = e0.value();
				if (iVal!=FALSE) {
					for (int j = 0; j < c; j++) {
						BooleanValue jVal = other.fastGet((i % b)*c + j);
						if (jVal!=FALSE) {
							ret.dotAccumulate((i / b)*c + j, factory.fastCompose(AND, iVal, jVal));					
						}
					}
				}
			}
		}
		if (t0) {
			for (IndexedEntry<BooleanValue> e1 :  other.cells) {
				int j = e1.index();
				BooleanValue jVal = e1.value();
				if (jVal!=FALSE) {
					for(int i = 0; i < a; i++) {
						if (!cells.containsIndex(j/c + b*i)) {
							ret.dotAccumulate(j%c + c*i, jVal);
						}
					}
				}
			}
		}	
		// make mutable gates immutable
		final BooleanConstant notZero = (t0 && t1 ? FALSE : TRUE);
		for(IndexedEntry<BooleanValue> e : ret.cells) {
			if (e.value()!=notZero) {
				ret.cells.put(e.index(), factory.fastAdopt((BooleanAccumulator) e.value()));
			}
		}
		return ret;
	}
	
	/**
     * Returns the transitive closure of this matrix.
     * 
     * @return { m: BooleanMatrix | m = ^this }
     * @throws UnsupportedOperationException - #this.diensions != 2 || !this.dimensions.square()
     */
	public BooleanMatrix closure() {
		if (dimensions.numDimensions() != 2 || !dimensions.isSquare()) {
			throw new UnsupportedOperationException("#this.diensions != 2 || !this.dimensions.square()");
		}
		if (cells.isEmpty())
			return copy();
		
		BooleanMatrix ret = this;
	
		// compute the number of rows in the matrix
		int rowNum = 0;
		final int rowFactor = dimensions.dimension(1);
		for(IndexedEntry<BooleanValue> rowLead = cells.first(); 
		    rowLead != null; rowLead = cells.ceil(((rowLead.index()/rowFactor) + 1) * rowFactor)) {
			rowNum++; //ret = ret.compose(OR, ret.dot(ret));
		}	
		
		// compute closure using iterative squaring
		for(int i = 1; i < rowNum; i*=2) {
			ret = ret.compose(OR, ret.dot(ret));
		}
		
		return ret;
	}
	
	/**
     * Returns the transpose of this matrix.
     * 
     * @return { m: BooleanMatrix | m = ~this }
     * @throws UnsupportedOperationException - #this.dimensions != 2
     */
	public BooleanMatrix transpose() {
		final BooleanMatrix ret = new BooleanMatrix(dimensions.transpose(), zero, factory);
		final int rows = dimensions.dimension(0), cols = dimensions.dimension(1);
		for (IndexedEntry<BooleanValue> e0 : cells) {
			ret.cells.put((e0.index()%cols)*rows + (e0.index()/cols), e0.value());
		}
		return ret;
	}
		
	/**
	 * Returns a boolean matrix m such that m = m0 if the given condition evaluates
	 * to TRUE and m = m1 otherwise.  The method assumes that m0.zero = m1.zero, 
	 * and that they have the same factory and dimensions.
	 */
	private static BooleanMatrix choiceSameZero(final BooleanValue c, final BooleanMatrix m0, final BooleanMatrix m1) {
		final BooleanFactory f = m0.factory;
		final BooleanConstant zero = m0.zero;
		final BooleanMatrix ret =  new BooleanMatrix(m0.dimensions, zero, f);
//		final BooleanValue notc = c.negation();
//		final BooleanValue c0 = (m0.zero==TRUE ? notc : c), c1 = c0.negation();
//		final Operator.Nary op = (m0.zero==TRUE ? OR : AND);
		
		IndexedEntry<BooleanValue> e0 = m0.cells.first(), e1 = m1.cells.first();
		
		while (e0 != null || e1 != null) {
			int i0 = e0==null ? Integer.MAX_VALUE : e0.index();
			int i1 = e1==null ? Integer.MAX_VALUE : e1.index();
			if (i0==i1) { // (c => e0, e1) = (!c || e0) && (c || e1)
				//ret.fastSet(i0, f.fastCompose(AND, f.fastCompose(OR, notc, e0.value()), f.fastCompose(OR, c, e1.value())));
				ret.fastSet(i0, f.fastITE(c, e0.value(), e1.value()));
				e0 = m0.cells.successor(i0);
				e1 = m1.cells.successor(i1);
			} else if (i0 < i1) { // (c => e0, zero) = (!c || e0) && (c || zero) = c0 op e0
//				ret.fastSet(i0, f.fastCompose(op, c0, e0.value()));
				ret.fastSet(i0, f.fastITE(c, e0.value(), zero));
				e0 = m0.cells.successor(i0);
			} else { // (c => zero, e1) = (!c || zero) && (c || e1) = c1 op e1
//				ret.fastSet(i1, f.fastCompose(op, c1, e1.value()));
				ret.fastSet(i1, f.fastITE(c, zero, e1.value()));
				e1 = m1.cells.successor(i1);
			}
		}
		
		return ret;
	}
	
	/**
	 * Returns a boolean matrix m such that m = m0 if the given condition evaluates
	 * to TRUE and m = m1 otherwise.  The method assumes that m0.zero != m1.zero, 
	 * and that they have the same factory and dimensions.
	 */
	private static BooleanMatrix choiceDifferentZero(final BooleanValue c, final BooleanMatrix m0, final BooleanMatrix m1) {
		final BooleanFactory f = m0.factory;
		final BooleanMatrix ret =  new BooleanMatrix(m0.dimensions, FALSE, f);
//		final BooleanValue notc = c.negation();
		
		for(int i = 0, n = ret.dimensions.capacity(); i < n; i++) {
			//ret.fastSet(i, f.fastCompose(AND, f.fastCompose(OR, notc, m0.fastGet(i)), f.fastCompose(OR, c, m1.fastGet(i))));
			ret.fastSet(i, f.fastITE(c, m0.fastGet(i), m1.fastGet(i)));
		}
		
		return ret;
	}
	
	/**
	 * Returns a boolean matrix m such that m = this if the given condition evaluates
	 * to TRUE and m = other otherwise.
	 * 
	 * @return { m: BooleanMatrix | m.dimensions = this.dimensions &&
     *                              all i: [0..m.dimensions.capacity) | 
     *                                 m.elements[i] = condition => this.elements[i], other.elements[i] }
     * @throws NullPointerException - other = null || condition = null
     * @throws IllegalArgumentException - !other.dimensions.equals(this.dimensions) || this.factory != other.factory
	 */
	public BooleanMatrix choice(BooleanValue condition, BooleanMatrix other) {
		if (factory != other.factory) throw new IllegalArgumentException();
		if (!dimensions.equals(other.dimensions)) throw new IllegalArgumentException();
		if (condition==TRUE) return this.copy();
		else if (condition==FALSE) return other.copy();
		else return zero==other.zero ? 
			        choiceSameZero(condition, this, other) : 
			        choiceDifferentZero(condition, this, other);
	}
	
	/**
	 * Returns a conjunction of the negated values between 
	 * start, inclusive, and end, exclusive.
	 * @requires 0 <= start < end < this.capacity() 
	 * @return !this.elements[start] && !this.elements[start+1] && ... && !this.elements[end-1]
	 */
	private BooleanValue conjunctionOfNegations(int start, int end) {
		int count = 0;
		final BooleanAccumulator g = BooleanAccumulator.treeGate(AND);
		for(Iterator<IndexedEntry<BooleanValue>> iter = cells.iterator(start, end-1); iter.hasNext(); ) {
			count++;
			if (g.add(factory.not(iter.next().value()))==FALSE) 
				return FALSE;
		}
		if (count < end - start) {
			g.add(factory.not(zero));
		}
		return factory.fastAdopt(g);
	}
	
	/**
	 * Returns the result of overriding this with other.  The method assumes that
	 * other has the same factory and dimensions as this and that 
	 * this.zero = FALSE.
	 */
	private BooleanMatrix overrideF(BooleanMatrix other) {
		final BooleanMatrix ret = other.copy();
		final int rowFactor = dimensions.capacity() / dimensions.dimension(0);
		int row = -1;
		BooleanValue rowVal = BooleanConstant.TRUE;
		for(IndexedEntry<BooleanValue> e0 : cells) {
			int e0row = e0.index() / rowFactor;
			if (row != e0row) {
				row = e0row;
				rowVal = other.conjunctionOfNegations(row*rowFactor, (row+1)*rowFactor);
			}
			ret.fastSet(e0.index(), factory.or(ret.fastGet(e0.index()), factory.and(e0.value(), rowVal)));
		}
		return ret;
	}
	
	/**
	 * Returns the result of overriding this with other.  The method assumes that
	 * other has the same factory and dimensions as this and that this.zero = TRUE.
	 */
	private BooleanMatrix overrideT(BooleanMatrix other) {
		final BooleanMatrix ret = copy();
		final int rowFactor = dimensions.capacity() / dimensions.dimension(0);
		int row = -1;
		
		for(IndexedEntry<BooleanValue> e1 : other.cells) {
			int e1row = e1.index() / rowFactor;
			if (row != e1row) {
				row = e1row;
				int rowIndex = row*rowFactor, nextRowIndex = (row+1)*rowFactor;
				BooleanValue rowVal = other.conjunctionOfNegations(rowIndex, nextRowIndex);
				if (rowVal != TRUE) {
					for(int i = rowIndex; i < nextRowIndex; i++) {
						ret.fastSet(i, factory.and(ret.fastGet(i), rowVal));
					}
				}		
			}
			ret.fastSet(e1.index(), factory.or(e1.value(), ret.fastGet(e1.index())));
		}		
		
		// fix up the returned matrix when other.zero = TRUE 
		if (other.zero == TRUE) {
			int max = 0;
			for(IndexedEntry<BooleanValue> e1 : other.cells) {
				for(int i = max; i < e1.index(); i++) {
					ret.fastSet(i, TRUE);
				}
				max = e1.index() + 1;
			}
			for(int i = max; i < dimensions.capacity(); i++) {
				ret.fastSet(i, TRUE);
			}
		}
		
		return ret;
	}
	
	/**
	 * Overrides the values in this matrix with those in <code>other</code>.
	 * Specifically, for each index i of the returned matrix m,
	 * m.elements[i] is true iff other.elements[i] is true or
	 * this.elements[i] is true and all elements of <code>other</code> 
	 * that are in the same row as i are false.
	 * @return {m: BooleanMatrix | m.dimensions = this.dimensions &&
	 *                             m.zero = this.zero || other.zero &&
	 *                             all i: [0..m.capacity()) | m.elements[i] = 
	 *                               other.elements[i] || 
	 *                               this.elements[i] && !OR(other.elements[row(i)]) }
	 * where other.elements[row(i)] selects all elements of <code>other</code>
	 * that are in the same row as i.  
	 * @throws NullPointerException - other = null
	 * @throws IllegalArgumentException - other.dimensions != this.dimensions
	 */
	public BooleanMatrix override(BooleanMatrix other) {
		if (factory != other.factory) throw new IllegalArgumentException();
		if (!dimensions.equals(other.dimensions)) throw new IllegalArgumentException();
		if (other.cells.isEmpty()) {
			return other.zero==TRUE ? other.copy() : this.copy();
		}
		
		return zero.booleanValue() ? overrideT(other) : overrideF(other);
	}
	
	/**
	 * Returns a BooleanValue that represents a composition of all the entries
	 * in this matrix using the specified operator.
	 * 
	 * @return { f: BooleanValue | f <=> this.elements[0] op ... op this.elements[this.dimensions.capacity-1] }
	 * @throws NullPointerException - op = null
	 */
	public BooleanValue fold(Operator.Nary op) {
		final BooleanConstant shortCircuit = op.shortCircuit();
		if (cells.isEmpty()) return zero;
		if (zero==shortCircuit && density() < dimensions.capacity()) return shortCircuit;
		final BooleanAccumulator g = BooleanAccumulator.treeGate(op);
		for(Iterator<IndexedEntry<BooleanValue>> entries = cells.iterator(); entries.hasNext();) {
			if (g.add(entries.next().value())==shortCircuit) return shortCircuit;
		}
		return factory.fastAdopt(g);
	}
	
	/**
     * Returns a BooleanValue that represents the conjunction of all the entries
     * in this matrix.  The effect of this method is the same as calling this.fold(AND).
     * 
     * @return { f: BooleanValue | f <=> this.elements[0] && ... && this.elements[this.dimensions.capacity-1] }
     */
	public BooleanValue andFold() {
		return fold(AND);
	}
	
	/**
     * Returns a BooleanValue that represents the disjunction of all the entries
     * in this matrix.  The effect of this method is the same as calling this.fold(OR).
     * 
     * @return { f: BooleanValue | f <=> this.elements[0] || ... || this.elements[this.dimensions.capacity-1] }
     */
	public BooleanValue orFold() {
		return fold(OR);
	}
	
	/**
	 * Returns a BooleanValue that constrains at least one value in this.elements to be true.  The
	 * effect of this method is the same as calling this.orFold().
	 * @return { f: BooleanValue | f <=> this.elements[0] || ... || this.elements[this.dimensions.capacity-1] }
	 */
	public BooleanValue some() {
		return orFold();
	}
	
	/**
	 * Returns a BooleanValue that constrains at most one value in this.elements to be true.
	 * The effect of this method is the same as calling this.factory.or(this.one(), this.none()).
	 * @return { f: BooleanValue | f <=> this.one() || this.none() }
	 */
	public BooleanValue lone() {
		return factory.fastCompose(OR, one(), none());
	}
	
	/**
	 * Returns a BooleanValue that constraints exactly one value in this.elements to be true.
	 * @return { f: BooleanValue | f <=> !(!this.elements[0] || this.elements[1..this.dimensions.capacity) || 
	 *                                   !(this.elements[0] || !this.elements[1] || this.elements[2..this.dimensions.capacity)) || ...
	 *                                   !(this.elements[0..this.dimensions.capacity-1) || !this.elements[this.dimensions.capacity-1] }
	 */
	public BooleanValue one() {
		final int density = density();
		if (zero==FALSE && cells.isEmpty() || zero==TRUE && density <= dimensions.capacity()-2) return FALSE;
		final BooleanAccumulator g = BooleanAccumulator.treeGate(OR);
		UPDATEg : for (int i = 0; i < density; i++) { // g contains the outermost disjunction
			Iterator<IndexedEntry<BooleanValue>> entries = cells.iterator();
			BooleanAccumulator v = BooleanAccumulator.treeGate(OR); // v contains the individual disjunctions
			for(int j = 0; j < i; j++) {
				if (v.add(entries.next().value())==TRUE) {
					continue UPDATEg;
				}
			}
			if (v.add(entries.next().value().negation())==TRUE) continue;
			for(int j = i+1; j < density; j++) {
				if (v.add(entries.next().value())==TRUE) {
					continue UPDATEg;
				}
			}
			if (g.add(factory.fastAdopt(v).negation())==TRUE) return TRUE;
		}
		
		return factory.fastAdopt(g);
	}
	
	/**
	 * Returns a BooleanValue that constraints all values in this.elements to be false.
	 * The effect of this method is the same as calling this.factory.not(this.orFold()).
	 * @return { f: BooleanValue | f <=> !(this.elements[0] || ... || !this.elements[this.dimensions.capacity-1]) }
	 */
	public BooleanValue none() {
		return factory.not(orFold());
	}
	
	/**
     * Sets the value at the specified index to the given value;
     * returns the value previously at the specified position.  
     * 
     * @return this.elements[index]
     * @effects this.elements'[index] = formula
     * @throws NullPointerException - formula = null
     * @throws IndexOutOfBoundsException - index < 0 || index >= this.dimensions.capacity
     */
	public BooleanValue set(final int index, final BooleanValue formula) {
		if (!dimensions.validate(index)) throw new IndexOutOfBoundsException("index < 0 || index >= this.dimensions.capacity");
		if (formula==null) throw new NullPointerException("formula=null");
		if (formula==zero) return maskNull(cells.remove(index));
		else return maskNull(cells.put(index,formula));
	}
	
	/**
	 * Returns a copy of this boolean matrix.
	 * @return {m: BooleanMatrix - this | m.zero = this.zero && m.dimensions = this.dimensions &&
	 *                                    m.elements = copy of this.elements } 
	 */
	public BooleanMatrix copy() { 
		final BooleanMatrix m = new BooleanMatrix(dimensions,zero,factory);
		m.cells.putAll(cells);
		return m;
	}
	
	public String toString() {
		final StringBuffer buff = new StringBuffer("dimensions: ");
		buff.append(dimensions);
		buff.append(", zero: ");
		buff.append(zero.toString());
		buff.append(", elements: ");
		buff.append(cells);
		return buff.toString();
	}	
}
