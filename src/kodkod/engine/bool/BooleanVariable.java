package kodkod.engine.bool;

import java.util.Iterator;

import kodkod.util.Ints;
import kodkod.util.Iterators;


/**
 * Represents a boolean variable.  
 * 
 * @invariant op = Operator.VAR
 * @invariant no inputs && label in [1, ..., Integer.MAX_VALUE)
 * @author Emina Torlak
 */
public final class BooleanVariable extends BooleanFormula {
	final int label;
	private final int hashcode;

	/**
	 * Constructs a new BooleanVariable with the given label.
	 * @requires label != 0
	 * @effects this.label' = label
	 */
	BooleanVariable(int label) {
		super(null);
		assert label != 0;
		this.label = label;
		this.hashcode = Ints.superFastHash(label);
	}
		
	/**
	 * Returns a hash of this variable's label.
	 * @return Ints.superFastHash(this.label)
	 */
	@Override
	int hash(Operator op) {
		return hashcode;
	}
	
	/**
	 * Returns the label for this value. 
	 * @return this.label
	 */
	@Override
	public int label() {	return label; }
	
	/**
	 * Returns a string representation of this variable.
	 * @return a string representation of this variable.
	 */
	public String toString() {
		return Integer.toString(label);
	}

	/**
	 * Passes this value and the given
	 * argument value to the visitor, and returns the resulting value.
	 * @return the value produced by the visitor when visiting this node
	 * with the given argument.
	 */
	@Override
	public <T, A> T accept(BooleanVisitor<T,A> visitor, A arg) {
		return visitor.visit(this, arg);
	}

	/**
	 * Returns the VAR operator.
	 * @return Operator.VAR
	 */
	@Override
	public Operator op() {
		return Operator.VAR;
	}

	/**
	 * Returns an empty iterator.
	 * @return an empty iterator
	 */
	@Override
	public Iterator<BooleanFormula> iterator() {
		return Iterators.emptyIterator();
	}

	/**
	 * Returns 0.
	 * @return 0
	 */
	@Override
	public int size() {
		return 0;
	}

	/**
	 * Throws an IndexOutOfBoundsException.
	 * @throws IndexOutOfBoundsException
	 */
	@Override
	public BooleanFormula input(int i) {
		throw new IndexOutOfBoundsException();
	}

	/**
	 * Returns a hashcode for this variable.
	 * @return a hashcode for this variable.
	 */
	public int hashCode() { 
		return hashcode;
	}
}
