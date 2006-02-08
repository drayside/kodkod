package kodkod.engine.bool;

import java.util.Iterator;
import java.util.NoSuchElementException;

import kodkod.engine.bool.MultiGate.Operator;
import kodkod.util.Ints;


/**
 * Represents a boolean variable.  
 * 
 * @invariant no inputs && literal in [1, ..., Integer.MAX_VALUE)
 * @author Emina Torlak
 */
public final class BooleanVariable extends BooleanFormula {
	final int literal;
	private final int digest;
	
	/*
	 * cache of this variable's inverter
	 * @invariant some negation => negation.input = this
	 */
	private NotGate negation = null;

	/**
	 * Constructs a new BooleanVariable with the given literal.
	 * @requires literal != 0
	 * @effects this.literal' = literal
	 */
	BooleanVariable(int literal) {
		assert literal != 0;
		this.literal = literal;
		this.digest = Ints.superFastHash(literal);
	}

	/**
	 * Returns true if this variable was created by the given factory.
	 * Otherwise returns false.
	 * @return this in factory.components
	 */
	boolean ownedBy(BooleanFactory factory) {
		return factory.variable(literal) == this;
	}
	
	/**
	 * Returns a hash of this variable's literal.
	 * @return IntHasher.superFastHash(this.literal)
	 */
	@Override
	int digest(Operator op) {
		return digest;
	}
	
	@Override
	BooleanFormula negation() {
		if (negation==null) {
			negation = new NotGate(this);
		}
		return negation;
	}
	
	@Override
	public int literal() {	return literal; }
	
	public String toString() {
		return Integer.toString(literal);
	}

	/**
	 * Returns an empty iterator, since a variable has no inputs.
	 * @return an empty iterator
	 */
	@Override
	public Iterator<BooleanValue> inputs() {
		return new Iterator<BooleanValue>() {

			public boolean hasNext() {	return false;	}

			public BooleanValue next() {
				throw new NoSuchElementException();
			}

			public void remove() {
				throw new UnsupportedOperationException();	
			}
			
		};
	}

	/**
	 * Returns 0, since a variable has no inputs.
	 * @return 0
	 */
	@Override
	public int numInputs() {
		return 0;
	}

	@Override
	public <T, A> T accept(BooleanVisitor<T,A> visitor, A arg) {
		return visitor.visit(this, arg);
	}
	
	
}
