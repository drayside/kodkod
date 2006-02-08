package kodkod.engine.bool;

import java.util.Iterator;
import java.util.NoSuchElementException;

import kodkod.engine.bool.MultiGate.Operator;
import kodkod.util.Ints;


/**
 * Represents a NOT gate.
 * 
 * @invariant #inputs = 1 && inputs in MultiGate + BooleanVariable
 * @invariant this.literal = -input.literal
 * @invariant literal in (-Integer.MAX_VALUE..-1]
 * @author Emina Torlak
 */
public final class NotGate extends BooleanFormula {

	private final BooleanFormula input;
	private final int digest;
	
	/**
	 * Constructs a new NotGate with the given formula as its input.
	 * @requires input != null && input !in NotGate
	 * @effects this.inputs' = 0->input && this.output'.literal = -input.literal
	 */
	NotGate(BooleanFormula input) {
		this.input = input;
		this.digest = Ints.superFastHash(-input.literal());
	}
	
	/**
	 * Returns a hash of this inverter's literal.
	 * @return IntHasher.superFastHash(this.literal)
	 */
	@Override
	int digest(Operator op) {
		return digest;
	}
	
	@Override
	BooleanFormula negation() {
		return input;
	}
	
	/**
	 * Returns the sole input to this NOT gate.
	 * @return this.inputs
	 */
	public BooleanFormula input() {
		return input; 
	}
	
	@Override
	public Iterator<BooleanValue> inputs() {
		return new Iterator<BooleanValue>() {
			boolean hasNext = true;
			public boolean hasNext() {
				return hasNext;
			}

			public BooleanFormula next() {
				if (!hasNext) throw new NoSuchElementException();
				hasNext = false;
				return input;
			}

			public void remove() {
				throw new UnsupportedOperationException();
			}
			
		};
	}
	
	@Override
	public final int literal() { return -input.literal(); }
	
	@Override
	public int numInputs() {
		return 1;
	}
	
	@Override
	public <T, A> T accept(BooleanVisitor<T,A> visitor, A arg) {
		return visitor.visit(this, arg);
	}
	
	public String toString() {
		return "!" + input.toString();
	}

}
