package kodkod.engine.bool;

import java.util.Iterator;
import java.util.NoSuchElementException;

import kodkod.util.Iterators;
import kodkod.util.Ints;


/**
 * An immutable implementation of the multi gate interface.
 *  
 * @invariant inputs in BooleanFormula
 * @author Emina Torlak
 */
abstract class ImmutableMultiGate extends MultiGate {

	private final int literalDigest, inputDigest;
	
	/**
	 * Constructs a new MultiGate gate with the given
	 * operator and  literal.
	 * The inputDigest is the sum of this circuit's irreducible
	 * inputs digests with respect to the given operator.
	 * @requires op != null && literal > 0
	 * @effects this.op' = op && this.literal' = literal
	 */
	private ImmutableMultiGate(Operator op, int literal, int inputDigest) {
		super(op, literal);
		this.literalDigest = Ints.superFastHash(literal);
		this.inputDigest = inputDigest;
	}
	
	/**
	 * Returns a new immutable gate with the given operator, operator, inputs, and literal.
	 * @requires components.i0 = components.i1 && i0 != i1 && i0.negation() != i1 &&
	 *           literal > i0.literal && literal > i1.literal
	 * @return { gate: ImmutableMultiGate | gate.op = op && gate.inputs = i0 + i1 && gate.literal = literal }
	 */
	static ImmutableMultiGate make(Operator op, int literal, BooleanFormula i0, BooleanFormula i1) {
		return new Binary(op, literal, i0, i1);
	}
	
	/**
	 * Returns a new immutable gate with the given literal and same inputs and operator as 
	 * the given mutable gate.
	 * @requires #g.inputs > 1 && one (components).(g.inputs) && literal > max(abs(g.inputs.literal))
     * @return { gate: ImmutableMultiGate | gate.op = g.op && gate.inputs = g.inputs && gate.literal = literal } 
	 */
	static ImmutableMultiGate make(MutableMultiGate g, int literal) {
		assert g.numInputs() > 1;
		return g.numInputs()==2 ? new Binary(g, literal) : new NAry(g, literal);	
	}
	
	/**
	 * Returns the digest of this formula that would be used
	 * to compute the digest of the composition of this and
	 * some other formula using the given operator.  Specifically,
	 * if op = this.op, then the sum of this circuit's irreducible
	 * inputs' digests (with respect to op) is returned.  Otherwise, 
	 * the superFastHash of this.literal is returned.
	 * @return this.op = op => sum((irreducible inputs of this).digest(op)), IntHash.superFastHash(this.literal)
	 */
	@Override
	final int digest(MultiGate.Operator op) {
		return op==this.op ? inputDigest : literalDigest;
	}
	
	
	
	/**
	 * Represents a multi gate with two inputs.
	 * @invariant #this.inputs = 2
	 * @invariant digest = sum(inputs.digest(this.op))
	 */
	private static final class Binary extends ImmutableMultiGate {
		private final BooleanFormula low, high;
		private final int numAtomicInputs;

		/**
		 * Constructs a new binary gate with the given operator, operator, inputs, and literal.
		 * @requires components.i0 = components.i1 && i0 != i1 && i0.negation() != i1
		 * @effects this.op' = op && this.inputs' = i0 + i1 && this.literal' = literal
		 */
		Binary(Operator op, int literal, BooleanFormula i0, BooleanFormula i1) {
			super(op, literal, i0.digest(op) + i1.digest(op));
			this.low = (i0.literal() < i1.literal() ? i0 : i1);
			this.high = (i0.literal() < i1.literal() ? i1 : i0);
			this.numAtomicInputs = i0.numAtomicParts(op) + i1.numAtomicParts(op);
		}
		
		/**
		 * Constructs a new binary gate with the given literal, 
		 * from the given mutable multi gate.
		 * @requires g != null && #g.inputs = 2
		 * @effects this.op' = g.op && this.inputs' = g.inputs && this.literal' = literal
		 */
		Binary(MutableMultiGate g, int literal) {
			super(g.op, literal, g.digest(g.op));
			this.numAtomicInputs = g.numAtomicParts(op);
			final Iterator<BooleanValue> i = g.inputs();
			this.low = (BooleanFormula) i.next();
			this.high = (BooleanFormula) i.next();
		}

		@Override
		int numAtomicParts(Operator op) {
			return this.op==op ? numAtomicInputs : 1;
		}
		
		@Override
		boolean contains(Operator op, BooleanFormula f, int depth) {
			if (f==this) return true;
			else if (depth <= 0 || this.op != op) return false;
			else {
				final int vabs = f.literal() > 0 ? f.literal() : -f.literal();
				return f==low || f==high || 
				       (vabs < low.literal() && low.contains(op, f, depth-1)) || 
				       (vabs < high.literal() && high.contains(op, f, depth-1));
			}
		}

		@Override
		boolean isPartOf(Operator op, BooleanFormula f, int thisDepth, int fDepth) {
			if (f.contains(op, this, fDepth)) return true;
			else if (thisDepth <= 0 || this.op != op) return false; 
			else {
				return low.isPartOf(op, f, thisDepth-1, fDepth) &&
				       high.isPartOf(op, f, thisDepth-1, fDepth);
			}
		}
		
		@Override
		public int numInputs() { return 2; }

		@Override
		public Iterator<BooleanValue> inputs() {
			return new Iterator<BooleanValue>() {
				int next = 0;		
				public boolean hasNext() { 	return next < 2; }
				public BooleanFormula next() {
					if (!hasNext()) throw new NoSuchElementException();
					return (next++ == 0 ? low : high);
				}
				public void remove() {
					throw new UnsupportedOperationException();
				}
			};
		}
	}

	/**
	 * Represents a multi gate with more than two inputs.
	 * @invariant #this.inputs > 2
	 * @invariant digest = sum(inputs.digest(this.op))
	 */
	private static final class NAry extends ImmutableMultiGate {
		private final int numAtomicInputs;
		private final BooleanFormula[] inputs;
		
		/**
		 * Constructs a new n-ary gate with the given literal, from the given mutable multi gate.
		 * @requires g != null && #g.inputs > 2
		 * @effects this.op' = g.op && this.inputs' = g.inputs && this.literal' = literal
		 */
		NAry(MutableMultiGate g, int literal) {
			super(g.op, literal, g.digest(g.op));
			this.numAtomicInputs = g.numAtomicParts(op);
			this.inputs = new BooleanFormula[g.numInputs()];
			int index = 0;
			for(Iterator<BooleanValue> i = g.inputs(); i.hasNext(); ) {
				inputs[index] = (BooleanFormula) i.next();
				index++;
			}
		}

		@Override
		int numAtomicParts(Operator op) {
			return numAtomicInputs;
		}

		@Override
		public int numInputs() {
			return inputs.length;
		}

		@Override
		public Iterator<BooleanValue> inputs() {
			return Iterators.iterate(inputs);
		}
	}
}