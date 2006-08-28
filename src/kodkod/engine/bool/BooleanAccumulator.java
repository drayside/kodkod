package kodkod.engine.bool;

import java.util.Iterator;

import kodkod.util.ints.IndexedEntry;
import kodkod.util.ints.SparseSequence;
import kodkod.util.ints.TreeSequence;


/**
 * An accumulator that may make the construction of large gates
 * faster.  An accumulator cannot be combined with other boolean values
 * using BooleanFactory methods.  To use the circuit
 * represented by an accumulator, one must first convert it into a gate
 * by calling BooleanFactory.toImmutableValue(MutableMultiGate g).  
 * 
 * @specfield components: set BooleanValue
 * @specfield op: Operator.Nary
 * @author Emina Torlak
 */
public final class BooleanAccumulator extends BooleanValue implements Iterable<BooleanValue>{
	final Operator.Nary op;
	private final SparseSequence<BooleanValue> inputs;
	
	/**
	 * Constructs a new accumulator with the given
	 * operator.
	 * @requires op != null
	 * @effects this.op' = op && this.label' = label
	 */
	private BooleanAccumulator(Operator.Nary op) {
		this.op = op;
		inputs = new TreeSequence<BooleanValue>();
	}
	
	/**
	 * Returns a tree based implementation of BooleanAccumulator.
	 * The addInput operation executes in O(lg n) time where n is the number of gate inputs. 
	 * @return a tree based BooleanAccumulator with the given operator and
	 * 0 as its label.
	 * @throws NullPointerException - op = null
	 */
	public static BooleanAccumulator treeGate(Operator.Nary op) {
		if (op==null) throw new NullPointerException();
		return new BooleanAccumulator(op);
	}
	
	/**
	 * Returns the operator for this accumulator.
	 * @return this.op
	 */
	public Operator op() {
		return op;
	}
	
	/**
	 * Adds the given value to this.components and returns the result.  Specifically,
	 * if the addition of the value causes the gate to evaluate to op.shortCircuit,
	 * then this.inputs is set to op.shortCircuit.  If the given value has already 
	 * been added or it is equal to this.op.identity, nothing changes.  Otherwise, v 
	 * is added to this.input.  The method returns this.op.shortCircuit if this.inputs
	 * contains it after the addition, otherwise it returns the gate itself.  
	 * @effects v = this.op.shortCircuit || v.negation in this.components => this.components' = this.op.shortCircuit,
	 *          v !in BooleanConstant => this.components' = this.components + v, 
	 *          this.components' = this.components
	 * @return this.components' = op.shortCircuit => op.shortCircuit, this
	 */
	public BooleanValue add(BooleanValue v) {
		if (isShortCircuited()) return op.shortCircuit(); 
		else{ 
			final int lit = v.label();
			if (v==op.shortCircuit() || inputs.containsIndex(-lit)) {
				inputs.clear();
				inputs.put(op.shortCircuit().label, op.shortCircuit());
				return op.shortCircuit();
			} 
			if (v!=op.identity() && !inputs.containsIndex(lit)) { inputs.put(lit, (BooleanValue) v);	}
			return this;
		}
	}
	
	/**
	 * Returns true if this gate is short circuited; that is,
	 * its inputs are reduced to this.op.shortCircuit.
	 * @return this.inputs = this.op.shortCircuit
	 */
	public boolean isShortCircuited() {
		return inputs.size()==1 && inputs.first().value()==op.shortCircuit();
	}

	/**
	 * Throws an IllegalArgumentException if op != this.op,
	 * otherwise returns the sum of digests of this gate's
	 * inputs with respect to the given operator.
	 * @return op = this.op => sum(this.inputs.digest(op))
	 * @throws IllegalArgumentException - op != this.op
	 * @throws ClassCastException - some this.inputs & BooleanConstant
	 */
	int digest(Operator op) {
		if (this.op != op) throw new IllegalArgumentException();
		int d = 0;
		for(Iterator<BooleanValue> inputs = iterator(); inputs.hasNext();) {
			d += ((BooleanFormula)inputs.next()).hash(op);
		}
		return d;
	}

	
		/**
		 * Returns the size of this accumulator.
		 * @return #this.inputs
		 */
		public int size() {
			return inputs.size();
		}
				
		/**
		 * Returns an iterator over this.components, in
		 * the increasing order of labels.  The returned iterator
		 * does not support removal.
		 * @return an iterator over this.components, in the
		 * increasing order of labels.
		 */
		public Iterator<BooleanValue> iterator() {
			return new Iterator<BooleanValue>() {
				final Iterator<IndexedEntry<BooleanValue>> iter = inputs.iterator();
				public boolean hasNext() {
					return iter.hasNext();
				}

				public BooleanValue next() {
					return iter.next().value();
				}

				public void remove() {
					throw new UnsupportedOperationException();
				}
				
			};
		}

		
		/**
		 * Throws an unsupported operation exception.
		 * @throws UnsupportedOperationException
		 */
		@Override
		BooleanValue negation() {
			throw new UnsupportedOperationException();
		}

		/**
		 * Returns 0.
		 * @return 0.
		 */
		@Override
		public int label() {
			return 0;
		}

		public String toString() {
			return inputs.toString();
		}
}
