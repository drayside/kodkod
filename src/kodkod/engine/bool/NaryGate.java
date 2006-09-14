package kodkod.engine.bool;

import java.util.Iterator;
import java.util.Set;

import kodkod.util.collections.Iterators;

/**
 * Represents a multi gate with more than two inputs.
 * @invariant #this.inputs > 2
 * @invariant digest = sum(inputs.digest(this.op))
 */
final class NaryGate extends MultiGate {
	private final BooleanFormula[] inputs;
	
	/**
	 * Constructs a new n-ary gate with the given label, from the given mutable multi gate.
	 * @requires g != null && #g.inputs > 2
	 * @effects this.op' = g.op && this.inputs' = g.inputs && this.label' = label
	 */
	NaryGate(BooleanAccumulator g, int label, int hashcode) {
		super(g.op, label, hashcode);
		this.inputs = new BooleanFormula[g.size()];
		int index = 0;
		for(Iterator<BooleanValue> i = g.iterator(); i.hasNext(); ) {
			inputs[index] = (BooleanFormula) i.next();
			index++;
		}
	}
	
	/**
	 * Returns the number of inputs to this gate.
	 * @return #this.inputs
	 */
	@Override
	public int size() {
		return inputs.length;
	}
	
	/**
	 * Returns an iterator over the inputs to this gate, in the ascending
	 * label order.
	 * @return an iterator over this.inputs
	 */
	@Override
	public Iterator<BooleanFormula> iterator() {
		return Iterators.iterate(inputs);
	}
	
	/**
	 * Returns an integer k' such that 0 < |k'| < k and |k'| is the number of flattening
	 * steps that need to be taken to determine that f is (not) an input to this circuit.
	 * A positive k' indicates that f is found to be an input to this circuit in k' steps.
	 * A negative k' indicates that f is not an input to this circuit, when it is flattened
	 * using at most k steps.  
	 * @requires k > 0
	 * @return the number of flattening
	 * steps that need to be taken to determine that f is (not) an input to this circuit
	 */
	@Override
	int contains(Operator op, BooleanFormula f, int k) {
		assert k > 0;
		if (f==this) return 1;
		else if (this.op != op) return -1;
		else {
			int low = 0, high = inputs.length-1, step = 1;
			final int key = f.label();
			while (low <= high && step <= k) {
				int mid = (low + high) >>> 1;
				int midVal = inputs[mid].label();
				
				if (midVal < key)
					low = mid + 1;
				else if (midVal > key)
					high = mid - 1;
				else
					return step; // key found in the given number of steps
				step++;
			}
			return 1-step;  // key not found.
		}
	}
	
	/**
	 * Flattens this circuit with respect to the given operator into 
	 * the provided set.  
	 * Specifically, the method modifies the set so that it contains
	 * the elements f_0, ..., f_k' where k' <= k elements and 
	 * [[this]] = op(f_0, ..., f_k').
	 * The default implementation simply adds this to the set.
	 * @requires k > 0
	 * @effects 1 <= k' <= k && some f_0,..., f_k' : flat.elts' | 
	 * [[this]] = op([[f_0]], ..., [[f_k']])
	 */
	@Override
	void flatten(Operator op, Set<BooleanFormula> flat, int k) {
		assert k > 0;
		if (this.op == op && k >= inputs.length) {
			int diff = k - inputs.length;
			for(BooleanFormula f: inputs) {
				int oldsize = flat.size();
				f.flatten(op, flat, StrictMath.max(1, diff));
				diff -= (flat.size() - oldsize);
			}
		} else {
			flat.add(this);
		}
	}
	
	/**
	 * Returns true if the given iterator and this.iterator 
	 * return the same elements, in the same order.
	 * @return true if values and this.iterator return the same elements,
	 * in the same order.
	 */
	boolean sameInputs(Iterator<? extends BooleanValue> values) {
		for(BooleanFormula f : inputs) {
			if (!(values.hasNext() && f == values.next()))
				return false;
		}
		return !values.hasNext();
	}
	
	/**
	 * Returns the ith input to this gate.
	 * @return this.inputs[i]
	 * @requires 0 <= i < size
	 * @throws IndexOutOfBoundsException - i < 0 || i >= #this.inputs
	 */
	@Override
	public BooleanFormula input(int i) {
		if (i < 0 || i > inputs.length)
			throw new IndexOutOfBoundsException();
		return inputs[i];
	}
}