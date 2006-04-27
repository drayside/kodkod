package kodkod.engine.bool;

import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Set;

import kodkod.util.collections.Iterators;
import kodkod.util.ints.Ints;


/**
 * Represents a gate with two or more inputs; an AND or an OR gate.
 *  
 * @specfield op: Operator.Binary
 * @invariant #inputs > 1
 * @invariant some components.this => label in [1..Integer.MAX_VALUE), label in [0..Integer.MAX_VALUE)
 * @invariant no c1, c2: inputs | c1.label = -c2.label
 * @invariant this.label > 0 => all c: inputs | |c.label| < this.label
 * @author Emina Torlak
 */
public abstract class MultiGate extends BooleanFormula {
	final Operator.Nary op;
	
	private final int label, labelhash, hashcode;
	
	/**
	 * Constructs a new MultiGate gate with the given operator and label.
	 * @requires op != null && label >= 0
	 * @effects this.op' = op && this.label' = label
	 */
	private MultiGate(Operator.Nary op, int label, int hashcode) {
		super(null);
		assert op != null;
		assert label >= 0;
		this.op = op;
		this.label = label;
		this.labelhash = Ints.superFastHash(label);
		this.hashcode = hashcode;
	}
	
	/**
	 * Returns the label for this value. 
	 * @return this.label
	 */
	@Override
	public final int label() { return label; }
	
	/**
	 * Returns the operator used to combine the input
	 * variables of this connective gate.
	 * @return this.op
	 */
	public final Operator.Nary op() { return op; }
	
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
	 * Returns a string representation of this multigate.
	 * @return a string representation of this multigate.
	 */
	public String toString() {
		final StringBuilder builder = new StringBuilder("(");
		final Iterator<BooleanFormula> children = iterator();
		builder.append(children.next());
		while(children.hasNext()) {
			builder.append(op);
			builder.append(children.next());
		}
		builder.append(")");
		return builder.toString();
	}
	
	/**
	 * Returns a hashcode for this gate.  The hashcode obeys the Object contract.
	 * @return a hashcode for this gate.
	 */
	@Override
	public final int hashCode() {
		return hashcode;
	}
	
	/**
	 * Returns a new immutable gate with the given operator, label, hashcode and inputs.
	 * @requires one components.(this + l + h)
	 * @requires l.label < h.label 
	 * @requires label > |l.label| && |label > h.label|
	 * @requires hashcode = op.hash(l, h)
	 * @return { gate: ImmutableMultiGate | gate.op = op && gate.inputs = l + h && gate.label = label }
	 */
	static MultiGate make(Operator.Nary op, int label, int hashcode, BooleanFormula l, BooleanFormula h) {
		return new Binary(op, label, hashcode, l, h);
	}
	
	/**
	 * Returns a new immutable gate with the given label, hashcode, and same inputs and operator as 
	 * the given mutable gate.
	 * @requires #g.inputs > 2 && one (components).(g.inputs) && label > max(|g.inputs.label|)
	 * @requires hashcode = op.hash(g.iterator())
	 * @return { gate: ImmutableMultiGate | gate.op = g.op && gate.inputs = g.inputs && gate.label = label } 
	 */
	static MultiGate make(BooleanAccumulator g, int label, int hashcode) {
		assert g.size() > 2;
		return new NAry(g, label, hashcode);	
	}
	
	/**
	 * Returns the digest of this formula that would be used
	 * to compute the digest of the composition of this and
	 * some other formula using the given operator.  Specifically,
	 * if op = this.op, then the sum of this circuit's irreducible
	 * inputs' hashes (with respect to op) is returned.  Otherwise, 
	 * the superFastHash of this.label is returned.
	 * @return this.op = op => this.op.hash(this.inputs), Ints.superFastHash(this.label)
	 */
	@Override
	final int hash(Operator op) {
		return op==this.op ? hashcode : labelhash; 
	}
	
	/**
	 * Represents a multi gate with two inputs.
	 * @invariant #this.inputs = 2
	 * @invariant digest = sum(inputs.digest(this.op))
	 */
	static final class Binary extends MultiGate {
		private final BooleanFormula low, high;
		
		/**
		 * Constructs a new binary gate with the given operator, label, and inputs.
		 * @requires components.h = components.l && l.label < h.label
		 * @effects this.op' = op && this.inputs' = l + h && this.label' = label
		 */
		private Binary(Operator.Nary op, int label, int hashcode, BooleanFormula l, BooleanFormula h) {
			super(op, label, hashcode);
			assert l.label() < h.label();
			this.low = l;
			this.high = h;
		}
		
		/**
		 * Returns an integer k' such that 0 < |k'| < k and |k'| is the number of flattening
		 * steps that need to be taken to determine that f is (not) an input to this circuit.
		 * A positive k' indicates that f is found to be an input to this circuit in k' steps.
		 * A negative k' indicatets that f is not an input to this circuit, when it is flattened
		 * using at most k steps.  
		 * @requires k > 0
		 * @return the number of flattening
		 * steps that need to be taken to determine that f is (not) an input to this circuit
		 */
		@Override
		int contains(Operator op, BooleanFormula f, int k) {
			assert k > 0;
			if (f==this) return 1;
			else if (this.op != op || k < 2) return -1;
			else {
				final int l = low.contains(op, f, k-1);
				if (l > 0) return l;
				else {
					final int h = high.contains(op, f, k + l);
					return h > 0 ? h - l : h + l;
				}
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
			if (this.op==op && k > 1) {
				final int oldsize = flat.size();
				low.flatten(op, flat, k-1);
				high.flatten(op, flat, k - (flat.size()-oldsize));
			} else {
				flat.add(this);
			}
		}
		
		/**
		 * Returns 2.
		 * @return 2
		 */
		@Override
		public int size() { return 2; }
		
		/**
		 * Returns an iterator over the inputs to this gate, in
		 * the increasing label order.
		 * @return an iterator over this.inputs.
		 */
		@Override
		public Iterator<BooleanFormula> iterator() {
			return new Iterator<BooleanFormula>() {
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
		
		/**
		 * Returns the ith input to this gate.
		 * @return this.inputs[i]
		 * @requires 0 <= i < size
		 * @throws IndexOutOfBoundsException - i < 0 || i >= #this.inputs
		 */
		@Override
		public BooleanFormula input(int i) {
			switch(i) {
			case 0 : return low;
			case 1 : return high;
			default:
				throw new IndexOutOfBoundsException();
			}
		}
	}
	
	/**
	 * Represents a multi gate with more than two inputs.
	 * @invariant #this.inputs > 2
	 * @invariant digest = sum(inputs.digest(this.op))
	 */
	static final class NAry extends MultiGate {
		private final BooleanFormula[] inputs;
		
		/**
		 * Constructs a new n-ary gate with the given label, from the given mutable multi gate.
		 * @requires g != null && #g.inputs > 2
		 * @effects this.op' = g.op && this.inputs' = g.inputs && this.label' = label
		 */
		private NAry(BooleanAccumulator g, int label, int hashcode) {
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
		 * A negative k' indicatets that f is not an input to this circuit, when it is flattened
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
					int mid = (low + high) >> 1;
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
		 * @requires the given iterator returns exactly #this.inputs elements
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
}
