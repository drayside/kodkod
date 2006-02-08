package kodkod.engine.bool;

import java.util.Iterator;

import kodkod.util.IndexedEntry;
import kodkod.util.SparseSequence;
import kodkod.util.TreeSequence;


/**
 * Represents a mutable multi gate that allows incremental addition of 
 * inputs.  A mutable gate cannot be combined with other boolean values
 * using the BooleanFactory.compose methods.  To use the circuit
 * represented by a mutable gate, one must first convert it into an immutable multigate
 * by calling BooleanFactory.toImmutableValue(MutableMultiGate g).  
 * 
 * @author Emina Torlak
 */
public abstract class MutableMultiGate extends MultiGate {

	/**
	 * Constructs a new mutable MultiGate gate with the given
	 * operator and literal.
	 * @requires op != null && literal >= 0
	 * @effects this.op' = op && this.literal' = literal
	 */
	private MutableMultiGate(Operator op, int literal) {
		super(op, literal);
	}
	
	/**
	 * Returns an array-based implementation of MutableMultiGate.  This
	 * is implementation is preferable to the one based on sparse sequences
	 * when the maximum number of gate inputs is known in advance (and that
	 * number is small).  The addInput operation executes in O(maxInputs) time.
	 * The effect of this method is the same as calling arrayBased(op, 0, maxInputs).
	 * @return an array-based MutableMultiGate with the given operator, 0 literal, and
	 * maximum possible number of inputs.
	 * @throws ArrayIndexOutOfBoundsException - maxInputs < 0
	 */
//	public static MutableMultiGate arrayBased(Operator op, int maxInputs) {
//		return new ArrayGate(op, maxInputs, 0);
//	}
	
	/**
	 * Returns an array-based implementation of MutableMultiGate.  This
	 * is implementation is preferable to the one based on sparse sequences
	 * when the maximum number of gate inputs is known in advance (and that
	 * number is small).  The addInput operation executes in O(maxInputs) time.
	 * @return an array-based MutableMultiGate with the given operator, literal, and
	 * maximum possible number of inputs.
	 * @throws ArrayIndexOutOfBoundsException - maxInputs < 0
	 * @throws IllegalArgumentException - literal < 0
	 */
//	public static MutableMultiGate arrayBased(Operator op, int literal, int maxInputs) {
//		if (literal < 0) throw new IllegalArgumentException("literal < 0");
//		return new ArrayGate(op, maxInputs, literal);
//	}
	
	/**
	 * Returns a tree based implementation of MutableMultiGate.
	 * The addInput operation executes in O(lg n) time where n is the number of gate inputs. 
	 * @return a sparse sequence based MutableMultiGate with the given operator and
	 * 0 as its literal.
	 * @throws NullPointerException - op = null
	 */
	public static MutableMultiGate treeGate(Operator op) {
		if (op==null) throw new NullPointerException();
		return new TreeGate(op, 0);
	}
	
	/**
	 * Returns a sparse sequence based implementation of MutableMultiGate.
	 * This implementation is preferable to the array based one when the
	 * number of gate inputs is not known in advance.  The addInput operation
	 * executes in O(lg n) time where n is the number of gate inputs.
	 * @return a sparse sequence based MutableMultiGate with the given operator
	 * and literal.
	 * @throws IllegalArgumentException - literal < 0
	 */
//	static MutableMultiGate logGate(Operator op, int literal) {
//		if (literal < 0) throw new IllegalArgumentException("literal < 0");
//		return new TreeGate(op, literal);
//	}
		
	/**
	 * Adds the given input to this.inputs and returns the result.  Specifically,
	 * if the addition of the input causes the gate to evaluate to op.shortCircuit,
	 * then this.inputs is set to op.shortCircuit.  If the given value has already 
	 * been added or it is equal to this.op.identity, nothing changes.  Otherwise, v 
	 * is added to this.input.  The method returns this.op.shortCircuit if this.inputs
	 * contains it after the addition, otherwise it returns the gate itself.  
	 * @effects v = this.op.shortCircuit || v.negation in this.inputs => this.inputs' = this.op.shortCircuit,
	 *          v !in BooleanConstant => this.inputs' = this.inputs + v, 
	 *          this.inputs' = this.inputs
	 * @return this.inputs' = op.shortCircuit => op.shortCircuit, this
	 * @throws ArrayIndexOutOfBoundsException - this gate can have no more inputs
	 */
	public abstract BooleanValue addInput(BooleanValue v);
	
	/**
	 * Returns true if this gate is short circuited; that is,
	 * its inputs are reduced to this.op.shortCircuit.
	 * @return this.inputs = this.op.shortCircuit
	 */
	public abstract boolean isShortCircuited();

	/**
	 * Throws an IllegalArgumentException if op != this.op,
	 * otherwise returns the sum of digests of this gate's
	 * inputs with respect to the given operator.
	 * @return op = this.op => sum(this.inputs.digest(op))
	 * @throws IllegalArgumentException - op != this.op
	 * @throws ClassCastException - some this.inputs & BooleanConstant
	 */
	@Override
	int digest(Operator op) {
		if (op != op()) throw new IllegalArgumentException();
		int d = 0;
		for(Iterator<BooleanValue> inputs = inputs(); inputs.hasNext();) {
			d += ((BooleanFormula)inputs.next()).digest(op);
		}
		return d;
	}

	/**
	 * Throws an IllegalArgumentException if op != this.op,
	 * otherwise returns the number of atomic parts comprising this gate
	 * with respect to the given operator.
	 * @return op = this.op => sum(this.inputs.numAtomicParts(op))
	 * @throws IllegalArgumentException - op != this.op
	 * @throws ClassCastException - some this.inputs & BooleanConstant
	 */
	@Override
	int numAtomicParts(Operator op) {
		if (op != op()) throw new IllegalArgumentException();
		int n = 0;
		for(Iterator<BooleanValue> inputs = inputs(); inputs.hasNext();) {
			n += ((BooleanFormula)inputs.next()).numAtomicParts(op);
		}
		return n;
	}

	/**
	 * Implementation of a MutableGate based on a SparseSequence.
	 */
	private static final class TreeGate extends MutableMultiGate {
		private final SparseSequence<BooleanValue> inputs;
		
		/**
		 * Constructs a new tree gate with the given operator and literal.
		 * @effects this.op' = op && this.literal' = literal
		 */
		TreeGate(Operator op, int literal) {
			super(op, literal);
			inputs = new TreeSequence<BooleanValue>();
		}
		
		@Override
		public int numInputs() {
			return inputs.size();
		}
		
		@Override
		public boolean isShortCircuited() {
			return inputs.size()==1 && inputs.first().value()==op.shortCircuit();
		}
		
		@Override
		public BooleanValue addInput(BooleanValue v) {
			if (isShortCircuited()) return op.shortCircuit(); 
			else{ 
				final int lit = v.literal();
				if (v==op.shortCircuit() || inputs.containsIndex(-lit)) {
					inputs.clear();
					inputs.put(op.shortCircuit().literal, op.shortCircuit());
					return op.shortCircuit();
				} 
				if (v!=op.identity() && !inputs.containsIndex(lit)) { inputs.put(lit, (BooleanValue) v);	}
				return this;
			}
		}
		
		@Override
		public Iterator<BooleanValue> inputs() {
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
		
	}
	
//	/**
//	 * Implementation of a MutableGate based on an array. 
//	 */
//	private static final class ArrayGate extends MutableMultiGate {
//		private BooleanValue[] inputs;
//		private int size;
//		
//		/**
//		 * Constructs an array based mutable multi gate with
//		 * the given literal that can have up to distinct
//		 * non-constant maxInputs.
//		 * @effects this.op' = op && no this.inputs'
//		 */
//		ArrayGate(Operator op, int maxInputs, int literal) {
//			super(op, literal);
//			inputs = new BooleanValue[maxInputs];
//			size = 0;
//		}
//	
//		@Override
//		public int numInputs() { return size;	}
//		
//		@Override
//		boolean isShortCircuited() {
//			return inputs[0]==op.shortCircuit();
//		}
//		
//		/**
//		 * Returns true if this array gate contains an
//		 * input with the given literal; otherwise returns false.
//		 * @return lit in this.inputs.literal
//		 */
//		private boolean containsInput(final int lit) {
//			for(int i = 0; i < size; i++) {
//				if (inputs[i].literal()==lit) {
//					return true;
//				}
//			}
//			return false;
//		}
//		
//		@Override
//		BooleanValue addInput(BooleanValue v) {
//			if (isShortCircuited()) return inputs[0];
//			else {
//				final int lit = v.literal();
//				if (v==op.shortCircuit() || containsInput(-lit)) {
//					inputs = new BooleanValue[1];
//					inputs[0] = op.shortCircuit();
//					size = 1;
//					return inputs[0];
//				}
//				if (v!=op.identity() && !containsInput(lit)) { inputs[size++] = v;	}
//				return this;
//			}
//		}
//
//		@Override
//		public Iterator<BooleanValue> inputs() {
//			Arrays.sort(inputs);
//			return Iterators.iterate(0, size, inputs);
//		}
//		
//	}
}
