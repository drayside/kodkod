package kodkod.engine.bool;

import java.util.Iterator;

import kodkod.engine.bool.MultiGate.Operator;


/**
 * Represents a non-constant boolean value.  A
 * formula may be a {@link kodkod.engine.bool.BooleanVariable variable}, 
 * {@link kodkod.engine.bool.NotGate inverter} or a multi-input
 * {@link kodkod.engine.bool.MultiGate circuit}.  
 * 
 * @specfield inputs: set BooleanValue
 * @author Emina Torlak
 */
public abstract class BooleanFormula extends BooleanValue {
	
	/**
	 * Constructs a new boolean formula.
	 */
	BooleanFormula() {	}
	
	/**
	 * Returns an integer summary of this formula, used
	 * to compute the summary of the composition of this and
	 * some other formula with the given operator.  
	 * @return an integer summary of this formula when acting
	 * as an input to a multigate with the given operator.
	 */
	abstract int digest(MultiGate.Operator op); 
	
	
	/**
	 * Returns the number of 'irreducible' parts of this circuit, 
	 * with respect to the given operator. For formulas that do
	 * not have operators with arity greater than one, this value is 
	 * always 1.  Hence, the default implementation returns 1.
	 * @return the number of atomic parts of this circuit
	 */
	int numAtomicParts(Operator op) {
		return 1;
	}
	
	/**
	 * Returns true if the given value is a part of this
	 * circuit with respect to the given operator, when this formula's
	 * tree is checked down to the given depth.  For formulas
	 * that do not have operators with arity greater than one,
	 * the result of this operation is true iff this==f.
	 * @requires depth >= 0
	 * @return this=f
	 */
	boolean contains(Operator op, BooleanFormula f, int depth) {
		return f==this;
	}
	
	/** 
	 * Returns true if this formula is a part of the given formula
	 * with respect to the specified operator, when the trees of this and f
	 * are checked to down to the depths thisDepth and fDepth, respectively.
	 * For formulas that do not have operators with arity greater than one,
	 * the result of this operation (the default implementation) is the
	 * same as calling f.contains(op, this, fDepth).
	 * @return f.contains(op, this, fDepth)
	 */
	boolean isPartOf(Operator op, BooleanFormula f, int thisDepth, int fDepth) {
		return f.contains(op, this, fDepth);
	}
	
	/**
	 * Returns an iterator over the inputs to this gate, in the order of 
	 * increasing literals.
	 * @return an iterator over this.inputs in the ascending literal order.
	 */
	public abstract Iterator<BooleanValue> inputs();
	
	/**
	 * Returns the number of inputs to this gate.
	 * @return #this.inputs
	 */
	public abstract int numInputs();
	
}
