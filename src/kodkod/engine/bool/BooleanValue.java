package kodkod.engine.bool;

/** 
 * Represents a boolean value, which may be a {@link kodkod.engine.bool.BooleanFormula formula} 
 * or a {@link kodkod.engine.bool.BooleanConstant constant}.  Boolean formulas are produced by 
 * {@link kodkod.engine.bool.BooleanFactory circuit factories}.  Each value is associated with 
 * an integer label; the labels are unique within a given factory.  
 * A boolean value with a negative label -|l| represents the negation of the value with the positive
 * label |l|. Non-constant values are not shared among factories.  
 * 
 * @specfield op: Operator
 * @specfield label: [-Integer.MAX_VALUE, Integer.MAX_VALUE]
 * @invariant no c: BooleanValue - this | some components.c & components.this && c.label = this.label
 * @author Emina Torlak  
 */
public abstract class BooleanValue implements Comparable<BooleanValue> {

	BooleanValue() {}
	
	/**
	 * Returns the negation of this boolean value
	 * @return { f: BooleanFormula | [[f]] = ![[this]] }
	 */
	abstract BooleanValue negation();
	
	/**
	 * Returns the label for this value. 
	 * @return this.label
	 */
	public abstract int label();
	
	/**
	 * Returns the operator representing the function
	 * computed by this gate.
	 * @return this.op
	 */
	public abstract Operator op();
	
	/**
	 * Boolean components are ordered according to their labels.
	 * Note that the ordering is well defined on components produced by the same factory.
	 * Specifically, this comparison function is consistent with equals for the components
	 * produced by the same factory, but may not be for the components produced by different factories. 
	 * @return 0 if the label of this and other are the same, a negative
	 * integer if the label of this is smaller than the label of other; and
	 * a positive integer otherwise.
	 */
	public final int compareTo(BooleanValue other) {
		return label() - other.label();
	}
}
