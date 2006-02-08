package kodkod.engine.bool;


/** 
 * Represents a boolean value, which may be a {@link kodkod.engine.bool.BooleanFormula formula} 
 * or a {@link kodkod.engine.bool.BooleanConstant constant}.  Boolean formulas are produced by 
 * {@link kodkod.engine.bool.BooleanFactory circuit factories}.  Each value is associated with 
 * an integer literal, which serves as its CNF identifer; the literals are unique within a given factory.  
 * A boolean value with a negative literal -|l| represents the negation of the value with the positive
 * literal |l|. Non-constant values are not status among factories.  
 * 
 * @specfield literal: [-Integer.MAX_VALUE, Integer.MAX_VALUE]
 * @invariant no c: BooleanValue - this | some components.c & components.this && c.literal = this.literal
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
	 * Returns the CNF identifier of this value. 
	 * @return this.literal
	 */
	public abstract int literal();
	
	
	/**
	 * Boolean components are ordered according to their literals.
	 * Note that the ordering is well defined on components produced by the same factory.
	 * Specifically, this comparison function is consistent with equals for the components
	 * produced by the same factory, but may not be for the components produced by different factories. 
	 * @return 0 if the literal of this and other are the same, a negative
	 * integer if the literal of this is smaller than the literal of other; and
	 * a positive integer otherwise.
	 */
	public final int compareTo(BooleanValue other) {
		return literal() - other.literal();
	}
	
	/**
	 * Passes this value and the given
	 * argument value to the visitor, and returns the resulting value.
	 * @return the value produced by the visitor when visiting this node
	 * with the given argument.
	 */
	public abstract <T, A> T accept(BooleanVisitor<T, A> visitor, A arg);
	
}
