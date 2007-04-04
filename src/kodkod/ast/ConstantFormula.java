/*
 * BooleanLiteral.java
 * Created on Oct 12, 2005
 */
package kodkod.ast;


import kodkod.ast.visitor.ReturnVisitor;
import kodkod.ast.visitor.VoidVisitor;

/** 
 * A boolean literal {@link kodkod.ast.Formula formula}, true or false.
 *
 * @invariant no children
 * @author Emina Torlak 
 */
public abstract class ConstantFormula extends Formula {
	private final boolean value;
	/**
	 * Constructs a constant formula with the given value.
	 */
	ConstantFormula(boolean value) {
		this.value = value;
	}
    
	/**
	 * Returns the boolean value that corresponds to this 
	 * constant formula.
	 * @return this=TRUE => true, false
	 */
	public final boolean booleanValue() { return value; }
	
	/**
     * Accepts the given visitor and returns the result.
     * @see kodkod.ast.Node#accept(kodkod.ast.visitor.ReturnVisitor)
     */
	@Override
	 public <E, F, D, I> F accept(ReturnVisitor<E, F, D, I> visitor) {
        return visitor.visit(this);
    }
	
	/**
     * Accepts the given visitor.
     * @see kodkod.ast.Node#accept(kodkod.ast.visitor.VoidVisitor)
     */
    public void accept(VoidVisitor visitor) {
        visitor.visit(this);
    }
	
    /**
	 * Returns the string representation of this formula.
	 * @return string representation of this formula
	 */
	public String toString() {
		return String.valueOf(booleanValue());
	}
}

