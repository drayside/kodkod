/*
 * NotFormula.java
 * Created on May 11, 2005
 */
package kodkod.ast;


import kodkod.ast.visitor.ReturnVisitor;
import kodkod.ast.visitor.VoidVisitor;


/**
 * Represents a negation of a {@link kodkod.ast.Formula formula}
 * 
 * @specfield formula: Formula
 * @invariant children = formula
 * @author Emina Torlak
 */
public final class NotFormula extends Formula {
    private final Formula formula;
    
    /**
     * Constructs a new formula: !formula 
     * 
     * @effects this.formula' = formula
     * @throws NullPointerException - formula = null
     */
    NotFormula(Formula child) {
        if (child == null) throw new NullPointerException("formula");
        this.formula = child;
    }
 
    /**
     * Returns this.formula.
     * @return this.formula
     */
    public Formula formula() { return formula; }
    
    /**
     * Returns the negation of this negation, which is this.formula.
     * @return this.formula
     */
    @Override
	public Formula not() { return formula; }

    /**
     * Accepts the given visitor and returns the result.
     * @see kodkod.ast.Node#accept(kodkod.ast.visitor.ReturnVisitor)
     */
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
        return "!" + formula;
    }
}
