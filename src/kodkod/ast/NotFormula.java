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
    private final int hashCode;
    
    /**
     * Constructs a new formula: !formula 
     * 
     * @effects this.formula' = formula
     * @throws NullPointerException - formula = null
     */
    NotFormula(Formula child) {
        if (child == null) throw new NullPointerException("formula");
        this.formula = child;
        this.hashCode = formula.hashCode() + 31;
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
    public <E, F, D> F accept(ReturnVisitor<E, F, D> visitor) {
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
     * Returns true of o is a NotFormula with the
     * same tree structure as this.
     * @return o in NotFormula && o.formula.equals(this.formula) 
     */
    public boolean equals(Object o) {
    	if (this == o) return true;
    	if (!(o instanceof NotFormula)) return false;
    	NotFormula that = (NotFormula)o;
    	return formula.equals(that.formula);
    }
    
    public int hashCode() {
    	return hashCode;
    }
   
    public String toString() {
        return "!" + formula;
    }
}
