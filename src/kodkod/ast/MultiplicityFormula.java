/*
 * MultiplicityFormula.java
 * Created on Jul 1, 2005
 */
package kodkod.ast;


import kodkod.ast.visitor.ReturnVisitor;
import kodkod.ast.visitor.VoidVisitor;


/** 
 * Represents a multiplicity formula, e.g. some x
 * 
 * @specfield expression: Expression
 * @specfield multiplicity: (ONE + LONE + SOME + NO)
 * @invariant children = expression
 * @author Emina Torlak 
 */
public final class MultiplicityFormula extends Formula {
    private final Expression expression;
    private final Multiplicity multiplicity;
    private final int hashCode;
    
    /**  
     * Constructs a new multiplicity formula:  multiplicity expression
     * 
     * @effects this.expression' = expression && this.multiplicity' = multiplicity
     * @throws NullPointerException - multiplicity = null || expression = null
     * @throws IllegalArgumentException - multiplicity = SET
     */
    MultiplicityFormula(Multiplicity multiplicity, Expression expression) {
    		if (multiplicity==Multiplicity.SET) throw new IllegalArgumentException("invalid expression mulitplicity: SET");
        if (multiplicity== null || expression == null) throw new NullPointerException("null arg");
        this.multiplicity = multiplicity;
        this.expression = expression;
        this.hashCode = multiplicity.hashCode() + expression.hashCode();
    }
    
    /**
     * Returns the mulitplicity of this.
     * @return this.multiplicity
     */
    public Multiplicity multiplicity() { return multiplicity; }
    
    /**
     * Returns the expression of this.
     * @return this.expression
     */
    public Expression expression() { return expression; }
    
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
     * Returns true of o is a MultiplicityFormula with the
     * same tree structure as this.
     * @return o.multiplicity.equals(this.multiplicity) && o.expression.equals(this.expression) 
     */
    public boolean equals(Object o) {
    	if (this == o) return true;
    	if (!(o instanceof MultiplicityFormula)) return false;
    	MultiplicityFormula that = (MultiplicityFormula)o;
    	return multiplicity.equals(that.multiplicity) &&
    		expression.equals(that.expression);
    }
    
    public int hashCode() {
    	return hashCode;
    }
    
    public String toString() {
        return multiplicity + " " + expression;
    }
}
