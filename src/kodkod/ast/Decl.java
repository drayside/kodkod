/*
 * Declaration.java
 * Created on May 24, 2005
 */
package kodkod.ast;


import kodkod.ast.visitor.ReturnVisitor;
import kodkod.ast.visitor.VoidVisitor;




/** 
 * Represents a variable declaration, such as 'x : lone X'.  Declarations
 * are used with quantified formulas and comprehension expressions.
 * 
 * @specfield variable: Variable
 * @specfield expression: Expression
 * @specfield multiplicity: LONE + ONE + SOME + SET
 * @invariant variable.arity = expression.arity
 * @invariant children = variable + expression
 * @author Emina Torlak 
 */
public final class Decl extends Decls {
	
    private final Variable variable;
    private final Multiplicity mult;
    private final Expression expression;
    private final int hashCode;
    
    /**  
     * Constructs a new declaration from the specified variable and
     * expression, with the specified order.
     * 
     * @effects this.variable' = variable && this.expression' = expression && this.multiplicity' = mult
     * @throws NullPointerException - variable = null || expression = null || mult = null
     * @throws IllegalArgumentException - variable.arity != expression.arity 
     */
    Decl(Variable variable, Multiplicity mult, Expression expression) {
    		if (mult==Multiplicity.NO)
    			throw new IllegalArgumentException("NO is not a valid multiplicity in a declaration.");
        if (variable.arity() != expression.arity())
            throw new IllegalArgumentException("Unmatched arities in a declaration: " + variable + " and " + expression);
        if (mult != Multiplicity.SET && expression.arity()>1) 
        		throw new IllegalArgumentException("Cannot use multiplicity " + mult + " with an expression of arity > 1.");
        this.variable = variable;
        this.mult = mult;
        this.expression = expression;
        this.hashCode = variable.hashCode() + mult.hashCode() + expression.hashCode();
    }
    
    /**
     * Returns the variable in this declaration.
     * @return this.variable
     */
    public Variable variable() { return variable; }
    
    /**
     * Returns the multiplicity in this declaration.
     * @return this.multiplicity
     */
    public Multiplicity multiplicity() { return mult; }
    
    /**
     * Returns the expression in this declaration.
     * @return this.exresssion
     */
    public Expression expression() { return expression;  }
    
    /**
     * Accepts the given visitor and returns the result.
     * @see kodkod.ast.Node#accept(kodkod.ast.visitor.ReturnVisitor)
     */
    public final <E, F, D, I> D accept(ReturnVisitor<E, F, D, I> visitor) {
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
     * Returns true of o is a Declaration with the
     * same tree structure as this.
     * @return o.variable.equals(this.variable) && o.multiplicity.equals(this.multiplicity) && o.expression.equals(this.expression) 
     */
    public boolean equals(Object o) {
    	if (this == o) return true;
    	if (!(o instanceof Decl)) return false;
    	Decl that = (Decl)o;
    	return variable.equals(that.variable) && mult.equals(that.mult) &&
    		expression.equals(that.expression);
    }
    
    /**
     * {@inheritDoc}
     * @see java.lang.Object#hashCode()
     */
    public int hashCode() {
    	return hashCode;
    }
    
    /**
     * {@inheritDoc}
     * @see java.lang.Object#toString()
     */
    public String toString() { return variable + ": " + mult + " " + expression; }
   
}
