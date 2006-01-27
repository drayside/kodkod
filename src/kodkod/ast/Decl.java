/*
 * Declaration.java
 * Created on May 24, 2005
 */
package kodkod.ast;




/** 
 * Represents a variable declaration, such as 'x : X'.  Declarations
 * are used with quantified formulas and comprehension expressions.
 * 
 * @specfield variable: Variable
 * @specfield expression: Expression
 * @invariant variable.arity = expression.arity
 * @invariant children = variable + expression
 * @author Emina Torlak 
 */
public final class Decl extends Decls {
	
    private final Variable variable;
    private final Expression expression;
    private final int hashCode;
    
    /**  
     * Constructs a new declaration from the specified variable and
     * expression, with the specified order.
     * 
     * @effects this.variable' = variable && this.expression' = expression &&
     * @throws NullPointerException - variable = null || expression = null
     * @throws IllegalArgumentException - variable.arity != expression.arity 
     */
    Decl(Variable variable, Expression expression) {
        if (variable.arity() != expression.arity())
            throw new IllegalArgumentException("Unmatched arities in a declaration.");
        this.variable = variable;
        this.expression = expression;
        this.hashCode = variable.hashCode() + expression.hashCode();
    }
    
    /**
     * Returns the variable in this declaration.
     * @return this.variable
     */
    public Variable variable() { return variable; }
    
    /**
     * Returns the expression in this declaration.
     * @return this.exresssion
     */
    public Expression expression() { return expression;  }
    
    /**
     * Accepts the given visitor and returns the result.
     * @see kodkod.ast.Node#accept(kodkod.ast.Visitor)
     */
    public final <E, F, D> D accept(Visitor<E, F, D> visitor) {
        return visitor.visit(this);
    }
    
    /**
     * Returns true of o is a Declaration with the
     * same tree structure as this.
     * @return o.variable.equals(this.variable) && o.expression.equals(this.expression) 
     */
    public boolean equals(Object o) {
    	if (this == o) return true;
    	if (!(o instanceof Decl)) return false;
    	Decl that = (Decl)o;
    	return variable.equals(that.variable) &&
    		expression.equals(that.expression);
    }
    
    public int hashCode() {
    	return hashCode;
    }
    
    public String toString() { return variable + ": " + expression; }
   
}
