/*
 * Variable.java
 * Created on May 24, 2005
 */
package kodkod.ast;


/** 
 * Represents a variable in a quantified formula or a comprehension expression,
 * or a let statement, etc.  Two variables are the same if and only if they
 * refer to the same object.  That is, v1.eauls(v2) <=> v1 == v2.  Each
 * variable has a name, which is basically a comment for the purpose of 
 * printing, viewing, etc.  The name has no meaning otherwise.  The arity of
 * a variable specifies the arity of expressions over which the variable can
 * range.
 * 
 * @specfield name: String
 * @specfield arity: int
 * @invariant no children
 * @author Emina Torlak 
 */
public final class Variable extends LeafExpression {

    /**
     * Constructs a variable with the specified name and arity 1.
     * @effects this.name' = name && this.arity' = 1
     */
    Variable(String name) {
    	super(name, 1);
    }
    
    /**
     * Returns a new variable with the specified name and arity 1.
     * @effects this.name' = name && this.arity' = 1
     */
    public static Variable unary(String name) {
    	return new Variable(name);
    }
    
    /**
     * Returns the declaration that constrains this variable to 
     * be a singleton element of the given expression:  'this: one expr'.
     * @return {d: Decl | d.variable = this && d.expression = expr }
     * @throws NullPointerException - expression = null
     * @throws IllegalArgumentException - expression.arity != 1
     */
    public Decl oneOf(Expression expr) {
    	return new Decl(this, expr);
    }
   
    /**
     * Returns true of o is a BinaryFormula with the
     * same tree structure as this.
     * @return o.op.equals(this.op) && o.left.equals(this.left) && o.right.equals(this.right) 
     */
    public <E, F, D> E accept(Visitor<E, F, D> visitor) {
        return visitor.visit(this);
    }
   

}
