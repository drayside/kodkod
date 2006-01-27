/*
 * LeafExpression.java
 * Created on May 24, 2005
 */
package kodkod.ast;

/** 
 * Represents a leaf expression -- an expression with no children.  
 * {@link kodkod.ast.Relation Relation} and {@link kodkod.ast.Variable Variable} 
 * are examples of leaf exressions.  Two leaf expressions are equal 
 * if and only if they refer to the same object.  That is, 
 * leaf1.eauls(leaf2) <=> leaf1 == leaf2.  A leaf has a name, which is 
 * basically a comment for the purpose of printing, viewing, etc.  The name 
 * has no meaning otherwise.  
 * 
 * @specfield name: String
 * @specfield arity: int
 * @specfield no children
 * @author Emina Torlak 
 */
public abstract class LeafExpression extends Expression {

    private final int arity;
    private final String name;
    
    /**
     * Constructs a leaf with the specified name and arity
     * 
     * @effects  this.name' = name && this.arity' = arity 
     * @throws IllegalArgumentException - arity < 1
     */
    LeafExpression(String name, int arity) {
        if (arity < 1) {
            throw new IllegalArgumentException("Arity must be at least 1: " + arity);
        }
        this.name = name;
        this.arity = arity;
    }
    

    /**
     * Returns the arity of this leaf.
     * @return this.arity
     */
    public final int arity() {
        return arity;
    }
    
    /**
     * Returns the name of this leaf.
     * @return this.name
     */
    public final String name() {
        return name;
    }
    
    public String toString() {
        return name;
    }
    
    /**
     * Returns true if and only if compared to itself; otherwise returns false
     * 
     * @return this = o
     */
    public final boolean equals(Object o) {
        return this == o;
    }

}
