/*
 * Node.java
 * Created on May 5, 2005
 */
package kodkod.ast;


/**
 * Represents a node in the abstract syntax tree.  A node
 * can accept a Visitor and have zero or more children.
 * 
 * @specfield children: set Node
 * @author Emina Torlak
 */
public interface Node {
    
    /**
     * Accepts the given visitor and returns the result
     * of the visit.
     * @return the result of being visited by the given visitor
     * @throws NullPointerException visitor = null
     * @see kodkod.ast.Node#accept(kodkod.ast.Visitor)
     */
    public <E, F, D> Object accept(Visitor<E, F, D> visitor);
}
