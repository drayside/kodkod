/*
 * Declarations.java
 * Created on May 24, 2005
 */
package kodkod.ast;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;


/** 
 * Represents a sequence of declarations. 
 * 
 * @specfield size: int
 * @specfield declarations: [0..size) -> one Decl
 * @invariant size > 0
 * @invariant children = declarations[int]
 * @author Emina Torlak 
 */
public class Decls implements Node, Iterable<Decl> {
	private final List<Decl> declarations;
	private final int hashCode;
	
	/**
	 * Constructs a Decls object with itself as its sole
	 * declaration.  This constructor can only be called
	 * from inside the Decl constructor; otherwise it will
	 * throw a ClassCastException.
	 * @effects this.declarations' = 0->this
	 * @throws ClassCastException - this !in Decl
	 */
    Decls() {
    	Decl singleDecl = (Decl)this;
    	this.declarations = Collections.singletonList(singleDecl);
    	this.hashCode = 0; // overridden by Decl
    }
    
    /**
	 * Constructs a new DeclSequence with the specified head and tail.
	 * @requires head.size > 0 && tail.size > 0
	 * @effects this.size' = head.size + tail.size &&
	 *          (all i: [0..head.size) | this.declarations[i] = head.declarations[i]) &&
	 *          (all i: [head.size..this.size') | this.declarations[i] = tail.declarations[i])
	 * @throws NullPointerException - head = null || tail is null 
	 */
	private Decls(Decls head, Decls tail) {
		List<Decl> temp = new ArrayList<Decl>(head.declarations.size() + tail.declarations.size());
		temp.addAll(head.declarations());
		temp.addAll(tail.declarations());
		this.declarations = Collections.unmodifiableList(temp);
		this.hashCode = declarations.hashCode();
	}
	
    /**
     * Returns an unmodifiable List view of this declaration sequence
     * @return {l: List | l.elems =  this.declarations }
     */
    public List<Decl> declarations() {
    	return declarations;
    }
    
    /**
     * Returns the number of declarations in this Decls object.
     * @return this.size
     */
    public int size() { return declarations.size(); }
    
    /**
     * Returns an unmodifiable iterator over the declarations in this Decls object.
     * @return this.declarations().iterator()
     */
    public Iterator<Decl> iterator() { return declarations.iterator(); }
    
    /**
     * Returns a sequence of this.size + decls.size declarations that has 
     * these declarations as the prefix and the given declarations as the suffix.
     * This method requires that no declaration in the argument object depends
     * on a variable declared in this.decls.  
     * @requires no declaration in decls depends on a variable declared in this.decls
     * @return {ds: Decls | ds.size = this.size + decls.size && 
     *                      ds.declarations = this.declarations + 
     *                      {i: [this.size..this.size+decls.size), d: Decl | d = decls.declarations[i-this.size] }
     * @throws NullPointerException - decl = null
     */
    public final Decls and(Decls decls) {
    	return new Decls(this, decls);
    }
    
    /**
     * Accepts the given visitor and returns the result.
     * @see kodkod.ast.Node#accept(kodkod.ast.Visitor)
     */
    public <E, F, D> D accept(Visitor<E, F, D> visitor) {
        return visitor.visit(this);
    }
    
    /**
     * Returns true of o is a Decls with the
     * same tree structure as this.
     * @return o.declarations.equals(this.declarations) 
     */
    public boolean equals(Object o) {
    	if (this == o) return true;
    	if (!(o instanceof Decls)) return false;
    	Decls that = (Decls)o;
    	return declarations.equals(that.declarations);
    }
    
    public int hashCode() {
    	return hashCode;
    }
    
    public String toString() {
        return declarations.toString();
    }
    
}
