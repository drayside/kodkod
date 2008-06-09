/* 
 * Kodkod -- Copyright (c) 2005-2007, Emina Torlak
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package kodkod.ast;

import java.util.Arrays;
import java.util.Iterator;

import kodkod.ast.visitor.ReturnVisitor;
import kodkod.ast.visitor.VoidVisitor;
import kodkod.util.collections.Containers;


/** 
 * A sequence of declarations. 
 * 
 * @specfield size: int
 * @specfield declarations: [0..size) -> one Decl
 * @invariant size > 0
 * @invariant children = declarations[int]
 * @author Emina Torlak 
 */
public class Decls implements Node, Iterable<Decl> {
	private final Decl[] declarations;
	
	/**
	 * Constructs a Decls object with itself as its sole
	 * declaration.  This constructor can only be called
	 * from inside the Decl constructor; otherwise it will
	 * throw a ClassCastException.
	 * @effects this.declarations' = 0->this
	 * @throws ClassCastException - this !in Decl
	 */
    Decls() {
    	this.declarations = new Decl[]{ (Decl) this };
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
		this.declarations = new Decl[head.size()+tail.size()];
		System.arraycopy(head.declarations, 0, declarations, 0, head.size());
		System.arraycopy(tail.declarations, 0, declarations, head.size(), tail.size());
	}
    
    /**
     * Returns the number of declarations in this Decls object.
     * @return this.size
     */
    public int size() { return declarations.length; }
    
    /**
     * Returns the ith declaration in this Decls sequence.
     * @requires 0 <= i < this.size
     * @return this.declarations[i]
     */
    public Decl get(int i) { return declarations[i]; }
    
    /**
     * Returns an unmodifiable iterator over the declarations in this Decls object.
     * @return this.declarations().iterator()
     */
    public Iterator<Decl> iterator() { return Containers.iterate(declarations); }
    
    /**
     * Returns a sequence of this.size + decls.size declarations that has 
     * these declarations as the prefix and the given declarations as the suffix.
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
     * @see kodkod.ast.Node#accept(kodkod.ast.visitor.ReturnVisitor)
     */
    public <E, F, D, I> D accept(ReturnVisitor<E, F, D, I> visitor) {
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
	 * Returns the string representation of these decls.
	 * @return string representation of these decls
	 */
    public String toString() {
        return Arrays.toString(declarations);
    }
    
}
