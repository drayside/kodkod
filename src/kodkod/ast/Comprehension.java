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


import kodkod.ast.visitor.ReturnVisitor;
import kodkod.ast.visitor.VoidVisitor;



/** 
 * A comprehension expression, e.g. { a: A, b: B | a.r = b }
 *  
 * @specfield declarations: Declarations
 * @specfield formula: Formula
 * @invariant arity = sum(declarations.declarations().arity)
 * @invariant children = declarations + formula
 * @author Emina Torlak 
 */
public final class Comprehension extends Expression  {

    private final Decls declarations;
    private final Formula formula;

    
    /**  
     * Constructs a comprehension expression with the specified declarations
     * and formula
     * 
     * @effects this.declarations' = declarations && this.formula' = formula
     * @throws NullPointerException - declarations = null || formula = null
     */
    Comprehension(Decls declarations, Formula formula) {
        if (formula == null) throw new NullPointerException("null formula");
        for(Decl decl : declarations) { 
        	if (decl.variable().arity()>1 || decl.multiplicity()!=Multiplicity.ONE)
        		throw new IllegalArgumentException("Cannot have a higher order declaration in a comprehension: "+decl);
        }
        this.declarations = declarations;
        this.formula = formula;
    }

    /**
     * @return this.formula
     */
    public Formula formula() {return formula; }
    
    /**
     * @return this.declarations
     */
    public Decls declarations() {return declarations;}
    
    /**
     * Returns the arity of this comprehension expression, which is the sum
     * of the arities of declared variables
     * 
     * @return sum(this.declarations.declarations().arity)
     */
    public int arity() {
        return declarations.size();
    }
    
    /**
     * Accepts the given visitor and returns the result.
     * @see kodkod.ast.Node#accept(kodkod.ast.visitor.ReturnVisitor)
     */
    public <E, F, D, I> E accept(ReturnVisitor<E, F, D, I> visitor) {
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
	 * Returns the string representation of this expression.
	 * @return string representation of this expression
	 */
    public String toString() {
        return "{ " + declarations().toString() + " | " + formula().toString() + " }";
    }

}
