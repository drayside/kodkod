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
 * A quantified formula.
 * 
 * @specfield declarations: Declarations
 * @specfield formula: Formula
 * @specfield quantifier: Quantifier
 * @invariant children = declarations + formula
 * @author Emina Torlak 
 */
public final class QuantifiedFormula extends Formula  {
    private final Quantifier quantifier;
    private final Decls declarations;
    private final Formula formula;
   
    /**  
     * Constructs a new quantified formula: quantifier declarations | formula
     * 
     * @effects this.quantifier' = quantifier && this.declarations' = declarations &&
     *          this.formula' = formula
     * @throws NullPointerException - quantifier = null || declarations = null || formula = null
     */
    QuantifiedFormula(Quantifier quantifier, Decls declarations, Formula formula) {
        if (quantifier == null || declarations == null || formula == null) {
            throw new NullPointerException("null arg");
        }
        this.quantifier = quantifier;
        this.declarations = declarations;
        this.formula = formula;
    }
    
    /**
     * Returns this.formula.
     * @return this.formula
     */
    public Formula formula() { return formula; }
    
    /**
     * Returns this.declarations.
     * @return this.declarations
     */
    public Decls declarations() { return declarations;}
    
    /**
     * Returns this.quantifier.
     * @return this.quantifier
     */
    public Quantifier quantifier() { return quantifier; }
    
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
	 * Returns the string representation of this formula.
	 * @return string representation of this formula
	 */
    public String toString() {
        return "(" + quantifier + " " + declarations + " | " + formula + ")";
    }

    /**
     * Represents a logical quantifier.
     */
    public static enum Quantifier {
    	/** Universal quantifier. */
        ALL  { public String toString() { return "all"; }},
        /** Existential quantifier. */
        SOME { public String toString() { return "some"; }}
    }

}
