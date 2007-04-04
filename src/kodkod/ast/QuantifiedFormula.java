/*
 * QuantifiedFormula.java
 * Created on Jul 1, 2005
 */
package kodkod.ast;


import kodkod.ast.visitor.ReturnVisitor;
import kodkod.ast.visitor.VoidVisitor;


/** 
 * Represents a quantified formula.
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
        ALL  { public String toString() { return "all"; }},
        SOME { public String toString() { return "some"; }}
    }

}
