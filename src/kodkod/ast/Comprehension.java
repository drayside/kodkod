/*
 * Comprehension.java
 * Created on May 24, 2005
 */
package kodkod.ast;


import kodkod.ast.visitor.ReturnVisitor;
import kodkod.ast.visitor.VoidVisitor;



/** 
 * Represents a comprehension expression
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
        if (declarations == null || formula == null) throw new NullPointerException("null arg");
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
        int arity = 0;
        for (Decl decl : declarations.declarations()) {
            arity += decl.variable().arity();
        }
        return arity;
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
