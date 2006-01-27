/*
 * Comprehension.java
 * Created on May 24, 2005
 */
package kodkod.ast;



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
    private final int hashCode;

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
        this.hashCode = declarations.hashCode() + formula.hashCode();
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
     * @see kodkod.ast.Node#accept(kodkod.ast.Visitor)
     */
    public <E, F, D> E accept(Visitor<E, F, D> visitor) {
        return visitor.visit(this);
    }
    
    /**
     * Returns true of o is a Comprehension with the
     * same tree structure as this.
     * @return o.declarations.equals(this.declarations) && o.formula.equals(formula) 
     */
    public boolean equals(Object o) {
    	if (this == o) return true;
    	if (!(o instanceof Comprehension)) return false;
    	Comprehension that = (Comprehension)o;
    	return declarations.equals(that.declarations) &&
    		formula.equals(that.formula);
    }
    
    public int hashCode() {
    	return hashCode;
    }
    
    public String toString() {
        return "{ " + declarations().toString() + " | " + formula().toString() + " }";
    }

}
