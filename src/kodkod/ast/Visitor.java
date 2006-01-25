/*
 * Visitor.java
 * Created on May 20, 2005
 */
package kodkod.ast;


/** 
 * A visitor that visits every node in the AST, returning some value for each.
 * The methods that visit an {@link kodkod.ast.Expression expression}, 
 * {@link kodkod.ast.Formula formula}, and {@link kodkod.ast.Decls declarations}
 * return values of types E, F, and D, respectively. 
 *
 *
 * @author Emina Torlak  
 */
public interface Visitor<E, F, D> {
    
	/** 
	 * Visits the given sequence of declarations and returns the result.
	 * @return the result of visiting <code>decls</code> 
	 **/
    public D visit(Decls decls);
    /** 
	 * Visits the given declaration and returns the result.
	 * @return the result of visiting <code>decl</code> 
	 **/
    public D visit(Decl decl);
    
    /** 
	 * Visits the given relation and returns the result.
	 * @return the result of visiting <code>relation</code> 
	 **/
    public E visit(Relation relation);
    /** 
	 * Visits the given variable and returns the result.
	 * @return the result of visiting <code>variable</code> 
	 **/
    public E visit(Variable variable);
    /** 
	 * Visits the given constant expression and returns the result.
	 * @return the result of visiting <code>constExpr</code> 
	 **/
    public E visit(ConstantExpression constExpr);
    
    /** 
	 * Visits the given binary expression and returns the result.
	 * @return the result of visiting <code>binExpr</code> 
	 **/
    public E visit(BinaryExpression binExpr);
    /** 
	 * Visits the given unary expression and returns the result.
	 * @return the result of visiting <code>unaryExpr</code> 
	 **/
    public E visit(UnaryExpression unaryExpr);   
    /** 
	 * Visits the given comprehension and returns the result.
	 * @return the result of visiting <code>comprehension</code> 
	 **/
    public E visit(Comprehension comprehension);
    /** 
	 * Visits the given if-then expression and returns the result.
	 * @return the result of visiting <code>ifExpr</code> 
	 **/
    public E visit(IfExpression ifExpr);
    
    /** 
	 * Visits the given quantified formula and returns the result.
	 * @return the result of visiting <code>quantFormula</code> 
	 **/
    public F visit(QuantifiedFormula quantFormula);
    /** 
	 * Visits the given binary formula and returns the result.
	 * @return the result of visiting <code>binFormula</code> 
	 **/
    public F visit(BinaryFormula binFormula);
    /** 
	 * Visits the given negation and returns the result.
	 * @return the result of visiting <code>not</code> 
	 **/
    public F visit(NotFormula not);
    /** 
	 * Visits the given constant formula and returns the result.
	 * @return the result of visiting <code>constant</code> 
	 **/
    public F visit(ConstantFormula constant);
    
    /** 
	 * Visits the given comparison formula and returns the result.
	 * @return the result of visiting <code>compFormula</code> 
	 **/
    public F visit(ComparisonFormula compFormula);
    /** 
	 * Visits the given multiplicity formula and returns the result.
	 * @return the result of visiting <code>multFormula</code> 
	 **/
    public F visit(MultiplicityFormula multFormula);
    /**
     * Visits the given relation predicate and returns the result.
     * @return the result of visiting <code>predicate</code>
     */
    public F visit(RelationPredicate predicate);

    
}
