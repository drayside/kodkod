package kodkod.ast.visitor;

import kodkod.ast.BinaryExpression;
import kodkod.ast.BinaryFormula;
import kodkod.ast.ComparisonFormula;
import kodkod.ast.Comprehension;
import kodkod.ast.ConstantExpression;
import kodkod.ast.ConstantFormula;
import kodkod.ast.Decl;
import kodkod.ast.Decls;
import kodkod.ast.IfExpression;
import kodkod.ast.IntComparisonFormula;
import kodkod.ast.IntConstant;
import kodkod.ast.MultiplicityFormula;
import kodkod.ast.NotFormula;
import kodkod.ast.QuantifiedFormula;
import kodkod.ast.Relation;
import kodkod.ast.RelationPredicate;
import kodkod.ast.UnaryExpression;
import kodkod.ast.Cardinality;
import kodkod.ast.Variable;

/** 
 * A visitor that visits every node in the AST and provides no return values.
 *
 * @author Emina Torlak  
 */
public interface VoidVisitor {

	/** 
	 * Visits the given sequence of declarations. 
	 **/
    public void visit(Decls decls);
    /** 
	 * Visits the given declaration.
	 **/
    public void visit(Decl decl);
    
    /** 
	 * Visits the given relation. 
	 **/
    public void visit(Relation relation);
    /** 
	 * Visits the given variable. 
	 **/
    public void visit(Variable variable);
    /** 
	 * Visits the given constant expression. 
	 **/
    public void visit(ConstantExpression constExpr);
    
    /** 
	 * Visits the given binary expression. 
	 **/
    public void visit(BinaryExpression binExpr);
    /** 
	 * Visits the given unary expression. 
	 **/
    public void visit(UnaryExpression unaryExpr);   
    /** 
	 * Visits the given comprehension. 
	 **/
    public void visit(Comprehension comprehension);
    /** 
	 * Visits the given if-then expression.
	 **/
    public void visit(IfExpression ifExpr);
    
    /**
     * Visits the given integer constant.
     */
    public void visit(IntConstant intConst);
    /**
     * Visits the given unary integer expression.
     */
    public void visit(Cardinality intExpr);
    /**
     * Visits the given integer comparison formula.
     */
    public void visit(IntComparisonFormula intComp);
    
    /** 
	 * Visits the given quantified formula. 
	 **/
    public void visit(QuantifiedFormula quantFormula);
    /** 
	 * Visits the given binary formula.
	 **/
    public void visit(BinaryFormula binFormula);
    /** 
	 * Visits the given negation. 
	 **/
    public void visit(NotFormula not);
    /** 
	 * Visits the given constant formula. 
	 **/
    public void visit(ConstantFormula constant);
    
    /** 
	 * Visits the given comparison formula.
	 **/
    public void visit(ComparisonFormula compFormula);
    /** 
	 * Visits the given multiplicity formula.
	 **/
    public void visit(MultiplicityFormula multFormula);
    /**
     * Visits the given relation predicate.
     */
    public void visit(RelationPredicate predicate);
    
}
