/*
 * Formula.java
 * Created on May 11, 2005
 */
package kodkod.ast;

import kodkod.ast.visitor.ReturnVisitor;



/**
 * A type representing a formula.  Unless otherwise noted,
 * all methods in this class throw a NullPointerException when given
 * null arguments.
 *
 * @author Emina Torlak 
 */
public abstract class Formula implements Node {
	
	/** constant formula true */
	public static final Formula TRUE = ConstantFormula.TRUE;
	
	/** constant formula false */
	public static final Formula FALSE = ConstantFormula.FALSE;
	
    Formula() {}

    /**
     * Returns the conjunction of this and the specified formula.  The effect
     * of this method is the same as calling this.compose(BinaryFormula.Operator.AND, formula).
     * @return {f : Formula | f <=> (this && formula)}
     */
    public final Formula and(Formula formula) {
        return compose(BinaryFormula.Operator.AND,formula);
    }
    
    /**
     * Returns the conjunction of this and the specified formula.  The effect
     * of this method is the same as calling this.compose(BinaryFormula.Operator.OR, formula).
     * @return {f : Formula | f <=> (this || formula)}
     */
    public final Formula or(Formula formula) {
        return compose(BinaryFormula.Operator.OR,formula);
    }
    
    /**
     * Returns a formula that equates this and the specified formula.  The effect
     * of this method is the same as calling this.compose(BinaryFormula.Operator.IFF, formula).
     * @return {f : Formula | f <=> (this <=> formula)}
     */
    public final Formula iff(Formula formula) {
        return compose(BinaryFormula.Operator.IFF,formula);
    }
    
    /**
     * Returns the implication of the specified formula by this.  The effect
     * of this method is the same as calling this.compose(BinaryFormula.Operator.IMPLIES, formula).
     * @return {f : Formula | f <=> (this => formula)}
     */
    public final Formula implies(Formula formula) {
        return compose(BinaryFormula.Operator.IMPLIES,formula);
    }
    
    /**
     * Returns the composition of this and the specified formula using the
     * given binary operator.
     * @return {f: Formula | f <=> (this op formula) }
     */
    public Formula compose(BinaryFormula.Operator op, Formula formula) {
    	if (formula == ConstantFormula.TRUE || formula == ConstantFormula.FALSE) {
    		if (op == BinaryFormula.Operator.IMPLIES) {
    			return formula==ConstantFormula.TRUE ? formula : this.not();
    		} else {
    			return formula.compose(op, this);
    		}
    	}
    	return new BinaryFormula(this, op, formula);
    }
    
    /**
     * Returns a formula that represents a universal quantification of this
     * formula over the given declarations.  The effect of this method is the same 
     * as calling this.quantify(QuantifiedFormula.Quantifier.ALL, decls).
     * @return {f: Formula | f <=> (all decls | this) }
     */
    public final Formula forAll(Decls decls) {
    	return quantify(QuantifiedFormula.Quantifier.ALL, decls);
    }
    
    /**
     * Returns a formula that represents an existential quantification of this
     * formula over the given declarations.  The effect of this method is the same 
     * as calling this.quantify(QuantifiedFormula.Quantifier.SOME, decls).
     * @return {f: Formula | f <=> (some decls | this) }
     */
    public final Formula forSome(Decls decls) {
    	return quantify(QuantifiedFormula.Quantifier.SOME, decls);
    }
    
    /**
     * Returns a quantification of this formula using the given quantifier over 
     * the specified declarations. 
     * @return {f: Formula | f <=> (quantifier decls | this) }
     */
    public Formula quantify(QuantifiedFormula.Quantifier quantifier, Decls decls) {
    	return new QuantifiedFormula(quantifier, decls, this);
    }
    
    /**
     * Returns the comprehension expression constructed from this formula and
     * the given declarations.
     * @return {e: Expression | e = {decls | this} }
     */
    public Expression comprehension(Decls decls) {
    	return new Comprehension(decls,this);
    }
    
    /**
     * Returns the if expression constructed from this formula and the
     * specified then and else expressions.
     * @return {e: Expression | e = if this then thenExpr else elseExpr}
     */
    public Expression thenElse(Expression thenExpr, Expression elseExpr) {
    		return new IfExpression(this, thenExpr, elseExpr);
    }
    
    /**
     * Returns the if expression constructed from this formula and the
     * specified then and else integer expressions.
     * @return {e: IntExpression | e = if this then thenExpr else elseExpr}
     */
    public IntExpression thenElse(IntExpression thenExpr, IntExpression elseExpr) {
    		return new IfIntExpression(this, thenExpr, elseExpr);
    }
    
    /**
     * Returns the negation of this formula.
     * @return {f : Formula | f <=> (!this)}
     */
    public Formula not() {
        return new NotFormula(this);
    }
    
    /**
     * Accepts the given visitor and returns the result.
     * @see kodkod.ast.Node#accept(kodkod.ast.visitor.ReturnVisitor)
     */
    public abstract <E, F, D, I> F accept(ReturnVisitor<E, F, D, I> visitor);
}
