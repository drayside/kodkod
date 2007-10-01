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

/**
 * A first-order formula.  Unless otherwise noted,
 * all methods in this class throw a NullPointerException when given
 * null arguments.
 * 
 * <p><b>Implementation Note</b>: The {@linkplain #compose(kodkod.ast.BinaryFormula.Operator, Formula) compose} method
 * performs constant folding, so that, for example, calling f.and(Formula.TRUE) returns f.  If it is necessary to 
 * have a new Formula returned in such cases, then f.and(<i>triviallyTrue</i>) should be called instead, 
 * where <i>triviallyTrue</i> is a trivially true formula such as Expression.NONE.no(). </p>
 * @author Emina Torlak 
 */
public abstract class Formula implements Node {
	
	/** Constant formula true */
	public static final Formula TRUE = new ConstantFormula(true) {
//		@Override
//		public Formula not() {
//			return FALSE;
//		}
//		@Override
//	    public Formula compose(BinaryFormula.Operator op, Formula formula) {
//			switch(op) {
//			case OR: return TRUE;
//			case AND: case IMPLIES: case IFF: return formula;
//			case XOR: return formula.not();
//			default: throw new IllegalArgumentException("unknown operator: " + op);
//			}
//	    }
	};
	
	/** Constant formula false */
	public static final Formula FALSE = new ConstantFormula(false) {
//		@Override
//		public Formula not() {
//			return TRUE;
//		}
//		@Override
//	    public Formula compose(BinaryFormula.Operator op, Formula formula) {
//			switch(op) {
//			case AND: return FALSE;
//			case OR: case XOR: return formula;
//			case IMPLIES: return TRUE;
//			case IFF: return formula.not();
//			default: throw new IllegalArgumentException("unknown operator: " + op);
//			}
//	    }
	};
	
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
     * Returns the exclusive OR of the specified formula by this.  The effect
     * of this method is the same as calling this.compose(BinaryFormula.Operator.XOR, formula).
     * @return {f : Formula | f <=> (this xor formula)}
     */
    public final Formula xor(Formula formula) { 
    	return compose(BinaryFormula.Operator.XOR,formula);
    }
    
    /**
     * Returns the composition of this and the specified formula using the
     * given binary operator.
     * @return {f: Formula | f <=> (this op formula) }
     */
    public final Formula compose(BinaryFormula.Operator op, Formula formula) {
//    	if (formula == ConstantFormula.TRUE || formula == ConstantFormula.FALSE) {
//    		if (op == BinaryFormula.Operator.IMPLIES) {
//    			return formula==ConstantFormula.TRUE ? formula : this.not();
//    		} else {
//    			return formula.compose(op, this);
//    		}
//    	}
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
    public final Formula quantify(QuantifiedFormula.Quantifier quantifier, Decls decls) {
    	return new QuantifiedFormula(quantifier, decls, this);
    }
    
    /**
     * Returns the comprehension expression constructed from this formula and
     * the given declarations.
     * @return {e: Expression | e = {decls | this} }
     */
    public final Expression comprehension(Decls decls) {
    	return new Comprehension(decls,this);
    }
    
    /**
     * Returns the if expression constructed from this formula and the
     * specified then and else expressions.
     * @return {e: Expression | e = if this then thenExpr else elseExpr}
     */
    public final Expression thenElse(Expression thenExpr, Expression elseExpr) {
    	return new IfExpression(this, thenExpr, elseExpr);
    }
    
    /**
     * Returns the if expression constructed from this formula and the
     * specified then and else integer expressions.
     * @return {e: IntExpression | e = if this then thenExpr else elseExpr}
     */
    public final IntExpression thenElse(IntExpression thenExpr, IntExpression elseExpr) {
    	return new IfIntExpression(this, thenExpr, elseExpr);
    }
    
    /**
     * Returns the negation of this formula.
     * @return {f : Formula | f <=> (!this)}
     */
    public final Formula not() {
        return new NotFormula(this);
    }
    
    /**
     * Accepts the given visitor and returns the result.
     * @see kodkod.ast.Node#accept(kodkod.ast.visitor.ReturnVisitor)
     */
    public abstract <E, F, D, I> F accept(ReturnVisitor<E, F, D, I> visitor);
}
