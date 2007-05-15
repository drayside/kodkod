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
 * A relational expression.  Unless otherwise noted,
 * all methods in this class throw a NullPointerException when given
 * null arguments.
 * 
 * @specfield arity: int
 * @invariant arity > 0
 *
 * @author Emina Torlak 
 */
public abstract class Expression implements Node {
	
	/** The universal relation:  contains all atoms in a {@link kodkod.instance.Universe universe of discourse}. */
	public static final Expression UNIV = new ConstantExpression("univ", 1);
	
	/** The identity relation: maps all atoms in a {@link kodkod.instance.Universe universe of discourse} to themselves. */
	public static final Expression IDEN = new ConstantExpression("iden", 2);
	
	/** The empty relation: contains no atoms. */
	public static final Expression NONE = new ConstantExpression("none", 1);
	
	/** The integer relation: contains all atoms {@link kodkod.instance.Bounds bound} to integers */
	public static final Expression INTS = new ConstantExpression("ints", 1);
	
    /**
     * Constructs a leaf expression
     * @effects no this.children'
     */
    Expression() { }

    /**
     * Returns the join of this and the specified expression.  The effect
     * of this method is the same as calling this.compose(BinaryExpression.Operator.JOIN, expr).
     * @return {e : Expression | e = this.expr}
     */
    public final Expression join(Expression expr) {
        return compose(BinaryExpression.Operator.JOIN,expr);
    }
    
    /**
     * Returns the product of this and the specified expression.  The effect
     * of this method is the same as calling this.compose(BinaryExpression.Operator.PRODUCT, expr).
     * @return {e : Expression | e = this->expr}
     */
    public final Expression product(Expression expr) {
        return compose(BinaryExpression.Operator.PRODUCT,expr);
    }
    
    /**
     * Returns the union of this and the specified expression.  The effect
     * of this method is the same as calling this.compose(BinaryExpression.Operator.UNION, expr).
     * @return {e : Expression | e = this + expr}
     */
    public final Expression union(Expression expr) {
    	return compose(BinaryExpression.Operator.UNION,expr);
    }
    
    /**
     * Returns the difference of this and the specified expression.  The effect
     * of this method is the same as calling this.compose(BinaryExpression.Operator.DIFFERENCE, expr).
     * @return {e : Expression | e = this - expr}
     */
    public final Expression difference(Expression expr) {
    	return compose(BinaryExpression.Operator.DIFFERENCE,expr);
    }
    
    /**
     * Returns the intersection of this and the specified expression.  The effect
     * of this method is the same as calling this.compose(BinaryExpression.Operator.INTERSECTION, expr).
     * @return {e : Expression | e = this & expr}
     */
    public final Expression intersection(Expression expr) {
    	return compose(BinaryExpression.Operator.INTERSECTION,expr);
    }
    
    /**
     * Returns the relational override of this with the specified expression.  The effect
     * of this method is the same as calling this.compose(BinaryExpression.Operator.OVERRIDE, expr).
     * @return {e : Expression | e = this ++ expr}
     */
    public final Expression override(Expression expr) {
    	return compose(BinaryExpression.Operator.OVERRIDE,expr);
    }
    
    /**
     * Returns the composition of this and the specified expression, using the
     * given binary operator.
     * @return {e: Expression | e = this op expr }
     */
    public final Expression compose(BinaryExpression.Operator op, Expression expr) {
    	return new BinaryExpression(this, op, expr);
    }
    
    /**
     * Returns the transpose of this.  The effect of this method is the same
     * as calling this.apply(UnaryExpression.Operator.TRANSPOSE).
     * @return {e : Expression | e = ~this}
     */
    public final Expression transpose() {
        return apply(UnaryExpression.Operator.TRANSPOSE);
    }
    
    /**
     * Returns the transitive closure of this.  The effect of this  method is the same
     * as calling this.apply(UnaryExpression.Operator.CLOSURE).
     * @return {e : Expression | e = ^this}
     * @throws IllegalArgumentException - this.arity != 2
     */
    public final Expression closure() {
        return apply(UnaryExpression.Operator.CLOSURE);
    }
    
    /**
     * Returns the reflexive transitive closure of this.  The effect of this 
     * method is the same
     * as calling this.apply(UnaryExpression.Operator.REFLEXIVE_CLOSURE).
     * @return {e : Expression | e = *this}
     * @throws IllegalArgumentException - this.arity != 2
     */
    public final Expression reflexiveClosure() {
    	return apply(UnaryExpression.Operator.REFLEXIVE_CLOSURE);
    }
    
    /**
     * Returns the expression that results from applying the given unary operator
     * to this.  
     * @return {e: Expression | e = op this }
     * @throws IllegalArgumentException - this.arity != 2
     */
    public final Expression apply(UnaryExpression.Operator op) {
    	return new UnaryExpression(op, this);
    }
    
    /**
     * Returns the projection of this expression onto the specified columns.
     * @return {e: Expression | e = project(this, columns) }
     * @throws IllegalArgumentException - columns.length < 1
     */
    public final Expression project(IntExpression... columns) {
    	return new ProjectExpression(this, columns);
    }
    
    /**
     * Returns the cardinality of this expression.  The effect of this method is the
     * same as calling this.apply(ExprToIntCast.Operator.CARDINALITY).  
     * @return {e: IntExpression | e = #this }
     */
    public final IntExpression count() {
    	return apply(ExprToIntCast.Operator.CARDINALITY);
    }
    
    /**
     * Returns the sum of the integer atoms in this expression.  The effect of this method is the
     * same as calling this.apply(ExprToIntCast.Operator.SUM).  
     * @return {e: IntExpression | e = sum(this) }
     */
    public final IntExpression sum() {
    	return apply(ExprToIntCast.Operator.SUM);
    }
    
    /**
     * Returns the cast of this expression to an integer expression,
     * that represents either the cardinality of this expression (if op is CARDINALITY)
     * or the sum of the integer atoms it contains (if op is SUM).
     * @return {e: IntExpression | e.op = op && e.expression = this} 
     */
    public final IntExpression apply(ExprToIntCast.Operator op) { 
    	return new ExprToIntCast(this, op);
    }
    
    /**
     * Returns the formula 'this = expr'. The effect of this method is the same 
     * as calling this.compose(ComparisonFormula.Operator.EQUALS, expr).
     * @return {f : Formula | f <=> this = expr}
     */
    public final Formula eq(Expression expr) {
    	return compose(ComparisonFormula.Operator.EQUALS, expr);
    }
    
    /**
     * Returns the formula 'this in expr'.  The effect of this method is the same 
     * as calling this.compose(ComparisonFormula.Operator.SUBSET, expr).
     * @return {f : Formula | f <=> this in expr}
     */
    public final Formula in(Expression expr) {
    	return compose(ComparisonFormula.Operator.SUBSET, expr);
    }
    
    /**
     * Returns the formula that represents the composition of this and the
     * given expression with the given comparison operator.
     * @return {f: Formula | f <=> this op expr }
     */
    public final Formula compose(ComparisonFormula.Operator op, Expression expr) {
    	return new ComparisonFormula(this, op, expr);
    }
    
    /**
     * Returns the formula 'some this'.  The effect of this method is the same as calling
     * this.apply(MultiplicityFormula.Multiplicity.SOME).
     * @return {f : Formula | f <=> some this}
     */
    public final Formula some() {
        return apply(Multiplicity.SOME);
    }
    
    /**
     * Returns the formula 'no this'.  The effect of this method is the same as calling
     * this.apply(MultiplicityFormula.Multiplicity.NO).
     * @return {f : Formula | f <=> no this}
     */
    public final Formula no() {
        return apply(Multiplicity.NO);
    }
    
    /**
     * Returns the formula 'one this'.  The effect of this method is the same as calling
     * this.apply(MultiplicityFormula.Multiplicity.ONE).
     * @return {f : Formula | f <=> one this}
     */
    public final Formula one() {
        return apply(Multiplicity.ONE);
    }
    
    /**
     * Returns the formula 'lone this'.  The effect of this method is the same as calling
     * this.apply(MultiplicityFormula.Multiplicity.LONE).
     * @return {f : Formula | f <=> lone this}
     */
    public final Formula lone() {
        return apply(Multiplicity.LONE);
    }
    
    /**
     * Returns the formula that results from applying the specified multiplicity to
     * this expression.  The SET multiplicity is not allowed.
     * @return {f: Formula | f <=> mult this}
     * @throws IllegalArgumentException - mult = Multiplicity.SET
     */
    public final Formula apply(Multiplicity mult) {
    	return new MultiplicityFormula(mult, this);
    }
    
    /**
     * Returns the arity of this expression.
     * @return this.arity
     */
    public abstract int arity();
        
    /**
     * Accepts the given visitor and returns the result.
     * @see kodkod.ast.Node#accept(kodkod.ast.visitor.ReturnVisitor)
     */
    public abstract <E, F, D, I> E accept(ReturnVisitor<E, F, D, I> visitor);
 }
