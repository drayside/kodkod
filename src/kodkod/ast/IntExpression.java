/**
 * 
 */
package kodkod.ast;

import kodkod.ast.visitor.ReturnVisitor;
import kodkod.ast.visitor.VoidVisitor;

/**
 * <p>Represents constant expressions used for arithmetic manipulations.
 * Each constant C in {ADD, MUL, DIV} is a ternary relation, with the following semantics:
 * if the tuple (a, b, c) is in C, then a op b = c, where op is the 
 * operation represented by C.  The constant SUCC is a binary relation, where
 * the presence of the tuple (a, b) indicates that b is the successor of a. </p>
 * 
 * <p>The result of all arithmetic operations constructed from the provided constants (ADD, MUL, DIV) automatically
 * 'wraps', as it does in Java.  Since the universe of discourse is always finite, (a op b) is computed as ((a op b) % |INTS|),
 * where INTS is a unary expression whose relational value is the set of all {@link kodkod.instance.IntAtom IntAtoms} in 
 * a {@link kodkod.instance.Universe universe of discourse}. </p>
 * 
 * <p>To construct an expression representing an integer constant, create a unary relation
 * and later bound it with appropriate integer atom.</p>
 * 
 * @author Emina Torlak
 */
 final class IntExpression extends LeafExpression {
	/** The set of all {@link kodkod.instance.IntAtom IntAtoms} in a {@link kodkod.instance.Universe universe of discourse}. */
	public static final Expression INTS = new IntExpression("INTS", 1);
	/** Addition:  "a + b" = b.(a.ADD) */
	public static final Expression ADD = new IntExpression("ADD",3);
	/** Multiplication:  "a * b" =  b.(a.MUL) */
	public static final Expression MUL = new IntExpression("MUL",3);	
	/** Integer division:  "a \ b" =  b.(a.DIV)
	 *  Division by 0 returns the empty set. */
	public static final Expression DIV = new IntExpression("DIV",3);	
	/** The successor relation:  "the successor of a" = a.SUCC. 
	 * Since the set of available integers is finite, the greatest element has no successor. */
	public static final Expression SUCC = new IntExpression("SUCC",2);
	/**
	 * The successors relation: "the successors of a" = a.SUCCS.
	 * SUCCS = ^SUCC.
	 */
	public static final Expression SUCCS = SUCC.closure();
	
	/**
	 * Constructs a leaf expression with the given name and arity.
	 */
	private IntExpression(String name, int arity) {
		super(name, arity);
	}

	
	/** 
	 * Accepts the given visitor and returns the result.
	 * @see kodkod.ast.Expression#accept(kodkod.ast.visitor.ReturnVisitor)
	 */
	@Override
	public <E, F, D> E accept(ReturnVisitor<E, F, D> visitor) {
		return null;//visitor.visit(this);
	}

	/**
     * Accepts the given visitor.
	 * @see kodkod.ast.Node#accept(kodkod.ast.visitor.VoidVisitor)
	 */
	public void accept(VoidVisitor visitor) {
		//visitor.visit(this);

	}

	/**
	 * Returns the 'sum' of a and b:  b.(a.ADD)
	 * @return b.join(a.join(ADD))
	 */
	public static Expression add(Expression a, Expression b) {
		return b.join(a.join(ADD));
	}
	
	/**
	 * Returns the 'difference' of a and b: b.(a.ADD)
	 * @return b.join(ADD.join(a))
	 */
	public static Expression subtract(Expression a, Expression b) {
		return b.join(ADD.join(a));
	}
	
	/**
	 * Returns the 'product' of a and b:  b.(a.MUL)
	 * @return b.join(a.join(MUL))
	 */
	public static Expression multiply(Expression a, Expression b) {
		return b.join(a.join(MUL));
	}
	
	/**
	 * Returns the 'quotient' of a and b: b.(a.DIV)
	 * @return b.join(a.join(DIV))
	 */
	public static Expression divide(Expression a, Expression b) {
		return b.join(a.join(DIV));
	}
	
	/**
	 * Returns the formula "a < b": b in a.^SUCC
	 * @return b.in( a.join(SUCC.closure())) )
	 */
	public static Formula lt(Expression a, Expression b) {
		return b.in(a.join(SUCCS));
	}
	
	/**
	 * Returns the formula "a <= b": b in a + a.^SUCC
	 * @return b.in( a.union(a.join(SUCC.closure())) )
	 */
	public static Formula lte(Expression a, Expression b) {
		return b.in(a.union(a.join(SUCCS)));
	}
	
	/**
	 * Returns the formula "a > b": a in b.^SUCC
	 * @return a.in( b.join(SUCC.closure())) )
	 */
	public static Formula gt(Expression a, Expression b) {
		return a.in(b.join(SUCCS));
	}
	
	/**
	 * Returns the formula "a >= b": a in b + b.^SUCC
	 * @return a.in( b.union(b.join(SUCC.closure())) )
	 */
	public static Formula gte(Expression a, Expression b) {
		return a.in(b.union(b.join(SUCCS)));
	}
	
}
