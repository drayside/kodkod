/**
 * 
 */
package kodkod.ast;


import kodkod.ast.visitor.ReturnVisitor;
import kodkod.ast.visitor.VoidVisitor;

/**
 * Represents an integer literal.
 * @specfield value: int
 * @author Emina Torlak
 */
public final class IntConstant extends IntExpression {
	private final int value;
	
	/**
	 * Constructs an int constant.
	 * @effects this.value' = value 
	 */
	private IntConstant(int value) {
		this.value = value;
	}

	/**
	 * Returns an IntConstant corresponding to the given value.
	 * @return {c: IntConstant | c.value = value}
	 */
	public static IntConstant constant(int value) {
		return new IntConstant(value);
	}
	
	/**
	 * Returns this.value.
	 * @return this.value
	 */
	public int value() {
		return value;
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.ast.IntExpression#compose(kodkod.ast.BinaryIntExpression.Operator, kodkod.ast.IntExpression)
	 */
	public IntExpression compose(BinaryIntExpression.Operator op, IntExpression intexpr) {
		if (intexpr instanceof IntConstant)
			return constant(op.apply(value, ((IntConstant)intexpr).value));
		else 
			return super.compose(op, intexpr);
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.ast.IntExpression#compare(kodkod.ast.IntComparisonFormula.Operator, kodkod.ast.IntExpression)
	 */
	public Formula compare(IntComparisonFormula.Operator op, IntExpression intexpr) {
		if (intexpr instanceof IntConstant) 
			return op.apply(value, ((IntConstant)intexpr).value) ? Formula.TRUE : Formula.FALSE;
		else
			return super.compare(op, intexpr);	
	}
	
	/**
	 * Return true if o is an IntConstant with the same value as this.
	 * @return o in IntConstant && o.value = this.value
	 */
	public boolean equals(Object o) {
		if (o==this) 
			return true;
		else if (o instanceof IntConstant) 
			return value==((IntConstant) o).value;
		else 
			return false;
	}
	
	/**
	 * {@inheritDoc}
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return String.valueOf(value);
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.ast.IntExpression#accept(kodkod.ast.visitor.ReturnVisitor)
	 */
	@Override
	public <E, F, D, I> I accept(ReturnVisitor<E, F, D, I> visitor) {
		return visitor.visit(this);
	}
	

	/**
	 * {@inheritDoc}
	 * @see kodkod.ast.IntExpression#accept(kodkod.ast.visitor.VoidVisitor)
	 */
	@Override
	public void accept(VoidVisitor visitor) {
		visitor.visit(this);
	}

}
