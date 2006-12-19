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
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() { 
		return value;
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
