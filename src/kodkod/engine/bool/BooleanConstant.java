package kodkod.engine.bool;


/**
 * Represents a boolean constant, true or false.  The integer
 * literal of the true and false constants are Integer.MAX_VALUE and -Integer.MAX_VALUE, respectively. 
 * The two boolean constants, TRUE and FALSE, are status among all factories.
 * 
 * @specfield value: boolean
 * @invariant value => Integer.MAX_VALUE, -Integer.MAX_VALUE
 * @author Emina Torlak
 */
public final class BooleanConstant extends BooleanValue {
	final int literal;
	
	public static final BooleanConstant TRUE = new BooleanConstant(true);
	public static final BooleanConstant FALSE = new BooleanConstant(false);
	
	/**
	 * Constructs a BooleanConstant that represent the given boolean
	 * value.
	 * @effects value => this.literal' = Integer.MAX_VALUE, this.literal' = -Integer.MAX_VALUE
	 */
	private BooleanConstant(boolean value) {
		this.literal = (value ? Integer.MAX_VALUE : -Integer.MAX_VALUE);
	}
	
	@Override
	BooleanValue negation() {
		return this==TRUE ? FALSE : TRUE;
	}
	
	/**
	 * Returns the primitive boolean representation of this literal.
	 * @return this.literal == Integer.MAX_VALUE
	 */
	public boolean booleanValue() { return literal > 0; } 
	
	/**
	 * Returns the BooleanConstant that represents the given boolean value.
	 * @return {c: BooleanConstant | value => c.literal = Integer.MAX_VALUE, c.literal = -Integer.MAX_VALUE }
	 */
	public static BooleanConstant constant(boolean value) {
		return value ? TRUE : FALSE;
	}
	
	@Override
	public int literal() {
		return literal;
	}
	
	@Override
	public <T, A> T accept(BooleanVisitor<T,A> visitor, A arg) {
		return visitor.visit(this, arg);
	}
	
	public String toString() {
		return literal>0 ? "T" : "F";
	}

}
