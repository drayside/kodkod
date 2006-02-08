package kodkod.engine.bool;

/** 
 * Visits {@link kodkod.engine.bool.BooleanValue boolean values}.
 * In addition to passing themselves as the argument to the visitor,
 * the boolean values also pass along satelite information of type A.
 *
 * @author Emina Torlak 
 */
public interface BooleanVisitor<T, A> {

	/**
	 * Visits the multigate and returns the result.
	 * @return the result of visiting the given multigate
	 */
    public T visit(MultiGate multigate, A arg);
    
    /**
	 * Visits the inverter and returns the result.
	 * @return the result of visiting the given inverter
	 */
    public T visit(NotGate negation, A arg);
    
    /**
	 * Visits the variable and returns the result.
	 * @return the result of visiting the given variable
	 */
    public T visit(BooleanVariable variable, A arg);
    
    /**
	 * Visits the constant and returns the result.
	 * @return the result of visiting the given constant
	 */
    public T visit(BooleanConstant constant, A arg);

}
