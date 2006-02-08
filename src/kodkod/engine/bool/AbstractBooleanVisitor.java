package kodkod.engine.bool;

import java.util.Iterator;

/**
 * A skeletal implementation of the visitor interface.  It visits
 * each node, depth first, passes the argument value to it, and returns null. 
 * 
 * @author Emina Torlak
 */
public class AbstractBooleanVisitor<T,A> implements BooleanVisitor<T,A> {

	/**
	 * Constructs a new boolean visitor.
	 */
	protected AbstractBooleanVisitor() {}

	/**
	 * Visits the inputs of the multigate, passing the given argument
	 * to each, and returns null.
	 * @return null
	 */
	public T visit(MultiGate multigate, A arg) {
		for(Iterator<BooleanValue> i = multigate.inputs(); i.hasNext();) {
			i.next().accept(this, arg);
		}
		return null;
	}

	/**
	 * Visits the sole input of the not gate, passing the given
	 * argument to it, and returns null.
	 * @return null
	 */
	public T visit(NotGate negation, A arg) {
		negation.input().accept(this, arg);
		return null;
	}

	/**
	 * Returns null.
	 * @return null
	 */
	public T visit(BooleanVariable variable, A arg) {
		return null;
	}

	/**
	 * Returns null.
	 * @return null
	 */
	public T visit(BooleanConstant constant, A arg) {
		return null;
	}

}
