/**
 * Evaluator.java
 * Created on 12:07:20 PM
 */
package kodkod.engine;

import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.engine.bool.BooleanMatrix;
import kodkod.engine.bool.BooleanValue;
import kodkod.engine.fol2sat.Fol2SatTranslator;
import kodkod.instance.Instance;
import kodkod.instance.TupleFactory;
import kodkod.instance.TupleSet;
import kodkod.util.IndexedEntry;

/**
 * An evaluator for relational formulas and expressions with
 * respect to a given {@link kodkod.instance.Instance instance}.
 * 
 * @specfield instance: Instance
 * 
 * @author Emina Torlak
 */
public final class Evaluator {
	private final Instance instance;
	
	/**
	 * Constructs a new Evaluator for the given instance.
	 * @throws NullPointerException - instance = null
	 */
	public Evaluator(Instance instance) {
		if (instance==null) throw new NullPointerException("instance=null");
		this.instance = instance;
	}
	
	/**
	 * Returns this.instance.  Any modifications to the returned object
	 * will be reflected in the behavior of the evaluate methods.
	 * 
	 * @return this.instance
	 */
	public Instance instance() { return instance; }
	
	/**
	 * Evaluates the specified formula with respect to the relation-tuple mappings 
	 * given by this.instance.
	 * 
	 * @return true if formula is true with respect to this.instance; otherwise returns false
	 */
	public boolean evaluate(Formula formula){
		return (Fol2SatTranslator.evaluate(formula, instance)).booleanValue();
	}
	
	/**
	 * Evaluates the specified expession with respect to the relation-tuple mappings 
	 * given by this.instance.
	 * 
	 * @return  {@link kodkod.instance.TupleSet set} of tuples to which the expression evaluates given the
	 * mappings in this.instance.
	 */
	public TupleSet evaluate(Expression expression){
		final BooleanMatrix sol = Fol2SatTranslator.evaluate(expression,instance);
		final TupleFactory factory = instance.universe().factory();
		final int arity = expression.arity();
		final TupleSet ret = factory.noneOf(arity);
		
		for(IndexedEntry<BooleanValue> cell : sol) {
			ret.add(factory.tuple(arity, cell.index()));
		}
		
		return ret;
	}
	
	
}
