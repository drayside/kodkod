/**
 * 
 */
package kodkod.engine.fol2sat;

import kodkod.ast.Relation;
import kodkod.engine.bool.BooleanFactory;
import kodkod.engine.bool.BooleanMatrix;
import kodkod.engine.bool.Dimensions;
import kodkod.instance.Instance;
import kodkod.instance.Universe;
import kodkod.util.ints.IntSet;

/**
 * A LeafInterpreter based on an {@link kodkod.instance.Instance instance object}.
 * 
 * @specfield boundingObj: Instance
 * @invariant all r: boundingObj.relations() | rBound[r] = instance.tuples[r] -> instance.tuples[r]
 * @author Emina Torlak
 */
final class InstanceInterpreter extends LeafInterpreter<Instance> {
	private final Instance instance;
	private final BooleanFactory factory;
	
	/**
	 * Constructs an InstanceInterpreter for the given instance, using the given factory.
	 * <p><b>Note: </b> the given instance object may be modified while in use by an
	 * instance interpreter, without affecting the interpreter's correctness.</p>
	 * @effects this.factory' = factory && this.boundingObj' = instance
	 */
	InstanceInterpreter(Instance instance, BooleanFactory factory) {
		this.instance = instance;
		this.factory = factory;
	}

	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.fol2sat.LeafInterpreter#factory()
	 */
	@Override
	BooleanFactory factory() {
		return factory;
	}

	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.fol2sat.LeafInterpreter#universe()
	 */
	@Override
	Universe universe() {
		return instance.universe();
	}

	/**
	 * Returns the Instance used by this instance interpreter.
	 * @return this.boundingObj
	 */
	@Override
	Instance boundingObject() {
		return instance;
	}
	/**
	 * Returns a {@link kodkod.engine.bool.BooleanMatrix matrix} m of 
	 * {@link kodkod.engine.bool.BooleanValue boolean formulas} representing
	 * the specified relation.    
	 * @requires r in this.boundingObj.relations()
	 * @return { m: BooleanMatrix | let hset = (this.rBounds[r][TupleSet]).tuples.index, dset = [0..this.universe.size()^r.arity) | 
	 *           m.dimensions.dimensions = [0..r.arity) ->one this.universe.size() && 
	 *           m.elements[hset] = TRUE && m.elements[dset-hset] = FALSE }
	 */
	@Override
	BooleanMatrix interpret(Relation r) {
		final IntSet indices = instance.tuples(r).indexView();
		return factory.matrix(Dimensions.square(universe().size(), r.arity()), indices, indices);
	}

	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.fol2sat.LeafInterpreter#ints()
	 */
	@Override
	IntSet ints() {
		return instance.ints();
	}

	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.fol2sat.LeafInterpreter#interpret(int)
	 */
	@Override
	int interpret(int i) {
		return instance.tuples(i).indexView().min();
	}

	

}
