/**
 * 
 */
package kodkod.engine.fol2sat;

import java.util.Map;

import kodkod.ast.Relation;
import kodkod.engine.bool.BooleanFactory;
import kodkod.engine.bool.BooleanMatrix;
import kodkod.engine.bool.Dimensions;
import kodkod.instance.Bounds;
import kodkod.instance.Universe;
import kodkod.util.ints.IntIterator;
import kodkod.util.ints.IntRange;
import kodkod.util.ints.IntSet;

/**
 * A LeafInterpreter based on {@link kodkod.instance.Bounds bounds object}.
 * 
 * @specfield boundingObj: Bounds
 * @author Emina Torlak
 */
abstract class BoundsInterpreter extends LeafInterpreter<Bounds> {
	private final BooleanFactory factory;
	private final Bounds bounds;
	
	/**
	 * Creates a BoundsInterpreter for the given bounds, using the given factory.
	 * @effects this.factory' = factory && this.boundingObj' = bounds
	 */
	private BoundsInterpreter(Bounds bounds, BooleanFactory factory) {
		this.bounds = bounds;
		this.factory = factory;
	}

	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.fol2sat.LeafInterpreter#factory()
	 */
	@Override
	final BooleanFactory factory() { 	return factory; }

	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.fol2sat.LeafInterpreter#universe()
	 */
	@Override
	final Universe universe() { return bounds.universe(); }

	/**
	 * Returns the Bounds used by this bounds interpreter.
	 * @return this.boundingObj
	 */
	@Override
	final Bounds boundingObject() { return bounds; }
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.fol2sat.LeafInterpreter#interpret(kodkod.ast.Relation)
	 */
	@Override
	abstract BooleanMatrix interpret(Relation r) ;

	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.fol2sat.LeafInterpreter#ints()
	 */
	@Override
	final IntSet ints() { return bounds.ints(); }

	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.fol2sat.LeafInterpreter#interpret(int)
	 */
	@Override
	final int interpret(int i) { return bounds.exactBound(i).indexView().min(); }

	/**
	 * A BoundsInterpreter that interprets relations exactly:  i.e. all tuples that
	 * are in the upper bound of a relation r but not in its lower bound are represented
	 * using {@link kodkod.engine.bool.BooleanVariable boolean variables}.
	 * 
	 * @specfield vars: boundingObj.relations() -> set BooleanVariables
	 * @invariant all r: boundingObj.relations() | rBounds[r].TupleSet = bounds.lowerBound[r] && rBounds[r][TupleSet] = Bounds.upperBound[r]
	 * @invariant max(vars[Relation].label) = factory.numberOfVariables()
	 * @invariant all r: relations | vars[r] in factory.components
	 * @invariant all r: relations | let n = rBounds[r][TupleSet].size() - rBounds[r].TupleSet.size() |  #vars[r] = n
	 */
	static final class Exact extends BoundsInterpreter {
		/** 
		 * maps a relation to the intrange whose minimum and maximum values represent
		 * the minimum and maximum literal of the variables allocated to represent
		 * that relation. 
		 **/
		private final Map<Relation, IntRange> vars; 
		
		/**  
		 * Constructs a new exact interpreter for the given Bounds and factory.
		 * The vars map is used to determine how to allocate variables
		 * to the relations whose upper and lower bounds are not equal.
		 * <p><b>Note:</b> the given bounds object and vars must not be modified through
		 * outside pointers once they are used to construct an Exact interpreter.  If they are,
		 * the interpreter is not guaranteed to behave as specified.</p>
		 * @requires vars.IntRange = bounds.relations
		 * @requires all r: vars.IntRange | vars[r] in factory.components
		 * @requires all r: vars.IntRange | let n = bounds.upperBound[r].size() - bounds.lowerBound[r].size() | #vars[r] = n 
		 * @requires max(vars.IntRange.max) = factory.numberOfVariables()
		 * @effects this.rBounds' = { r: bounds.relations, l: TupleSet, h: TupleSet | 
		 *                             l = bounds.lowerBound[r] && h = bounds.upperBound[r] } && 
		 *          this.vars' = { r: bounds.relations, v: BooleanVariable | vars[r].contains(v.label) }
		 */
		Exact(Bounds bounds, BooleanFactory factory, Map<Relation, IntRange> vars) {
			super(bounds, factory);
			this.vars = vars;
		}
		
		/**
		 * Returns this.vars.  The returned object must not be modified while it is in use
		 * by this BoundsInterpreter; if it is, the interpreter is not guaranteed to behave as
		 * specified.
		 * @return this.vars.
		 */
		Map<Relation, IntRange> vars() { 
			return vars;
		}
		
		/**
		 * Returns a {@link kodkod.engine.bool.BooleanMatrix matrix} m of 
		 * {@link kodkod.engine.bool.BooleanValue boolean formulas} representing
		 * the specified relation.    
		 * @requires r in this.boundingObj.relations()
		 * @return { m: BooleanMatrix | let lset = (this.rBounds[r].TupleSet).tuples.index, 
		 *           hset = (this.rBounds[r][TupleSet]).tuples.index, dset = [0..this.universe.size()^r.arity) | 
		 *           m.dimensions.dimensions = [0..r.arity) ->one this.universe.size() && 
		 *           m.elements[lset] = TRUE && m.elements[dset-hset] = FALSE &&
		 *           all disj i, j: hset-lset | m.elements[i]+m.elements[j] in this.vars[r] && 
		 *            m.elements[i].label < m.elements[j].label <=> i < j }
		 */
		@Override
		BooleanMatrix interpret(Relation r) {
			int varId = vars.containsKey(r) ? vars.get(r).min() : 0; // no vars allocated to r
			
			final IntSet lowerBound = super.bounds.lowerBound(r).indexView();
			final IntSet upperBound = super.bounds.upperBound(r).indexView();
			
			final BooleanMatrix m = super.factory.matrix(Dimensions.square(r.arity(), universe().size()), upperBound, lowerBound);
			
			if (upperBound.size() > lowerBound.size()) {
				for (IntIterator indeces = upperBound.iterator(); indeces.hasNext();) {
					int tupleIndex = indeces.nextInt();
					if (!lowerBound.contains(tupleIndex))  
						m.set(tupleIndex, super.factory.variable(varId++));
				}
			}
			return m;
		}
	}
	
	/**
	 * A BoundsInterpreter that overapproximates the values of relations:  i.e. relations are
	 * interpreted as containing all the tuples in their upper bounds.
	 */
	static final class Overapproximating extends BoundsInterpreter {

		/**  
		 * Constructs a new overapproximating interpreter for the given Bounds and factory.
		 * <p><b>Note:<b> the given bounds object may be modified while in use by an
		 * Overapproximating interpreter, without affecting the interpreter's correctness.</p>
		 * @effects this.rBounds' = { r: bounds.relations, l: TupleSet, h: TupleSet | 
		 *                             l = h = bounds.upperBound[r]} 
		 */
		Overapproximating(Bounds bounds, BooleanFactory factory) {
			super(bounds, factory);
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
			final IntSet upper = super.bounds.upperBound(r).indexView();
			return super.factory.matrix(Dimensions.square(r.arity(), universe().size()), upper, upper);
		}
		
	}
}
