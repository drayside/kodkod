/*
 * BooleanConstantAllocator.java
 * Created on Aug 31, 2005
 */
package kodkod.engine.fol2sat;

import kodkod.ast.Relation;
import kodkod.engine.bool.BooleanConstant;
import kodkod.engine.bool.BooleanFactory;
import kodkod.engine.bool.BooleanMatrix;
import kodkod.engine.bool.Dimensions;
import kodkod.instance.Bounds;
import kodkod.instance.Instance;
import kodkod.instance.TupleSet;
import kodkod.instance.Universe;
import kodkod.util.IntIterator;

/** 
 * Uses {@link kodkod.engine.bool.BooleanConstant boolean constants} to 
 * represent particular tuples in {@link kodkod.ast.Expression expressions}.  
 * No variables are allocated; the constants represent the upper bounds on the
 * contents of expressions.
 * 
 * @specfield tuples: Relation -> one TupleSet
 * @specfield factory: BooleanFactory
 * @invariant all r: tuples.TupleSet | tuples[r].arity = r.arity
 * @author Emina Torlak 
 */
abstract class BooleanConstantAllocator extends BooleanFormulaAllocator {
	
	private final BooleanFactory factory;
	/**  
	 * Constructs a new BooleanConstantAllocator.
	 * 
	 * @effects this.factory.components' = BooleanConstant
	 */
	private BooleanConstantAllocator() {
		this.factory = BooleanFactory.factory(0);
	}
	
	public final BooleanFactory factory() { return factory; } 
	
	@Override
	abstract Universe universe();
	
	/**
	 * Returns the tuples that represent the 
	 * upper bound on the given relation.  If 
	 * the relation is not mapped by this.tuples,
	 * null is returned.
	 * @return this.bounds[r]
	 */
	abstract TupleSet tuples(Relation r);
	
	/** 
	 * Returns a {@link kodkod.engine.bool.BooleanMatrix matrix m} of 
	 * {@link kodkod.engine.bool.BooleanConstant boolean constants} representing
	 * the tuples assigned to the specified relation by this.tuples.    
	 * 
	 * @return {m : BooleanMatrix | m.zero = FALSE && 
	 *           m.elements[this.tuples[r].indexView()] = TRUE }
	 * @throws NullPointerException - r = null 
	 * @throws IllegalArgumentException - no this.tuples[r]
	 * @see kodkod.engine.fol2sat.BooleanFormulaAllocator#allocate(kodkod.ast.Relation)
	 */
	public BooleanMatrix allocate(Relation r) {
		
		final TupleSet tuples = tuples(r);
		if (tuples==null) {
			throw new IllegalArgumentException(r + " is not bound.");
		}
		
		final BooleanMatrix m = factory.matrix(Dimensions.square(r.arity(), universe().size()), BooleanConstant.FALSE);
		
		for (IntIterator indeces = tuples.indexView().iterator(); indeces.hasNext(); ) {
			m.set(indeces.nextInt(), BooleanConstant.TRUE);
		}
		
		return m;
	}
	
	
	
	/**
	 * A BooleanConstantAllocator that is based on an Instance.
	 * Specifically, true entries in the matrix representing
	 * a relation r correspond to the exact tuples assigned to r by a given instance.
	 * 
	 * @specfield instance: Instance
	 * @invariant tuples = instance.tuples
	 * @author Emina Torlak
	 */
	final static class Exact extends BooleanConstantAllocator {
		private final Instance instance;
		
		/**
		 * Constructs a new instance based constant allocator.
		 * @effects this.instance' = instance && this.factory.components' = BooleanConstant
		 */
		Exact(Instance instance) {
			if (instance==null) throw new NullPointerException("instance=null");
			this.instance = instance;
		}
		
		@Override
		Universe universe() {
			return instance.universe();
		}

		@Override
		TupleSet tuples(Relation r) {
			return instance.tuples(r);
		}
	}
	
	/**
	 * A BooleanConstantAllocator that is based on a Bounds object.
	 * Specifically, true entries in the matrix representing
	 * a relation r correspond to the upper bound tuples assigned to 
	 * r by a given Bounds.
	 * 
	 * @specfield bounds: Bounds
	 * @invariant tuples = bounds.upperBound
	 * @author Emina Torlak
	 */
	final static class Overapproximating extends BooleanConstantAllocator {
		private final Bounds bounds;
		
		/**
		 * Constructs a new bounds based constant allocator.
		 * @effects this.bounds' = instance && this.factory.components' = BooleanConstant
		 */
		Overapproximating(Bounds bounds) {
			if (bounds==null) throw new NullPointerException("bounds=null");
			this.bounds = bounds;
		}
		
		@Override
		Universe universe() {
			return bounds.universe();
		}

		@Override
		TupleSet tuples(Relation r) {
			return bounds.upperBound(r);
		}
		
	}
	
}
