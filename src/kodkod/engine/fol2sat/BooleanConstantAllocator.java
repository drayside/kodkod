/*
 * BooleanConstantAllocator.java
 * Created on Aug 31, 2005
 */
package kodkod.engine.fol2sat;

import kodkod.ast.Relation;
import kodkod.engine.Options;
import kodkod.engine.bool.BooleanFactory;
import kodkod.engine.bool.BooleanMatrix;
import kodkod.engine.bool.Dimensions;
import kodkod.instance.Bounds;
import kodkod.instance.Instance;
import kodkod.instance.TupleSet;
import kodkod.instance.Universe;
import kodkod.util.ints.IntSet;

/** 
 * Uses {@link kodkod.engine.bool.BooleanConstant boolean constants} to 
 * represent particular tuples in {@link kodkod.ast.Expression expressions}.  
 * No variables are allocated; the constants represent the upper bounds on the
 * contents of expressions.
 * 
 * @specfield universe: Universe
 * @specfield relations: set Relation
 * @specfield ints: set int
 * @specfield factory: BooleanFactory
 * @specfield formulas: (relations + ints) lone->one BooleanMatrix 
 * @invariant formulas[Relation].factory = factory
 * @author Emina Torlak 
 */
abstract class BooleanConstantAllocator extends BooleanValueAllocator {
	
	private final BooleanFactory factory;
	/**  
	 * Constructs a new BooleanConstantAllocator using the specified options.
	 * 
	 * @effects this.factory.components' = BooleanConstant
	 */
	private BooleanConstantAllocator(Options options) {
		this.factory = BooleanFactory.factory(0, options);
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.fol2sat.BooleanValueAllocator#factory()
	 */
	public final BooleanFactory factory() { return factory; } 

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
		 * Constructs a new instance based constant allocator using the given options.
		 * @effects this.instance' = instance && this.factory.components' = BooleanConstant
		 */
		Exact(Instance instance, Options options) {
			super(options);
			if (instance==null) throw new NullPointerException();
			this.instance = instance;
		}
		
		/**
		 * {@inheritDoc}
		 * @see kodkod.engine.fol2sat.BooleanValueAllocator#interpret(kodkod.ast.Relation)
		 */
		final BooleanMatrix interpret(Relation r) {
			
			final TupleSet tuples = instance.tuples(r);
			if (tuples==null) {
				throw new IllegalArgumentException(r + " is not bound.");
			}
			
			return super.factory.matrix(Dimensions.square(r.arity(), instance.universe().size()), tuples.indexView(), tuples.indexView());
		}

		/**
		 * {@inheritDoc}
		 * @see kodkod.engine.fol2sat.BooleanValueAllocator#universe()
		 */
		@Override
		Universe universe() {
			return instance.universe();
		}

		/**
		 * {@inheritDoc}
		 * @see kodkod.engine.fol2sat.BooleanValueAllocator#ints()
		 */
		@Override
		IntSet ints() {
			return instance.ints();
		}

		/**
		 * {@inheritDoc}
		 * @see kodkod.engine.fol2sat.BooleanValueAllocator#interpret(int)
		 */
		@Override
		BooleanMatrix interpret(int i) {
			final IntSet s = instance.tuples(i).indexView();
			return super.factory.matrix(Dimensions.square(1, instance.universe().size()), s, s);
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
		 * Constructs a new bounds based constant allocator using the given options.
		 * @effects this.bounds' = instance && this.factory.components' = BooleanConstant
		 */
		Overapproximating(Bounds bounds, Options options) {
			super(options);
			if (bounds==null) throw new NullPointerException();
			this.bounds = bounds;
		}

		/**
		 * {@inheritDoc}
		 * @see kodkod.engine.fol2sat.BooleanValueAllocator#interpret(kodkod.ast.Relation)
		 */
		final BooleanMatrix interpret(Relation r) {
			
			final TupleSet tuples = bounds.upperBound(r);
			if (tuples==null) {
				throw new IllegalArgumentException(r + " is not bound.");
			}
			
			return super.factory.matrix(Dimensions.square(r.arity(), bounds.universe().size()), tuples.indexView(), tuples.indexView());
		}

		@Override
		Universe universe() {
			return bounds.universe();
		}

		@Override
		IntSet ints() {
			return bounds.ints();
		}

		@Override
		BooleanMatrix interpret(int i) {
			final IntSet s = bounds.exactBound(i).indexView();
			return super.factory.matrix(Dimensions.square(1, bounds.universe().size()), s, s);
		}
	
		
	}
	
}
