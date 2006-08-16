/*
 * LeafInterpreter.java
 * Created on Aug 31, 2005
 */
package kodkod.engine.fol2sat;

import kodkod.ast.ConstantExpression;
import kodkod.ast.Expression;
import kodkod.ast.Relation;
import kodkod.engine.bool.BooleanFactory;
import kodkod.engine.bool.BooleanMatrix;
import kodkod.engine.bool.Dimensions;
import kodkod.instance.Universe;
import kodkod.util.ints.IntIterator;
import kodkod.util.ints.IntSet;
import kodkod.util.ints.Ints;

/** 
 * <p>Interprets the unquantified leaf expressions of a kodkod ast, {@link kodkod.ast.Relation relations} and
 * {@link kodkod.ast.ConstantExpression constant expressions}, as {@link kodkod.engine.bool.BooleanMatrix matrices} of {@link kodkod.engine.bool.BooleanValue
 * boolean values}.  Depending on the implementation details of a particular
 * leaf interpreter, a {@link kodkod.engine.fol2sat.FOL2BoolTranslator
 * FOL to boolean translator} that uses it can act either as a translator or as an evaluator.</p> 
 * 
 * <p>A leaf interpreter also interprets primitive integers as corresponding to particular atoms in the {@link kodkod.instance.Universe universe
 * of discourse}</p>
 * 
 * <p>All boolean values
 * are produced using the same {@link kodkod.engine.bool.BooleanFactory circuit factory}, and all relation,
 * constant,  and
 * integer interpretations are based on the same {@link kodkod.instance.Bounds bounds} or 
 * {@link kodkod.instance.Instance instance} object.
 * </p>
 * 
 * @specfield boundingObj: B
 * @specfield rBounds: boundingObj.relations() ->one (TupleSet one->one TupleSet)
 * @specfield factory: BooleanFactory
 * @invariant B in Bounds + Instance
 * @author Emina Torlak 
 */
abstract class LeafInterpreter<B> {
	
	
	LeafInterpreter() {}

	/**
	 * Returns the factory used to generate formulas by this allocator.
	 * @return this.factory
	 */
	abstract BooleanFactory factory();

	/**
	 * Returns the universe of discourse.
	 * @return this.boundingObj.universe()
	 */
	abstract Universe universe();
	
	/**
	 * Returns the bounding object used by this translation interpreter.
	 * @return this.boundingObj
	 */
	abstract B boundingObject();
	
	/**
	 * Returns a {@link kodkod.engine.bool.BooleanMatrix matrix} m of 
	 * {@link kodkod.engine.bool.BooleanValue boolean formulas} representing
	 * the specified relation.    
	 * @requires r in this.boundingObj.relations()
	 * @return { m: BooleanMatrix | let lset = (this.rBounds[r].TupleSet).tuples.index, 
	 *           hset = (this.rBounds[r][TupleSet]).tuples.index, dset = [0..this.universe.size()^r.arity) | 
	 *           m.dimensions.dimensions = [0..r.arity) ->one this.universe.size() && 
	 *           m.elements[lset] = TRUE && m.elements[dset-hset] = FALSE &&
	 *           m.elements[hset-lset] != FALSE }
	 */
	abstract BooleanMatrix interpret(Relation r);  
	
	/**
	 * Returns a {@link kodkod.engine.bool.BooleanMatrix matrix} m of 
	 * {@link kodkod.engine.bool.BooleanValue boolean formulas} representing
	 * the specified constant expression.    
	 * @return { m: BooleanMatrix | let dset = [0..this.universe.size()^c.arity) | 
	 *           m.dimensions.dimensions = [0..c.arity) ->one this.universe.size() && 
	 *           c = UNIV => m.elements[dset] = TRUE, c = NONE => m.elements[dset] = FALSE,
	 *           c = IDEN => (all i: dset | (some j: int | i = j*(1+this.universe.size())) => m.elements[i] = TRUE, m.elements[i] = FALSE),
	 *           c = INT => (all i: dset | (some j: int | this.interpret(j)=i) => m.elements[i] = TRUE, m.elements[i] = FALSE }
	 */
	final BooleanMatrix interpret(ConstantExpression c) {
		final int univSize = universe().size();
		if (c==Expression.UNIV) {
			final IntSet all =  Ints.rangeSet(Ints.range(0, univSize-1));
			return factory().matrix(Dimensions.square(univSize, 1), all, all);
		} else if (c==Expression.IDEN) {
			final Dimensions dim2 = Dimensions.square(univSize, 2);
			final IntSet iden = Ints.bestSet(dim2.capacity());
			for(int i = 0; i < univSize; i++) {
				iden.add(i*univSize + i);
			}			
			return factory().matrix(dim2, iden, iden);
		} else if (c==Expression.NONE) {
			return factory().matrix(Dimensions.square(univSize, 1), Ints.EMPTY_SET, Ints.EMPTY_SET);
		} else if (c==Expression.INTS) {
			final IntSet ints = Ints.bestSet(univSize);
			for(IntIterator iter = ints().iterator(); iter.hasNext(); ) {
				ints.add(interpret(iter.nextInt()));
			}
			return factory().matrix(Dimensions.square(univSize, 1), ints, ints);
		} else {
			throw new IllegalArgumentException("unknown constant expression: " + c);
		}
	}
	
	/**
	 * Returns the set of all integers corresponding to some
	 * atom in this.universe.
	 * @return this.boundingObj().ints()
	 */
	abstract IntSet ints();
	
	/**
	 * Returns the index of the atom from this.universe which represents the given integer.
	 * @requires i in this.ints
	 * @return this.boundingObj in Instance => this.boundingObj.tuples[i].tuples[0],
	 *         this.boundingObj.intBound[i].tuples[0]               
	 */
	abstract int interpret(int i);
	
}