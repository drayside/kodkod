/*
 * BooleanValueAllocator.java
 * Created on Aug 31, 2005
 */
package kodkod.engine.fol2sat;

import kodkod.ast.Relation;
import kodkod.engine.bool.BooleanFactory;
import kodkod.engine.bool.BooleanMatrix;
import kodkod.instance.Universe;
import kodkod.util.ints.IntSet;

/** 
 * <p>Interprets the constraints on relations and integers, as given by 
 * a specific {@link kodkod.instance.Bounds partial} or {@link kodkod.instance.Instance full}
 * instance. A relation is interpreted as a matrix of {@link kodkod.engine.bool.BooleanValue boolean values}. 
 * All boolean values
 * are produced using the same {@link kodkod.engine.bool.BooleanFactory circuit factory}.  An integer is 
 * interpreted as a vector with a single TRUE entry, which corresponds to the atom that represents the integer.
 * </p>
 * 
 * @specfield constraints: Bounds + Instance
 * @specfield factory: BooleanFactory
 * @author Emina Torlak 
 */
abstract class BooleanValueAllocator {
	
	
	BooleanValueAllocator() {}

	/**
	 * Returns the factory used to generate formulas by this allocator.
	 * @return this.fatory
	 */
	abstract  BooleanFactory factory();

	/**
	 * Returns the universe of discourse.
	 * @return constraints.universe()
	 */
	abstract Universe universe();
	
	/**
	 * Returns a {@link kodkod.engine.bool.BooleanMatrix matrix} m of 
	 * {@link kodkod.engine.bool.BooleanValue boolean formulas} representing
	 * the specified relation.    
	 * @requires constraints.contains(r)
	 * @return a {@link kodkod.engine.bool.BooleanMatrix matrix} m of 
	 * {@link kodkod.engine.bool.BooleanValue boolean formulas} representing
	 * the specified relation.   
	 */
	abstract BooleanMatrix interpret(final Relation r);  
	
	/**
	 * Returns the set of all integers corresponding to some
	 * atom in this.constraints.universe.
	 * @return this.constraints.ints()
	 */
	abstract IntSet ints();
	
	/**
	 * Returns a matrix with one TRUE entry, corresponding to the atom that represents to the given integer.
	 * @requires constraints.contains(i)
	 * @return let atom = (if this.constraints in Instance the this.constraints.tuples[i].tuples[0] else this.constraints.intBound[i].tuples[0]) | 
	 *  { m: BooleanMatrix | m.elements[int] = [0..this.constraints.universe.size()) ->one FALSE ++ 
	 *       this.constraints.universe.atoms.atom -> one TRUE }                 
	 */
	abstract BooleanMatrix interpret(int i);
	
}