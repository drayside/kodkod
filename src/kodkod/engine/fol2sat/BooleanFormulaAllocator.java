/*
 * BooleanFormulaAllocator.java
 * Created on Aug 31, 2005
 */
package kodkod.engine.fol2sat;

import kodkod.ast.Relation;
import kodkod.engine.bool.BooleanFactory;
import kodkod.engine.bool.BooleanMatrix;
import kodkod.instance.Universe;

/** 
 * Allocates {@link kodkod.engine.bool.BooleanValue boolean formulas} to 
 * {@link kodkod.ast.Relation relations}.  A formula allocated 
 * to a relation indicates the presence/absence of a particular 
 * tuple in the relations' bounds.  All allocated formulas
 * are produced using the same {@link kodkod.engine.bool.BooleanFactory circuit factory}.
 * All calls to {@link BooleanFormulaAllocator#allocate(Relation) 
 * allocate(Relation)} allocate variables to the argument relation
 * using the same bounding information.
 * 
 * @specfield relations: set Relation
 * @specfield factory: BooleanFactory
 * @specfield formulas: relations lone->one BooleanMatrix
 * @invariant formulas[Relation].factory = factory
 *
 * @author Emina Torlak 
 */
abstract class BooleanFormulaAllocator {
	
	
	BooleanFormulaAllocator() {}
	
	/**
	 * Returns the universe from which the atoms
	 * that belong to this.formulas.BooleanMatrix
	 * are drawn.
	 * @return the universe from which the atoms
	 * that belong to this.formulas.BooleanMatrix
	 * are drawn.
	 */
	abstract Universe universe();
	
	/**
	 * Returns the factory used to generate formulas by this allocator.
	 * @return this.fatory
	 */
	public abstract  BooleanFactory factory();
	
	/**
	 * Returns a {@link kodkod.engine.bool.BooleanMatrix matrix} m of 
	 * {@link kodkod.engine.bool.BooleanValue boolean formulas} representing
	 * the specified relation.    
	 * 
	 * @return {m : BooleanMatrix | m.zero = BooleanConstant.FALSE && m.elements[int] in this.factory }
	 * @throws NullPointerException - r = null 
	 * @throws IllegalArgumentException - the bounding object used by this allocator
	 *                                    has no bounds for r
	 */
	public abstract BooleanMatrix allocate(final Relation r);  

}