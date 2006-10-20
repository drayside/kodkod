package kodkod.engine.fol2sat;

import kodkod.ast.Formula;
import kodkod.engine.bool.BooleanConstant;
import kodkod.instance.Bounds;

/**
 * Thrown when a reduction is found to be trivially (un)satisfiable 
 * with respect to given Bounds.
 * 
 * @specfield formula: Formula
 * @specfield reduction: reduction.*children
 * @specfield bounds: Bounds
 * @specfield formulaValue: BooleanConstant // the value to which the reduction simplified
 * @invariant reduction is a subtree of reduction that has caused it to simplify to a constant
 * @author Emina Torlak
 */
public final class TrivialFormulaException extends Exception {
	private final BooleanConstant formulaValue;
	private final Formula reduction;
	private final Bounds bounds;
	
	private static final long serialVersionUID = 6251577831781586067L;

	/**
	 * Constructs a new TrivialFormulaException caused by the specified reduction.
	 * That is, the reduction has caused its parent reduction
	 * to simplify to the given value when translated using the given Bounds.  
	 * @requires reduction != null && bounds != null && formulaValue != null
	 * @effects this.reduction' = reduction && this.bounds' = bounds && this.formulaValue' = formulaValue 
	 */
	 TrivialFormulaException(Formula reduction, BooleanConstant formulaValue, Bounds bounds) {
		super("Trivially " + ((formulaValue==BooleanConstant.FALSE) ? "un" : "" )  + "satisfiable formula.");
		assert formulaValue != null && bounds != null && reduction != null;
		this.reduction = reduction;
		this.bounds = bounds;
		this.formulaValue = formulaValue;
	}

	/**
	 * Returns this.reduction.
	 * @return this.reduction
	 */
	public Formula reduction() {
		return reduction;
	}
	
	/**
	 * Return this.bounds.
	 * @return this.bounds
	 */
	public Bounds bounds() {
		return bounds;
	}
	
	/**
	 * Returns the value to which this.formula is trivially reducible.
	 * @return this.formulaValue
	 */
	public BooleanConstant formulaValue() {
		return formulaValue;
	}

}
