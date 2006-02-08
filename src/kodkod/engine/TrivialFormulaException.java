package kodkod.engine;

import kodkod.ast.Formula;
import kodkod.engine.bool.BooleanConstant;
import kodkod.instance.Bounds;

/**
 * Thrown when a formula is found to be trivially (un)satisfiable 
 * with respect to given Bounds.
 * 
 * @specfield formula: Formula
 * @specfield bounds: Bounds
 * @specfield formulaValue: BooleanConstant // the value to which the formula simplified
 * @author Emina Torlak
 */
public final class TrivialFormulaException extends Exception {
	private final BooleanConstant formulaValue;
	private final Formula formula;
	private final Bounds bounds;
	
	private static final long serialVersionUID = 6251577831781586067L;

	/**
	 * Constructs a new TrivialFormulaException caused by the specified Formula
	 * simplifying to the given value when translated using the given Bounds.
	 * @requires formula != null && bounds != null && formulaValue != null
	 * @effects this.formula' = formula && this.bounds' = bounds && this.formulaValue' = formulaValue 
	 */
	public TrivialFormulaException(Formula formula, Bounds bounds, BooleanConstant formulaValue) {
		super();
		assert formulaValue != null && bounds != null && formula != null;
		this.formula = formula;
		this.bounds = bounds;
		this.formulaValue = formulaValue;
	}

	/**
	 * Returns this.formula.
	 * @return this.formula
	 */
	public Formula formula() {
		return formula;
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
