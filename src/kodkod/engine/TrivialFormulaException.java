package kodkod.engine;

import java.util.Map;

import kodkod.ast.Decl;
import kodkod.ast.Formula;
import kodkod.ast.Relation;
import kodkod.engine.bool.BooleanConstant;
import kodkod.instance.Bounds;

/**
 * Thrown when a reduction is found to be trivially (un)satisfiable 
 * with respect to given Bounds.
 * 
 * @specfield formula: Formula
 * @specfield reduction: reduction.*children
 * @specfield skolems: formula.^children & Decl -> lone Relation
 * @specfield bounds: Bounds
 * @specfield formulaValue: BooleanConstant // the value to which the reduction simplified
 * @invariant reduction is a subtree of reduction that has caused it to simplify to a constant
 * @invariant skolems holds any skolem constants generated during translation
 * @author Emina Torlak
 */
public final class TrivialFormulaException extends Exception {
	private final BooleanConstant formulaValue;
	private final Formula reduction;
	private final Bounds bounds;
	private final Map<Decl, Relation> skolems;
	
	private static final long serialVersionUID = 6251577831781586067L;

	/**
	 * Constructs a new TrivialFormulaException caused by the specified reduction.
	 * That is, the reduction has caused its parent reduction
	 * to simplify to the given value when translated using the given Bounds.  
	 * @requires reduction != null && bounds != null && formulaValue != null
	 * @effects this.reduction' = reduction && this.bounds' = bounds && this.formulaValue' = formulaValue 
	 */
	public TrivialFormulaException(Formula reduction, BooleanConstant formulaValue, Bounds bounds, Map<Decl, Relation> skolems) {
		super();
		assert formulaValue != null && bounds != null && reduction != null;
		this.reduction = reduction;
		this.bounds = bounds;
		this.formulaValue = formulaValue;
		this.skolems = skolems;
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

	/**
	 * If the options with which this.formula was translated
	 * specified skolemization, returns a map from existentially
	 * quantified declarations in this.formula to their corrensponding
	 * skolem constants.  Otherwise returns null.
	 * @return this.skolems
	 */
	public Map<Decl, Relation> skolems() {
		return skolems;
	}
}
