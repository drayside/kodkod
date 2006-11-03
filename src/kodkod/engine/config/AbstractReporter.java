/**
 * 
 */
package kodkod.engine.config;


import kodkod.ast.Decl;
import kodkod.ast.Formula;
import kodkod.ast.Relation;
import kodkod.engine.bool.BooleanFormula;
import kodkod.instance.Bounds;

/**
 * A skeleton implementation of the {@link Reporter} interface.
 * The default implementation for each method has an empty body.s
 * @author Emina Torlak
 */
public abstract class AbstractReporter implements Reporter {

	/**
	 * Constructs a new abstract reporter.
	 */
	protected AbstractReporter() {}
	
	/**
	 * @see kodkod.engine.config.Reporter#breakingSymmetries()
	 */
	public void breakingSymmetries() {}

	/**
	 * @see kodkod.engine.config.Reporter#flattening(kodkod.engine.bool.BooleanFormula)
	 */
	public void flattening(BooleanFormula circuit) {}

	/**
	 * @see kodkod.engine.config.Reporter#skolemizing(kodkod.ast.Decl, kodkod.ast.Relation)
	 */
	public void skolemizing(Decl decl, Relation skolem) {}

	/**
	 * @see kodkod.engine.config.Reporter#solvingCNF(int, int, int)
	 */
	public void solvingCNF(int primaryVars, int vars, int clauses) {}

	/**
	 * @see kodkod.engine.config.Reporter#detectingSymmetries()
	 */
	public void detectingSymmetries() {}

	/**
	 * @see kodkod.engine.config.Reporter#collectingStructuralInfo()
	 */
	public void collectingStructuralInfo() {}

	/**
	 * @see kodkod.engine.config.Reporter#translatingToBoolean(kodkod.ast.Formula, kodkod.instance.Bounds)
	 */
	public void translatingToBoolean(Formula formula, Bounds bounds) {}

	/**
	 * @see kodkod.engine.config.Reporter#translatingToCNF(kodkod.engine.bool.BooleanFormula)
	 */
	public void translatingToCNF(BooleanFormula circuit) {}

}
