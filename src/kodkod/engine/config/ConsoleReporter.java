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
 * An implementation of a reporter that prints messages
 * to standard out.
 * @author Emina Torlak
 */
public final class ConsoleReporter implements Reporter {
	
	/**
	 * Constructs a new instance of the ConsoleReporter.
	 */
	public ConsoleReporter() {}
	
	/**
	 * @see kodkod.engine.config.Reporter#breakingSymmetries()
	 */
	public void breakingSymmetries() {
		System.out.println("breaking symmetries ...");
	}

	/**
	 * @see kodkod.engine.config.Reporter#flattening(kodkod.engine.bool.BooleanFormula)
	 */
	public void flattening(BooleanFormula circuit) {
		System.out.println("flattening ...");
	}

	/**
	 * @see kodkod.engine.config.Reporter#skolemizing(kodkod.ast.Decl, kodkod.ast.Relation)
	 */
	public void skolemizing(Decl decl, Relation skolem) {
		System.out.println("skolemizing " + decl + ": skolem relation=" + skolem + ", arity=" + skolem.arity());
	}

	/**
	 * @see kodkod.engine.config.Reporter#solvingCNF(int, int, int)
	 */
	public void solvingCNF(int primaryVars, int vars, int clauses) {
		System.out.println("solving p cnf " + vars + " " + clauses);
	}

	/**
	 * @see kodkod.engine.config.Reporter#detectingSymmetries()
	 */
	public void detectingSymmetries() {
		System.out.println("detecting symmetries ...");
	}

	/**
	 * @see kodkod.engine.config.Reporter#collectingStructuralInfo()
	 */
	public void collectingStructuralInfo() {
		System.out.println("analyzing formula structure ...");
	}

	/**
	 * @see kodkod.engine.config.Reporter#translatingToBoolean(kodkod.ast.Formula, kodkod.instance.Bounds)
	 */
	public void translatingToBoolean(Formula formula, Bounds bounds) {
		System.out.println("translating to boolean ...");
	}

	/**
	 * @see kodkod.engine.config.Reporter#translatingToCNF(kodkod.engine.bool.BooleanFormula)
	 */
	public void translatingToCNF(BooleanFormula circuit) {
		System.out.println("translating to cnf ...");
	}
	
	/**
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return "ConsoleReporter";
	}

}
