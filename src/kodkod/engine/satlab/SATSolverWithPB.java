/**
 * 
 */
package kodkod.engine.satlab;

/**
 * Extends the SATSolver interface to handle Pseudo-Boolean constraints.
 * @author jbaek
 *
 */
public interface SATSolverWithPB extends SATSolver {
	
	/**
	 * Adds the specified Pseudo-Boolean constraint to this.clauses.
	 * The Pseudo-Boolean constrait is of the following form:
	 * 	cs[0]*lits[0] + cs[1]*lits[1] + ... + cs[n-1]*lits[n-1] [>|>=|==|<=|<] rhs,
	 * where the exact inequality sign is determined by the parameter ineq.
	 * (-2 for <, -1 for <=, 0 for ==, 1 for >=, 2 for >.) This representation is
	 * consistent with the representation used in MiniSAT+.
	 * @requires all i: [0..lits.length) | lits[i] != 0 && |lits[i]| <= #this.literals && -2 <= ineq && ineq <= 2 && lits.length == cs.length  	 
	 * @effects this.clauses' = this.clauses + [the aforementioned PB constraint]
	 * @effects lits' may not have the same contents as lits
	 * @throws NullPointerException - lits = null || cs = null
	 * @see kodkod.engine.satlab.SATSolver#addClause(int[])
	 */
	public abstract void addPBClause(int[] lits, int[] cs, int rhs, int ineq);
}
