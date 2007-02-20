package kodkod.engine.satlab;


/**
 * Provides an interface to a SAT solver that can generate
 * proofs of unsatisfiability.
 * 
 * @specfield variables: set [1..)
 * @specfield clauses: set Clause
 * @invariant all i: [2..) | i in variables => i-1 in variables
 * @invariant all c: clauses | all lit: c.literals | lit in variables || -lit in variables
 * @author Emina Torlak
 */
public interface SATProver extends SATSolver {
	
	/**
	 * Returns a resolution-based proof of  unsatisfiability of this.clauses.
	 * @requires {@link SATSolver#solve()} has been called, and it returned false
	 * @return { p: ResolutionProof | p.core in this.clauses }
	 * @throws IllegalStateException - {@link SATSolver#solve()} has not been called, 
	 * or the last call to {@link SATSolver#solve()} returned true
	 */
	public ResolutionTrace proof();

	/**
	 * Returns a resolution-based proof of unsatisfiability of this.clauses, minimized
	 * using the given core reduction strategy. 
	 * @requires {@link SATSolver#solve()} has been called, and it returned false
	 * @return { p: ResolutionProof | p.core in this.clauses && strategy.next(p).isEmpty()}
	 * @throws IllegalStateException - {@link SATSolver#solve()} has not been called, 
	 * or the last call to {@link SATSolver#solve()} returned true
	 */
	public ResolutionTrace proof(ReductionStrategy strategy);
}
