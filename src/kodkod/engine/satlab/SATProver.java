package kodkod.engine.satlab;

import java.util.Set;

import kodkod.util.ints.IntSet;

/**
 * Provides an interface to a SAT solver that can generate
 * proofs of unsatisfiability.
 * 
 * @specfield variables: set [1..)
 * @specfield clauses: set IntSet
 * @invariant all i: [2..) | i in variables => i-1 in variables
 * @invariant all c: clauses | all lit: c.ints | lit in variables || -lit in variables
 * @author Emina Torlak
 */
public interface SATProver extends SATSolver {

	/**
	 * Returns a Clause that proves the unsatisfiability of this.clauses.  
	 * In particular, the returned clause is an empty Clause (i.e. a Clause
	 * with no literals) {@link Clause#learnt() learned} from this.clauses.
	 * @requires {@link SATSolver#solve()} has been called, and it returned false
	 * @return  {p: Clause | p.prover = this and p.literals.isEmpty() and 
	 *   all core: {c: p.*antecedents | no c.antecedents } | c.literals in this.clauses }  
	 * @throws IllegalStateException - {@link SATSolver#solve()} has not been called, 
	 * or the last call to {@link SATSolver#solve()} returned true
	 */
	public Clause proof();
		
	/**
	 * A clause in a proof of unsatisfiability. 
	 * @specfield prover: SATProver \\ sat prover containing this clause 
	 * @specfield literals: set int \\ clause literals 
	 * @specfield antecedents: set Clause \\ the clauses from which this clause was derived
	 * @invariant some antecedents => 
	 *  this.literals = { lit: antecedents.literals | -lit !in antecedents.literals } 
	 * @invariant prover = antecedents.prover
	 */
	public static interface Clause {
		
		/**
		 * Returns a non-negative integer indicating when this
		 * clause was first seen by this.prover.  In particular,
		 * the first clause that was added to this.prover
		 * via {@link SATSolver#addClause(int[])} will have
		 * index 0, the next one will have index 1, the first
		 * learned clause will have index {@link SATSolver#numberOfClauses()}, etc.
		 * @return the index of this clause
		 */
		public int index();
			
		/**
		 * Returns an unmodifiable IntSet view of this.literals.
		 * @return an unmodifiable IntSet view of this.literals
		 */
		public IntSet literals();
		
		/**
		 * Returns an unmodifiable set view of this.antecedents. 
		 * @return an unmodifiable set view of this.antecedents.
		 */
		public Set<Clause> antecedents();
		
		/**
		 * Returns true if this clause was learned during solving.
		 * @return some this.antecedents
		 */
		public boolean learned();
	}
}
