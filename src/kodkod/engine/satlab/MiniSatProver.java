/**
 * 
 */
package kodkod.engine.satlab;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import kodkod.util.ints.IntBitSet;

/**
 * Java wrapper for Niklas EŽn and Niklas Sšrensson MiniSAT solver 
 * with proof logging.
 * @author Emina Torlak
 */
final class MiniSatProver extends NativeSolver implements SATProver {
	private ResolutionTrace proof;
	/**
	 * Constructs a new MiniSat prover wrapper.
	 */
	MiniSatProver() {
		super(make());
		proof = null;
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.satlab.SATProver#proof()
	 */
	public ResolutionTrace proof() {	
		if (!Boolean.FALSE.equals(status())) throw new IllegalStateException();
		if (proof==null) {
			final Object[] trace = trace(peer(), true);
			free();
			proof = new ResolutionTrace(trace, new IntBitSet(trace.length-1, (long[])trace[trace.length-1]));
		}
		return proof;
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.satlab.SATProver#proof(kodkod.engine.satlab.ReductionStrategy)
	 */
	public ResolutionTrace proof(ReductionStrategy strategy) {
		proof();
		for(Set<Clause> next = strategy.next(proof); !next.isEmpty(); next = strategy.next(proof)) {
			List<Clause> core = new ArrayList<Clause>(next.size());
			long prover = make();
			addVariables(prover, numberOfVariables());
			for(Clause c : next) {
				if (addClause(prover, c.literals().toArray()) >= 0) {
					core.add(c);
				}
			}
			if (!solve(prover)) {
				
				Object[] trace = trace(prover, false);
				free(prover);
				for(int i = 0, j = 0, max = trace.length-1; i < max; i++) {
					if (trace[i]==null) { 
						trace[i] = core.get(j++);
					}
				}
				proof = new ResolutionTrace(proof, trace, new IntBitSet(trace.length-1, (long[])trace[trace.length-1]));
				
//				System.out.println("UNSAT: " + proof.core().size());
			} else {
//				System.out.println("SAT");
				free(prover);
			}
		}
		return proof;
	}
	
	/**
	 * {@inheritDoc}
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return "MiniSatProver";
	}
	
	
	static {
		loadLibrary("minisatprover");
	}
	
	/**
	 * Returns a pointer to an instance of  MiniSAT.
	 * @return a pointer to an instance of minisat.
	 */
	private static native long make();
	
	/**
	 * Releases the resources associated with
	 * the given instance of minisat.  This method
	 * must be called when the object holding the
	 * given reference goes out of scope to avoid
	 * memory leaks.
	 * @effects releases the resources associated
	 * with the given instance of minisat
	 */
	native void free(long peer);
	
	/**
	 * Adds the given number of variables
	 * to the instance of minisat referenced
	 * by the first argument.
	 * @effects increases the vocabulary of the given instance
	 * by the specified number of variables
	 */
	native void addVariables(long peer, int numVariables);
	
	/**
	 * Adds the specified clause to the instance
	 * of minisat referenced by the first argument.
	 * @requires all i: lits[int] | some l: [1..numVariables(minisat) | 
	 *            i = l || i = -l
	 * @effects adds the given clause to the specified instance of minisat.
	 * @return clause index of the given clause if it was added to the peer;
	 * a negative integer if not.  IF the returned index is positive, it is 
	 * guaranteed to be larger than any index returned so far.
	 */
	native int addClause(long peer, int[] lits);
	
	/**
	 * Calls the solve method on the instance of 
	 * minisat referenced by the given long.
	 * @return true if sat, false otherwise
	 */
	native boolean solve(long peer);
	
	/**
	 * Returns the assignment for the given literal
	 * by the specified instance of minisat:  1 means
	 * the variable is TRUE, 0 that it's FALSE, and 
	 * -1 that it is UNDECIDED.
	 * @requires the last call to {@link #solve(long) solve(minisat)} returned SATISFIABLE
	 * @return the assignment for the given literal
	 */
	native boolean valueOf(long peer, int literal);

	/**
	 * Returns an array of arrays that encodes the most recently generated
	 * resolution trace.  The resolution trace is encoded as follows. Let
	 * R be the returned array. The last element in R is an array of longs, L.  
	 * For all integers 0 <= i < trace.length-1 such that L[i>>>6] & (1L << i) == 1,
	 * R[i] contains the antecedents of the ith resolvent clause, representend
	 * by an array of integers.  In particular, for each 0 <= j < R[i].length, 
	 * R[i][j] < i and R[j] contains the encoding of the jth antecedent of the
	 * R[i].  For all integers 0 <= i < trace.length-1 such that 
	 * L[i>>>6] & (1L << i) == 0, R[i] contains the literals of the ith core clause
	 * (i.e. the clause c for which {@link #addClause(long, int[]) addClause(peer,c)} == i)
	 * if recordCore is true; otherwise R[i] is null.
	 * @return an array of arrays that encodes the most recently generated
	 * resolution trace
	 */
	native Object[] trace(long peer, boolean recordCore);
}
