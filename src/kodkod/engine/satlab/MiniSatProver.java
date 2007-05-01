/* 
 * Kodkod -- Copyright (c) 2005-2007, Emina Torlak
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
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
//			if (!proof.conflict().literals().isEmpty()) {
//				throw new IllegalStateException();
//			}
		}
		return proof;
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.satlab.SATProver#reduce(kodkod.engine.satlab.ReductionStrategy)
	 */
	public void reduce(ReductionStrategy strategy) {
		proof();
		for(Set<Clause> next = strategy.next(proof); !next.isEmpty(); next = strategy.next(proof)) {
			List<Clause> core = new ArrayList<Clause>(next.size());
			long prover = make();
			addVariables(prover, numberOfVariables());
			for(Clause c : next) {
				if (addClause(prover, c.literals().toArray())) {
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
	 * {@inheritDoc}
	 * @see kodkod.engine.satlab.NativeSolver#free(long)
	 */
	native void free(long peer);
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.satlab.NativeSolver#addVariables(long, int)
	 */
	native void addVariables(long peer, int numVariables);

	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.satlab.NativeSolver#addClause(long, int[])
	 */
	native boolean addClause(long peer, int[] lits);
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.satlab.NativeSolver#solve(long)
	 */
	native boolean solve(long peer);
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.satlab.NativeSolver#valueOf(long, int)
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
