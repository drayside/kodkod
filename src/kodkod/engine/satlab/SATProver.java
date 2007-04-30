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



/**
 * Provides an interface to a SAT solver that can generate
 * proofs of unsatisfiability.
 * 
 * @specfield variables: set [1..)
 * @specfield clauses: set IntSet
 * @invariant all i: [2..) | i in variables => i-1 in variables
 * @invariant all c: clauses | all lit: c.ints | lit in variables || -lit in variables
 * @invariant all c: clauses | all disj i,j: c.ints | abs(i) != abs(j)
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
