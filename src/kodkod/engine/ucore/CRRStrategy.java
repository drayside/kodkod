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
package kodkod.engine.ucore;

import java.util.Collections;
import java.util.Iterator;
import java.util.Set;

import kodkod.engine.satlab.Clause;
import kodkod.engine.satlab.ReductionStrategy;
import kodkod.engine.satlab.ResolutionTrace;
import kodkod.util.ints.IntBitSet;
import kodkod.util.ints.IntSet;
import kodkod.util.ints.Ints;


/**
 * A skeletal implementation of the Complete Resolution Refutation algorithm
 * for for producing locally minimal cores.
 * An unsatisfiable core is locally minimal iff removing any single clause from
 * the core will make the resulting formula satisfiable.
 * @specfield traces: [0..)->ResolutionTrace
 * @specfield nexts: [0..)->Set<Clause>
 * @invariant traces.ResolutionTrace = nexts.Set<Clause>
 * @invariant all i: [1..) | some traces[i] => some traces[i-1]
 * @invariant all i: [0..#nexts) | nexts[i] in traces[i].conflict.^antecedents
 * @invariant no disj i,j: [0..#nexts) | traces[i] = traces[j] && nexts[i] = nexts[j]
 * @author Emina Torlak
 * @see <a href="http://www.cs.tau.ac.il/~ale1/muc_sat06_short_8.pdf">N. Dershowitz, Z. Hanna, and A. Nadel.  <i>A scalable algorithm for minimal unsatisfiable core
 * extraction.</i>  In Proceedings of Ninth International Conference on Theory and Applications of 
 * Satisfiability Testing (SAT '06). 2006.</a>
 */
public abstract class CRRStrategy implements ReductionStrategy {
	private IntSet excluded;
	
	/** 
	 * Constructs a new instance of CRRStrategy. 
	 * @effects no this.traces' and no this.nexts'
	 **/
	protected CRRStrategy() {
		excluded = Ints.EMPTY_SET;
	}
	
	/**
	 * Returns the next subset of clauses in the given trace to be analyzed.  In particular,
	 * returns all clauses from the given trace that are not reachable from the first clause
	 * returned by {@link #order(ResolutionTrace) this.order(trace)} that has not yet been excluded. 
	 * @requires {@inheritDoc} 
	 * @effects {@inheritDoc}
	 * @return  last(this.nexts')
	 */
	@SuppressWarnings("unchecked")
	public final Set<Clause> next(final ResolutionTrace trace) {
		if (excluded.isEmpty()) { // the first time this method is called
			excluded = new IntBitSet(trace.maxIndex()+1);
		}
		for(Iterator<Clause> itr = order(trace); itr.hasNext(); ) {
			Clause clause = itr.next();
			if (excluded.add(clause.index())) 
				return new UnreachableClauses(trace, clause);
		}
		
		return Collections.EMPTY_SET;
	}
	
	/**
	 * Returns an iterator that imposes some total ordering on trace.core.
	 * @return an iterator that imposes some total ordering on trace.core.
	 */
	protected abstract Iterator<Clause> order(ResolutionTrace trace);
	
	
}
