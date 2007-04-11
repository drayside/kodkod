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

import java.util.Iterator;

import kodkod.engine.satlab.Clause;
import kodkod.engine.satlab.ResolutionTrace;

/**
 * A basic cnf-level CRR strategy.  No heuristic is used
 * to pick the clauses to be excluded from the core.
 * @specfield traces: [0..)->ResolutionTrace
 * @specfield nexts: [0..)->Set<Clause>
 * @invariant traces.ResolutionTrace = nexts.Set<Clause>
 * @invariant all i: [1..) | some traces[i] => some traces[i-1]
 * @invariant all i: [0..#nexts) | nexts[i] in traces[i].conflict.^antecedents
 * @invariant no disj i,j: [0..#nexts) | traces[i] = traces[j] && nexts[i] = nexts[j]
 * @author Emina Torlak
 */
public final class BasicCRRStrategy extends CRRStrategy {

	/**
	 * Constructs a new instance of BasicCRRStrategy.
	 * @effects no this.traces' && no this.nexts'
	 */
	public BasicCRRStrategy() {}

	/**
	 * Returns an iterator that traverses trace.core in random order.
	 * @return an iterator that traverses trace.core in random order.
	 * @see kodkod.engine.ucore.CRRStrategy#order(kodkod.engine.satlab.ResolutionTrace)
	 */
	@Override
	protected Iterator<Clause> order(ResolutionTrace trace) {
		return trace.core().iterator();
	}
	

}
