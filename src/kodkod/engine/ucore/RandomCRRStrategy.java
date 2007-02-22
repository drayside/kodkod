/**
 * 
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
public final class RandomCRRStrategy extends CRRStrategy {

	/**
	 * Constructs a new instance of BasicCRRStrategy.
	 * @effects no this.traces' && no this.nexts'
	 */
	public RandomCRRStrategy() {}

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
