/**
 * 
 */
package kodkod.engine.ucore;

import java.util.Collections;
import java.util.Set;

import kodkod.engine.satlab.Clause;
import kodkod.engine.satlab.ReductionStrategy;
import kodkod.engine.satlab.ResolutionTrace;

/**
 * This class implements the Empty Clause Cone algorithm for reducing
 * the size of an unsatisfiable core.
 * @author Emina Torlak
 * @see L. Zhang and S. Malik. <i>Extracting small unsatisfiable cores from unsatisfiable
 * Boolean formula.</i>  In Proceedings of Sixth International Conference on Theory and Applications of 
 * Satisfiability Testing (SAT '03). 2003.
 */
public final class EmptyClauseConeStrategy implements ReductionStrategy {
	private int lastCore;
	/**
	 * Constructs a new instance of the empty clause cone strategy for 
	 * minimizing unsatisfiable cores.
	 */
	public EmptyClauseConeStrategy() {
		lastCore = Integer.MAX_VALUE;
	}
	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.satlab.ReductionStrategy#next(kodkod.engine.satlab.ResolutionTrace)
	 */
	@SuppressWarnings("unchecked")
	public Set<Clause> next(final ResolutionTrace trace) {
		final Set<Clause> core = trace.core();
		if (lastCore > core.size()) {
			lastCore = core.size();
			return core;
		} else {
			lastCore = Integer.MIN_VALUE;                  
			return Collections.EMPTY_SET;
		}
	}

}
