/**
 * 
 */
package kodkod.engine.ucore;

import java.util.Collections;
import java.util.Iterator;
import java.util.Set;

import kodkod.ast.Formula;
import kodkod.engine.fol2sat.TranslationLog;
import kodkod.engine.satlab.Clause;
import kodkod.engine.satlab.ReductionStrategy;
import kodkod.engine.satlab.ResolutionTrace;
import kodkod.util.collections.IdentityHashSet;
import kodkod.util.ints.IntSet;
import kodkod.util.ints.IntTreeSet;

/**
 * A hybrid strategy for generating unsat cores that are minimal at the
 * logic rather than CNF level.
 * @author Emina Torlak
 */
public final class HybridStrategy implements ReductionStrategy {
	private final IntSet topLevel;
	
	/**
	 * Constructs a hybrid strategy that will use the given translation
	 * log to relate the cnf clauses back to the logic constraints from 
	 * which they were generated.
	 */
	public HybridStrategy(TranslationLog log) {
		topLevel = new IntTreeSet();
		for(Iterator<TranslationLog.Record> itr = log.replay(); itr.hasNext();) {
			TranslationLog.Record r = itr.next();
			if (r.env().isEmpty() && r.node() instanceof Formula) {
				topLevel.add(Math.abs(r.literal()));
			}
		}
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.satlab.ReductionStrategy#next(kodkod.engine.satlab.ResolutionTrace)
	 */
	@SuppressWarnings("unchecked")
	public Set<Clause> next(ResolutionTrace trace) {
		if (topLevel.isEmpty()) return Collections.EMPTY_SET; // tried everything
		final Set<Clause> excludedTop = new IdentityHashSet<Clause>();
		int excludedId = 0;
		for(Clause clause : trace.core()) {
			int id = Math.max(Math.abs(clause.literals().min()), Math.abs(clause.literals().max()));
			if (excludedId==0) {
				if (topLevel.remove(id)) {
					excludedId = id;
				}
			}  
			if (id==excludedId) {
				excludedTop.add(clause);
			}
		}
		if (excludedTop.isEmpty()) {
			topLevel.clear();
			return excludedTop;
		} else {
//			System.out.println(excludedTop);
			return new UnreachableClauses(trace, excludedTop);
		}
	}

}
