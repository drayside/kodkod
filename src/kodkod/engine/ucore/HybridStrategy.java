/**
 * 
 */
package kodkod.engine.ucore;

import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.Set;

import kodkod.ast.Formula;
import kodkod.engine.fol2sat.TranslationLog;
import kodkod.engine.satlab.Clause;
import kodkod.engine.satlab.ReductionStrategy;
import kodkod.engine.satlab.ResolutionTrace;
import kodkod.util.collections.IdentityHashSet;
import kodkod.util.ints.ArrayIntVector;
import kodkod.util.ints.IntBitSet;
import kodkod.util.ints.IntIterator;
import kodkod.util.ints.IntSet;
import kodkod.util.ints.IntTreeSet;
import kodkod.util.ints.IntVector;

/**
 * A hybrid strategy for generating unsat cores that are minimal at the
 * logic rather than CNF level.  This strategy will work properly iff 
 * the logic formula is translated to CNF in such a way that the magnitude 
 * of a literal representing the truth value of a logic formula is strictly larger 
 * than the magnitudes of the literals representing
 * the truth values of the formula's descendents.  
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
	 * Returns the absolute value of the literal in the given clause that
	 * has the largest magnitude.
	 * @requires some clause.literals
	 * @return max(abs(clause.literals))
	 */
	private static int absMax(Clause clause) {
		return Math.max(Math.abs(clause.literals().min()), Math.abs(clause.literals().max()));
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
			int id = absMax(clause);
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

	/**
	 * Adds the absolute values of the literals in c to s.
	 * @effects s.ints' = s.ints + abs(c.literals)
	 */
	private static void addAbsolute(IntSet s, Clause c) {
		for(IntIterator itr = c.literals().iterator(); itr.hasNext();) {
			s.add(Math.abs(itr.nextInt()));
		}
	}
	
	/**
	 * Returns the clauses that should be excluded from trace.core based on the
	 * given exclusion index.  In particular, let 
	 * R = { c1, c2: trace.core | absMax(c2) in abs(c1.literals) } and
	 * E = {c: trace.core | absMax(c) = exclude}.  Then, the excluded set is
	 * { c: E.*R | (all c': *R.c, p, p': R.c' | absMax(p) = absMax(p')) }.
	 * @requires exclude > 0 && some c: trace.core | absMax(c) = exclude
	 * @return clauses that should be excluded from trace.core based on the given exclusion index.
	 */
	private static Set<Clause> excluded(ResolutionTrace trace, int exclude) {
		final Clause[] core = trace.core().toArray(new Clause[trace.core().size()]);
		Arrays.sort(core, new Comparator<Clause>(){
			public int compare(Clause arg0, Clause arg1) {
				return absMax(arg0) - absMax(arg1);
			}});
		
		final IntSet lits = new IntBitSet(absMax(core[core.length-1])+1);
		lits.add(exclude);
		
		final IntSet excluded = new IntBitSet(core.length);
		for(int i = core.length-1; i >= 0; i--) {
			if (lits.contains(absMax(core[i]))) {
				excluded.add(i);
				addAbsolute(lits, core[i]);
			}
		}
		
		assert !excluded.isEmpty();
		
		
		
		
		return null;
	}
	
}
