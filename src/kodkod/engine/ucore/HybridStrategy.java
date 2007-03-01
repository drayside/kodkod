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
import kodkod.util.collections.Containers;
import kodkod.util.ints.IntBitSet;
import kodkod.util.ints.IntIterator;
import kodkod.util.ints.IntSet;
import kodkod.util.ints.IntTreeSet;

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
	private final Comparator<Clause> cmp;
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
		cmp = new Comparator<Clause>(){
			public int compare(Clause arg0, Clause arg1) {
				return absMax(arg0) - absMax(arg1);
		}};
	}
	
	
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.satlab.ReductionStrategy#next(kodkod.engine.satlab.ResolutionTrace)
	 */
	@SuppressWarnings("unchecked")
	public Set<Clause> next(ResolutionTrace trace) {
		if (topLevel.isEmpty()) return Collections.EMPTY_SET; // tried everything
		
		for(Clause clause : trace.core()) {
			int absMax = absMax(clause);
			if (topLevel.remove(absMax)) {
				Set<Clause> exclude = excluded(trace, absMax);
				if (!exclude.isEmpty())
					return new UnreachableClauses(trace, exclude);
			}
		}
		
		topLevel.clear();
		return Collections.EMPTY_SET;
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
	 * Given an array of clauses sorted according to their absMax literals, finds 
	 * the largest index i such that absMax(clauses[i]) = absMaxLit.  Returns
	 * a negative integer if no such index is found.
	 * @requires all i,j: [0..clauses.length) | i < j =>  absMax(clauses[i]) <=  absMax(clauses[j])
	 * @return some i: [0..clauses.length) |  absMax(clauses[i]) = absMaxLit =>
	 *  {max: [0..clauses.length) | absMax(clauses[max]) = absMaxLit && 
	 *   no j: (max..clauses.length) | absMax(clauses[j]) = absMaxLit},
	 *  {neg: int | neg < 0 }  
	 */
	private static int maxIndex(Clause[] clauses, int absMaxLit) {
		int low = 0;
		int high = clauses.length-1;

		while (low <= high) {
			int mid = (low + high) >>> 1;
			int midLit = absMax(clauses[mid]);		
			if (midLit < absMaxLit)
				low = mid + 1;
			else if (midLit > absMaxLit)
				high = mid - 1;
			else { // some index found, now get the highest one
				for(++mid; mid < clauses.length && absMax(clauses[mid])==absMaxLit; mid++);
				return mid-1;
			}
		}

		return -(low + 1);  // absMaxLit not found.
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
	 * Returns true if the positive set contains the absolute value
	 * of any of the literals from the mixed set.
	 * @requires all i: positive.ints | i > 0
	 * @return some i: mixed.ints | positive.contains(abs(i))
	 */
	private static boolean containsAbs(IntSet positive, IntSet mixed) {
		if (!positive.isEmpty()) {
			for(IntIterator itr =  mixed.iterator(); itr.hasNext(); ) {
				if (positive.contains(Math.abs(itr.nextInt())))
					return true;
			}	
		}
		return false;
	}
	
	/**
	 * Returns the clauses that should be excluded from trace.core based on the
	 * given exclusion index.  In particular, let 
	 * R = { c1, c2: trace.core | absMax(c2) in abs(c1.literals) } and
	 * E = {c: trace.core | absMax(c) = exclude}.  Assuming that all c: *R.E | lone R.c, the excluded set is
	 * { c: E.*R | *R.c - E.*R = *R.E }.  Otherwise, the excluded set is empty.
	 * @requires exclude > 0 && some c: trace.core | absMax(c) = exclude
	 * @return clauses that should be excluded from trace.core based on the given exclusion index.
	 */
	@SuppressWarnings("unchecked")
	private Set<Clause> excluded(ResolutionTrace trace, int exclude) {
		final Clause[] core = trace.core().toArray(new Clause[trace.core().size()]);
		Arrays.sort(core, cmp);
		
		System.out.println("trying "+exclude);
		
		
		final int maxExcludeIndex = maxIndex(core, exclude);
		assert maxExcludeIndex >= 0;
				
		final IntSet lits = new IntBitSet(absMax(core[core.length-1])+1);
		
		// make sure that all c: *R.E | lone R.c
		for(int i = maxExcludeIndex+1, last = exclude; i < core.length; i++) {
			IntSet s = core[i].literals();
			if (containsAbs(lits, s)) 
				return Collections.EMPTY_SET; // some c: *R.E | not lone R.c
			if (s.contains(last) || s.contains(-last)) {
				lits.add(last);
				last = absMax(core[i]);
			}
		}
		
		System.out.println("not shared "+exclude);
		
		// get E.*R
		final IntSet excluded = new IntBitSet(core.length);
		lits.clear();
		lits.add(exclude);
		for(int i = maxExcludeIndex; i >= 0; i--) {
			if (lits.contains(absMax(core[i]))) {
				excluded.add(i);
				addAbsolute(lits, core[i]);
			}
		}
		
//		for(IntIterator itr = excluded.iterator(); itr.hasNext();) {
//			System.out.println(core[itr.nextInt()]);
//		}
//		System.out.println("------------------------------------");
//		for(Clause c : core) {
//			System.out.println(c);
//		}
//		System.exit(0);
		
		// get { c: E.*R | *R.c - E.*R = *R.E }
		lits.clear();
		for(int i = core.length-1; i>=0; i--) {
			int absMax = absMax(core[i]);
			if (absMax>exclude || absMax<exclude && lits.contains(absMax) && excluded.remove(i)) {
				addAbsolute(lits, core[i]);
			}
		}
		
		final Clause[] clauses = new Clause[excluded.size()];
		final IntIterator itr = excluded.iterator();
		for(int i = 0; itr.hasNext(); i++ ) {
			clauses[i] = core[itr.nextInt()];
		}
		
		return Containers.asIdentitySet(Containers.identitySort(clauses));
	}
	
}
