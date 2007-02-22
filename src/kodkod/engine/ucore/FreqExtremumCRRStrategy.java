/**
 * 
 */
package kodkod.engine.ucore;

import java.util.Arrays;
import java.util.Comparator;
import java.util.Iterator;

import kodkod.engine.satlab.Clause;
import kodkod.engine.satlab.ResolutionTrace;
import kodkod.util.collections.Containers;
import kodkod.util.ints.IntBitSet;
import kodkod.util.ints.IntSet;
import kodkod.util.ints.Ints;

/**
 * A cnf-level CRR strategy that uses a frequency
 * heuristic to pick the clauses to be excluded from the core.  
 * Instances of this strategy can be configured
 * to pick the clause that is either least or most present
 * in a resolution trace.  A clause is considered to be least present
 * in a trace if it has the least number of ancestors; conversely, 
 * a clause is most present if it has the greatest number of ancestors.  
 * @specfield traces: [0..)->ResolutionTrace
 * @specfield nexts: [0..)->Set<Clause>
 * @invariant traces.ResolutionTrace = nexts.Set<Clause>
 * @invariant all i: [1..) | some traces[i] => some traces[i-1]
 * @invariant all i: [0..#nexts) | nexts[i] in traces[i].conflict.^antecedents
 * @invariant no disj i,j: [0..#nexts) | traces[i] = traces[j] && nexts[i] = nexts[j]
 * @author Emina Torlak
 */
public class FreqExtremumCRRStrategy extends CRRStrategy {
	private final boolean leastFreq;
	/**
	 * Constructs a new instance of DistExtremumCRRStrategy that
	 * picks either the least (leastFreq = true) or most (leastFreq = false)
	 * frequent clause in a given trace..
	 */
	public FreqExtremumCRRStrategy(boolean leastFreq) {
		this.leastFreq = leastFreq;
	}
	
	/**
	 * Computes the transitive closure of the clause graph rooted at the given clause.
	 * The closure information is stored in the descendents arrays as follows:
	 * at each index i, descendents[i] contains the indices of the descendents of the
	 * clause with the index i.  The method assumes thatthe array is large enough.
	 * @effects computes the transitive closure of the clause graph rooted at the given clause.
	 */
	private static void closure(Clause clause, IntSet[] descendents) {
		if (descendents[clause.index()]==null) { // not processed
			if (clause.learned()) {
				descendents[clause.index()] = new IntBitSet(descendents.length);
				for(Clause ante : clause.antecedents()) {
					closure(ante, descendents);
					descendents[clause.index()].addAll(descendents[ante.index()]);
				}
				
			} else {
				descendents[clause.index()] = Ints.singleton(clause.index());
			}
		}
	}
	
	/**
	 * Returns the core clauses, sorted in the increasing order of frequency.
	 * @requires core.length = freq.length = trace.core().size().
	 * @return an array of core clauses core such that for all i,j: [0..core.length) | i < j =>
	 *  #trace.conflict.*antecedents & core[i].*~antecedents <= #trace.conflict.*antecedents & core[j].*~antecedents
	 */
	private Clause[] coreFreq(ResolutionTrace trace) {
		final IntSet[] descendents = new IntSet[trace.maxIndex()+1];
		closure(trace.conflict(), descendents);
		final Clause[] core = new Clause[trace.core().size()];
		final int[] freq = new int[trace.maxIndex()+1];
		int i = 0;
		for(Clause c : trace.core()) {
			core[i++] = c;
			for(IntSet s : descendents) {
				if (s!=null && s.contains(c.index()))
					freq[c.index()]++;
			}
		}
		Arrays.sort(core, new Comparator<Clause>() {
			public int compare(Clause arg0, Clause arg1) {
				return freq[arg0.index()] - freq[arg1.index()];
			}});
		return core;
	}
	
	/**
	 * Returns an iterator that traverses the core clauses in the given trace
	 * according to the frequency of their occurence in the trace.
	 * @return an iterator that traverses the core clauses in the given trace
	 * according to  to the frequency of their occurence in the trace.
	 * @see kodkod.engine.ucore.CRRStrategy#order(kodkod.engine.satlab.ResolutionTrace)
	 */
	@Override
	protected Iterator<Clause> order(ResolutionTrace trace) {
		final Clause[] core = coreFreq(trace);
		return leastFreq ? Containers.iterate(core) : Containers.iterate(core.length-1,-1,core);
	}

}
