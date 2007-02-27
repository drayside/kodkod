/**
 * 
 */
package kodkod.engine.ucore;

import java.util.AbstractSet;
import java.util.Collections;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Set;

import kodkod.engine.satlab.Clause;
import kodkod.engine.satlab.ReductionStrategy;
import kodkod.engine.satlab.ResolutionTrace;
import kodkod.util.ints.IntBitSet;
import kodkod.util.ints.IntSet;
import kodkod.util.ints.Ints;

/**
 * A naive minimization strategy that tests the presence
 * of each clause in the core by simply re-solving {core} - {clause}.
 * @author Emina Torlak
 */
public final class NaiveStrategy implements ReductionStrategy {
	private IntSet excluded;
	
	/** 
	 * Constructs a new instance of NaiveStrategy. 
	 * @effects no this.traces' and no this.nexts'
	 **/
	public NaiveStrategy() {
		excluded = Ints.EMPTY_SET;
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.satlab.ReductionStrategy#next(kodkod.engine.satlab.ResolutionTrace)
	 */
	@SuppressWarnings("unchecked")
	public Set<Clause> next(ResolutionTrace trace) {
		if (excluded.isEmpty()) { // the first time this method is called
			excluded = new IntBitSet(trace.maxIndex()+1);
		}
		for(Clause c : trace.core()) {
			if (excluded.add(c.index()))
				return new DifferenceClauses(trace, c);
		}
		return Collections.EMPTY_SET;
	}

	private static final class DifferenceClauses extends AbstractSet<Clause> {
		final ResolutionTrace trace;
		final Clause excluded;
		/**
		 * Constructs the set of all clauses in trace.core - excluded
		 */
		DifferenceClauses(ResolutionTrace trace, Clause excluded) {
			this.trace = trace;
			this.excluded = excluded;
		}
		
		@Override
		public int size() {
			return trace.core().size()-1;
		}
		
		@Override
		public Iterator<Clause> iterator() {
			return new Iterator<Clause>() {
				final Iterator<Clause> itr = trace.core().iterator();
				Clause next = null;
				public boolean hasNext() { 
					if (next != null) return true;
					while(itr.hasNext()) {
						Clause c = itr.next();
						if (!c.equals(excluded)) {
							next = c;
							return true;
						}
					}
					return false;
				}

				public Clause next() {
					if (!hasNext()) throw new NoSuchElementException();
					final Clause last = next;
					next = null;
					return last;
				}

				public void remove() {throw new UnsupportedOperationException(); }
			};
		}

		
		
	}
}
