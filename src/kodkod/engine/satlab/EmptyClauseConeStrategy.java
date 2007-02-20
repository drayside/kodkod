/**
 * 
 */
package kodkod.engine.satlab;

import java.util.AbstractSet;
import java.util.Collections;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Set;

/**
 * This class implements the Empty Clause Cone algorithm for reducing
 * the size of an unsatisfiable core (L. Zhang and S. Malik. <i>Extracting small unsatisfiable cores from unsatisfiable
 * Boolean formula.</i>  In Proceedings of Sixth International Conference on Theory and Applications of 
 * Satisfiability Testing (SAT '03). 2003.).
 * @author Emina Torlak
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
		if (trace.coreSize() >= lastCore)
			return Collections.EMPTY_SET;
		
		lastCore = trace.coreSize();	
		
		return new AbstractSet<Clause>() {
			
			public int size() { return lastCore;}
			
			public Iterator<Clause> iterator() {
				return new Iterator<Clause>() {
					final Iterator<Clause> itr = trace.iterator();
					Clause next = null;
					
					public boolean hasNext() { 
						if (next!=null) return true;
						while(itr.hasNext()) {
							Clause c = itr.next();
							if (!c.learned()) {
								next = c;
								return true;
							}
						}
						return false;
					}
					public Clause next() {
						if (!hasNext()) throw new NoSuchElementException();
						Clause last = next;
						next = null;
						return last;
					}
					public void remove() { throw new UnsupportedOperationException();}				
				};
			}	
		};
	}

}
