package kodkod.engine.ucore;

import java.util.AbstractSet;
import java.util.Collections;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Set;

import kodkod.engine.satlab.Clause;
import kodkod.engine.satlab.ResolutionTrace;
import kodkod.util.ints.IntBitSet;
import kodkod.util.ints.IntSet;

/**
 * An unmodifiable set view of all clauses in a given trace that are not (backward) reachable from
 * a given set of core clauses.
 * @specfield trace: ResolutionTrace
 * @specfield excluded: set trace.core
 * @author Emina Torlak
 */
final class UnreachableClauses extends AbstractSet<Clause> {
	final IntSet unreachable;
	final ResolutionTrace trace;
	
	/**
	 * Constructs the set of all clauses in trace.conflict.*antecedents that 
	 * are not reachable from the given excluded clause.
	 * @requires excluded in trace.core
	 * @effects this.trace' = trace && this.excluded' = excluded 
	 */
	public UnreachableClauses(ResolutionTrace trace, Clause excluded) {
		this(trace, Collections.singleton(excluded));
	}
	
	/**
	 * Constructs the set of all clauses in trace.conflict.*antecedents that 
	 * are not reachable from the given excluded clause.
	 * @requires excluded in trace.core
	 * @effects this.trace' = trace && this.excluded' = excluded 
	 */
	public UnreachableClauses(ResolutionTrace trace, Set<Clause> excluded) {
		this.trace = trace;
		this.unreachable = unreachable(trace, excluded);
	}
	
	/**
	 * Returns the indices of the clauses from the given trace that are unreachable
	 * from the specified clauses.
	 * @requires all c: clauses | !c.learned()
	 * @return (trace.conflict.*antecedents - clauses.*(~antecedents)).index()
	 */
	static IntSet unreachable(ResolutionTrace trace, Set<Clause> clauses) {
		final IntSet reachable = new IntBitSet(trace.maxIndex()+1);
		final IntSet all = new IntBitSet(trace.maxIndex()+1);
		
		reachable(trace.conflict(), clauses, reachable, all);
		
//		System.out.println("all:  " + all);
//		System.out.println("reachable:  " + reachable);
		
		all.removeAll(reachable);
//		System.out.println("unreachable:  " + all);
		return all;
	}
	
	/**
	 * Fills the reachable set with the indices of the descedents of the given clause from which
	 * the specified clauses can be reached following the antecedents relation.  The visited set 
	 * is filled with the indices of all clauses reachable from the given clause.  The method should
	 * be called with both sets empty.
	 * @requires all c: clauses | !c.learned()
	 * @effects visited.ints' = visited.ints + clause.*antecedents
	 * @effects reachable.ints' = reachable.ints + (clause.*antecedents & clauses.*(~antecedents)).traceIndex()
	 */
	private static void reachable(Clause clause, Set<Clause> clauses, IntSet reachable, IntSet visited) {
		if (visited.add(clause.index())) { // not seen
			if (clause.learned()) {
				boolean anteReachable = false;
				for(Clause ante : clause.antecedents()) {
					reachable(ante, clauses, reachable, visited);
					anteReachable = anteReachable || reachable.contains(ante.index());
				}
				if (anteReachable) {
					reachable.add(clause.index());
				}
			} else {
				if (clauses.contains(clause))
					reachable.add(clause.index());
			}
		}
	}
	
	/**
	 * Returns the number of clauses in this.trace that are not 
	 * (backward) reachable from this.excluded.
	 * @return #(this.trace.conflict.*antecedents - this.excluded.*(~antecedents))
	 */
	public int size() { return unreachable.size();}

	/**
	 * Returns an iterator over the clauses in this.trace that are not 
	 * (backward) reachable from this.excluded.
	 * @return an iterator over this.trace.conflict.*antecedents - this.excluded.*(~antecedents)
	 */
	public Iterator<Clause> iterator() {
		return new Iterator<Clause>() {
			final Iterator<Clause> itr = trace.iterator();
			Clause next = null;
			
			public boolean hasNext() {
				if (next!=null) return true;
				while(itr.hasNext()) {
					Clause c = itr.next();
					if (unreachable.contains(c.index())) {
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
			
			public void remove() { throw new UnsupportedOperationException();}
			
		};
	}
	
}