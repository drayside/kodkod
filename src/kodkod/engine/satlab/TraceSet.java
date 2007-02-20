/**
 * 
 */
package kodkod.engine.satlab;

import java.util.AbstractSet;
import java.util.Iterator;
import java.util.NoSuchElementException;

import kodkod.util.ints.IntBitSet;
import kodkod.util.ints.IntSet;

/**
 * A set implementation that efficiently stores clauses
 * belonging to a given {@link ResolutionTrace}.  <b>This is not 
 * a general purpose set implementation.</b>  In particular, the behavior
 * of an instance of this class is undefined if any of its methods that
 * take a Clause or Object argument are called with a Clause
 * that is not a member of the trace with which the instance 
 * was constructed.
 * @specfield trace: ResolutionTrace
 * @specfield clauses: set trace.conflict.*antecedents
 * @author Emina Torlak
 */
public final class TraceSet extends AbstractSet<Clause> {
	private final IntSet indices;
	private final ResolutionTrace trace;
	/**
	 * Constructs a clause set for the clauses from the
	 * given resolution trace.
	 * @effects this.trace' = trace && no this.clauses'
	 */
	public TraceSet(ResolutionTrace trace) {
		this.trace = trace;
		indices = new IntBitSet(trace.maxIndex()+1);
	}

	/**
	 * Adds the given member of this.trace to this.clauses
	 * if not already present and returns true.  Otherwise
	 * does nothing and returns false.
	 * @requires o in this.trace.conflict.*antecedents
	 * @effects this.clauses' = this.clauses + o
	 * @return this.clauses' != this.clauses
	 */
	public boolean add(Clause o) {
		return indices.add(o.index());
	}

	/**
	 * Removes all clauses from this.clauses.
	 * @effects no this.clauses'
	 */
	public void clear() {
		indices.clear();
	}

	/**
	 * Returns true if this.clauses contains the given
	 * member of this.trace.
	 * @requires o in this.trace.conflict.*antecedents
	 * @return o in this.clauses
	 */
	public boolean contains(Object o) {
		return indices.contains(((Clause)o).index());
	}

	/**
	 * Returns an iterator over this.clauses.  Note that 
	 * the returned iterator may not behave correctly if 
	 * there has been an attempt to {@link #add(Clause) add} a Clause not in 
	 * this.trace.conflict.*antecedents to this.clauses.
	 * @return an iterator over this.clauses.
	 */
	public Iterator<Clause> iterator() {
		return new Iterator<Clause>() {
			final Iterator<Clause> itr = trace.iterator();
			int last = -1;
			int returned = 0;
			public boolean hasNext() {	return itr.hasNext() && returned < indices.size(); }

			public Clause next() {
				if (!hasNext()) throw new NoSuchElementException();
				for(Clause next = itr.next(); itr.hasNext(); next = itr.next()) {
					if (indices.contains(next.index())) {
						last = next.index();
						returned++;
						return next;
					}
				}
				throw new IllegalStateException();
			}

			public void remove() {
				if (last < 0) throw new IllegalStateException();
				indices.remove(last);
				last = -1;
			}
			
		};
	}

	/**
	 * Removes the given member of this.trace from this.clauses
	 * if present and returns true.  Otherwise does nothing and returns false.
	 * @requires o in this.trace.conflict.*antecedents
	 * @effects this.clauses' = this.clauses - o
	 * @return this.clauses' != this.clauses
	 */
	public boolean remove(Object o) {
		return indices.remove(((Clause)o).index());
	}

	/**
	 * Returns the cardinality of this.clauses.
	 * @return #this.clauses
	 */
	public int size() {
		return indices.size();
	}

}
