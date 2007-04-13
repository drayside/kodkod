/* 
 * Kodkod -- Copyright (c) 2005-2007, Emina Torlak
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package kodkod.engine.satlab;

import java.util.AbstractList;
import java.util.AbstractSet;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Set;

import kodkod.util.collections.ArrayStack;
import kodkod.util.collections.Stack;
import kodkod.util.ints.ArrayIntVector;
import kodkod.util.ints.IntBitSet;
import kodkod.util.ints.IntIterator;
import kodkod.util.ints.IntSet;
import kodkod.util.ints.IntVector;
import kodkod.util.ints.Ints;

/**
 * A resolution trace that proves the unsatisfiability of a 
 * set of clauses in a {@link SATProver}.
 * @specfield conflict: Clause
 * @specfield core: set conflict.^antecedents
 * @specfield resolvents: set conflict.^antecedents
 * @invariant core = { c: conflict.^antecedents | no c.antecedents }
 * @invariant resolvents = { r: conflict.*antecedents | some r.antecedents }
 * @invariant no conflict.literals 
 * @invariant all r: resolvents - conflict | some r.literals
 * @invariant no r: resolvents | r in r.^antecedents
 * @author Emina Torlak
 */
public final class ResolutionTrace implements Iterable<Clause>{
	private final int traceSize, coreSize, maxIndex;
	private final Clause conflict;
	private Set<Clause> core;	
	/**
	 * Constructs a resolution trace from the given raw trace and resolvent indices.  
	 * The raw trace is encoded as follows.  At each index i in the resolvents set, 
	 * trace[i] stores a resolvent clause encoded as an array of integers [i1,...,ik] such 
	 * that i1,..., ik < i and trace[i1],..., trace[ik] stores the encoding of an antecedent of trace[i].  
	 * At each index j outside the resolvent set but reachable from it, trace[j] stores a core clause encoded as an array of integers that
	 * represent trace[j]'s literals.  
	 * @requires all i: resolvents.ints | all j: trace[i].length | 0 <= trace[i][j] < i
	 * @effects the trace array may be modified
	 */
	ResolutionTrace(Object[] trace, IntSet resolvents) {
		final IntSet reachable = reachable(trace, resolvents);
		int index = 0, core = 0;
		for(IntIterator itr = reachable.iterator(); itr.hasNext(); ) {
			int idx = itr.next();
			if (resolvents.contains(idx)) {
				trace[idx] = Clause.learned(index++, antecedents(trace, idx));
			} else {
				trace[idx] = Clause.core(index++, literals(trace, idx));
				core++;
			}
		}
		this.conflict = (Clause)trace[resolvents.max()];
		this.traceSize = reachable.size();
		this.coreSize = core;
		this.maxIndex = index-1;
	}
	
	/**
	 * Constructs a resolution trace from the given original trace, raw trace based on the original
	 * trace, and resolvent indices.  The raw trace is encoded as follows.  At each index i in the resolvents set, 
	 * trace[i] stores a resolvent clause encoded as an array of integers [i1,...,ik] such 
	 * that i1,..., ik < i and trace[i1],..., trace[ik] stores the encoding of an antecedent of trace[i].  
	 * At each index j outside the resolvents set but reachable from it, trace[j] stores a Clause from original.conflict.^antecedents.  
	 * @requires all i: resolvents.ints | all j: trace[i].length | 0 <= trace[i][j] < i
	 * @requires all i: [0..trace.length) - resolvents.ints | trace[i] in original.conflict.^antecedents
	 * @effects the trace array may be modified
	 * @effects the resolvents set may be modified
	 */
	ResolutionTrace(ResolutionTrace original, Object[] trace, IntSet resolvents) {
		final IntSet reachable = reachable(trace, resolvents);
		resolvents.retainAll(reachable);
		reachable.removeAll(resolvents);
		
		final BitSet indices = new BitSet(StrictMath.max(original.maxIndex, reachable.size()-1)+1);
		for(IntIterator itr = reachable.iterator(); itr.hasNext(); ) {
			setAll(indices, (Clause)trace[itr.next()]);
		}
		int core = 0;
		for(Clause clause : original) {
			if (!clause.learned() && indices.get(clause.index())) 
				core++;
		}
		int index = 0;
		for(IntIterator itr = resolvents.iterator(); itr.hasNext(); ) {
			int idx = itr.next();
			index = indices.nextClearBit(index);
			trace[idx] = Clause.learned(index, antecedents(trace, idx));
			indices.set(index);	
		}
		this.conflict = (Clause)trace[resolvents.max()];
		this.traceSize = indices.cardinality();
		this.coreSize = core;
		this.maxIndex = indices.length()-1;
	}
	
	/**
	 * Sets the bits of the given bitset that correspond to the given clause and its
	 * descedents.
	 * @requires all c: clause.*antecedents | indices.get(c.index) => 
	 *   all c': c.^antecedents | indices.get(c'.index)
	 * @effects all c: clause.*antecedents | indices.get(c.index)
	 */
	private static void setAll(BitSet indices, Clause clause) {
		if (!indices.get(clause.index())) {
			indices.set(clause.index());
			if (clause.learned()) {
				for(Clause ante : clause.antecedents()) {
					setAll(indices, ante);
				}
			}
		}
	}
	/**
	 * Given a trace array and resolvent indices,
	 * finds the indices of the clauses reachable from
	 * the conflict clause, trace[resolvents.max()].  The trace array
	 * encodes a resolution proof as follows.  At each index i in the resolvents set, 
	 * trace[i] stores a resolvent clause encoded as an array of integers [i1,...,ik] such 
	 * that i1,..., ik < i and trace[i1],..., trace[ik] stores the encoding of an antecedent of trace[i].  
	 * At each index j outside the resolvent set but reachable from it, trace[j] stores a core clause.  
	 * @requires all i: resolvents.ints | all j: trace[i].length | 0 <= trace[i][j] < i
	 * @return the indices of the clauses reachable from the conflict clause, trace[resolvents.max()].
	 */
	private static IntSet reachable(Object[] trace, IntSet resolvents) {
		final int conflict = resolvents.max();
		final IntSet reachable = new IntBitSet(conflict + 1);
		final IntVector vec = new ArrayIntVector();
		
		vec.add(conflict);
		while(!vec.isEmpty()) {
			int back = vec.removeAt(vec.size()-1);
			if (resolvents.contains(back)) { // learned clause
				for(int ante : (int[])trace[back]) { 
					if (reachable.add(ante)) { // unseen clause
						vec.add(ante);
					}
				}
			} else { // unseen core clause
				reachable.add(back);
			}
		}
		
		reachable.add(conflict);
		return reachable;
	}
	
	/**
	 * Returns the antecedents of the learned clause at the given index in the specified trace.  
	 * @requires trace[learntIdx] instance of int[]
	 * @requires all j: [0..trace[learntIdx].length) | trace[j] instanceof Clause
	 * @return l: List<Clause> | l.size() = trace[learntIdx].length && 
	 *   all i: [0..l.size()) | l.get(i) = trace[trace[learntIdx][i]] }
	 */
	private static List<Clause> antecedents(Object[] trace, int learntIdx) {
		final int[] anteIndices = (int[]) trace[learntIdx];
		final Clause[] ante = new Clause[anteIndices.length];
		for(int i = 0; i < ante.length; i++) {
			ante[i] = (Clause) trace[anteIndices[i]];
		}
		return new AbstractList<Clause>() {
			@Override
			public Clause get(int index) {
				return ante[index];
			}
			@Override
			public int size() {
				return ante.length;
			}
		};
	}
	
	/**
	 * Returns the literals of the core clause at the given index in the specified trace.
	 * @requires trace[core] instance of int[]
	 * @return { i: int | some j: int | i = trace[coreIdx][j] }
	 */
	private static IntSet literals(Object[] trace, int coreIdx) {
		final int[] literals = (int[]) trace[coreIdx];
		java.util.Arrays.sort(literals);
		return Ints.asSet(literals);
	}
	
	/**
	 * Returns the relative hardness of this trace,
	 * which is defined as the ratio of the total number of clauses
	 * and the number of core clauses.
	 * @return this.size() / #this.core
	 */
	public double relativeHardness() {
		return ((double)traceSize)/((double)coreSize);
	}

	/**
	 * Returns the size of this trace (the number of clauses).
	 * @return #this.resolvents + #this.core
	 */
	public int size() {	return traceSize; }
		
	/**
	 * Returns the largest index identifying 
	 * a clause in this trace.
	 * @return max((this.core + this.resolvents).index)
	 */
	public int maxIndex() {	return maxIndex; }
	
	/**
	 * Returns an unmodifiable view of this.core.
	 * @return this.core
	 */
	public Set<Clause> core() {
		if (core==null) {
			core = new AbstractSet<Clause>() {
				public int size() {	return coreSize; }
				
				public Iterator<Clause> iterator() {
					return new Iterator<Clause>() {
						final Iterator<Clause> itr = ResolutionTrace.this.iterator();
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
		return core;
	}
	
	
	/**
	 * Returns the conflict clause (i.e. root of the resolution trace).
	 * @return this.conflict
	 */
	public Clause conflict() { 	return conflict; }
	
	/**
	 * Returns an iterator that traverses this resolution 
	 * trace in the default depth-first order.  The effect
	 * of this method is the same as calling {@link #iterator(kodkod.engine.satlab.TraversalStrategy)
	 * iterator(Traversal.DEPTH_FIRST)}.
	 * @return an iterator that traverses this resolution 
	 * proof in the default depth-first order.
	 */
	public Iterator<Clause> iterator() { return new DepthFirstIterator(); }
	
	/**
	 * Returns an iterator that traverses this resolution 
	 * trace in the given traversal order.
	 * @return an iterator that traverses this resolution 
	 * proof in given traversal order.
	 */
	public Iterator<Clause> iterator(TraversalStrategy traversal) {
		switch (traversal) {
		case TOPOLOGICAL: return new TopologicalIterator();
		case DEPTH_FIRST: return new DepthFirstIterator();
		case BREADTH_FIRST: return new BreadthFirstIterator();
		default : throw new InternalError(); // can't happen
		}
	}
//	
//	/**
//	 * Returns an iterator that traverses this resolution
//	 * trace in the default depth-first order and that only
//	 * returns clauses that one of the given indices.
//	 * @return an iterator that traverses this resolution
//	 * trace in the default depth-first order and that only
//	 * returns clauses that one of the given indices.
//	 */
//	public Iterator<Clause> iterator(final IntSet indices) {
//		return new Iterator<Clause>() {
//			final Iterator<Clause> itr = ResolutionTrace.this.iterator();
//			Clause next = null;
//			
//			public boolean hasNext() {
//				if (next!=null) return true;
//				while(itr.hasNext()) {
//					Clause c = itr.next();
//					if (indices.contains(c.index())) {
//						next = c;
//						return true;
//					}
//				}
//				return false;
//			}
//
//			public Clause next() {
//				if (!hasNext()) throw new NoSuchElementException();
//				final Clause last = next;
//				next = null;
//				return last;
//			}
//			
//			public void remove() { throw new UnsupportedOperationException();}
//			
//		};
//	}
	
	
	/**
	 * A base class for a trace iterator.
	 */
	private  abstract class TraceIterator<L extends List<Clause>> implements Iterator<Clause> {
		final L nexts;
		
		/**
		 * Constructs a new instance of the trace iterator using
		 * the given list implementation to store the clauses to 
		 * be returned.  The queue is initialized with the conflict clause.
		 * @requires queue.isEmpty()
		 */
		TraceIterator(L list) {
			this.nexts = list;
			this.nexts.add(conflict);
		}
		
		/**
		 * {@inheritDoc}
		 * @see java.util.Iterator#hasNext()
		 */
		public final boolean hasNext() { return !nexts.isEmpty(); }
		
		/**
		 * Returns true if the given clause should be added to the nexts list.
		 * @return true if the given clause should be added to the nexts list.
		 */
		public abstract boolean addToNexts(Clause clause);
		
		/**
		 * Removes the next clause to be returned from the queue and returns it.
		 * @effects removes the next clause from the queue
		 * @return returns the next clause to be returned
		 */
		public abstract Clause popNext();
		
		/**
		 * {@inheritDoc}
		 * @see java.util.Iterator#next()
		 */
		public final Clause next() {
			if (!hasNext()) throw new NoSuchElementException();
			final Clause next = popNext();
			if (next.learned()) { 
				for(Clause ante : next.antecedents()) {
					if (addToNexts(ante)) {
						nexts.add(ante);
					}
				}
			}
			return next;
		}
		
		/** @throws UnsupportedOperationException */
		public final void remove() { throw new UnsupportedOperationException(); }
	}
	
	/**
	 * A depth first iterator.
	 */
	private  final class DepthFirstIterator extends TraceIterator<ArrayList<Clause>> {
		private final IntSet visited;
		
		public DepthFirstIterator() {
			super(new ArrayList<Clause>());
			visited = new IntBitSet(maxIndex+1);
		}
		
		public Clause popNext() { return nexts.remove(nexts.size()-1);	}
		public boolean addToNexts(Clause clause) { return visited.add(clause.index());	}
	}
	
	/**
	 * A breadth first iterator.
	 */
	private  final class BreadthFirstIterator extends TraceIterator<LinkedList<Clause>> {
		private final IntSet visited;
		
		public BreadthFirstIterator() {
			super(new LinkedList<Clause>());
			visited = new IntBitSet(maxIndex+1);
		}
		
		public Clause popNext() { return nexts.removeFirst(); }
		public boolean addToNexts(Clause clause) { return visited.add(clause.index());	}	
	}
	
	/**
	 * A topological iterator.
	 */
	private  final class TopologicalIterator extends TraceIterator<LinkedList<Clause>> {
		private final int[] inDegree;
		
		TopologicalIterator() {
			super(new LinkedList<Clause>());
			inDegree = new int[maxIndex+1];
			
			final Stack<Clause> stack = new ArrayStack<Clause>();
			stack.push(conflict);
			while(!stack.empty()) {
				Clause front = stack.pop();
				if (front.learned()) {
					for(Clause ante : front.antecedents()) {
						if (inDegree[ante.index()]++ == 0) { // not yet visited
							stack.push(ante);
						}
					}
				} 
			}
		}
		
		public Clause popNext() { return nexts.removeFirst();}
		public boolean addToNexts(Clause clause) { return --inDegree[clause.index()]==0;	}
	}
}
