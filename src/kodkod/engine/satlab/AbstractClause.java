/**
 * 
 */
package kodkod.engine.satlab;

import java.util.Collections;
import java.util.Set;

import kodkod.util.ints.IntIterator;
import kodkod.util.ints.IntSet;
import kodkod.util.ints.IntTreeSet;
import kodkod.util.ints.Ints;

/**
 * An immutable implementation of the SATProver.Clause interface
 * @author Emina Torlak
 */
abstract class AbstractClause implements SATProver.Clause {
	private final int index;
	
	/**
	 * Constructs a new clause with the given index.
	 */
	private AbstractClause(int index) {
		assert index >= 0;
		this.index = index;
	}
	

	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.satlab.SATProver.Clause#index()
	 */
	public int index() { return index; }

	/**
	 * Returns a new root clause with the given index and literals.
	 * @return {c : Clause | c.literals = literals and no c.antecedents } 
	 */
	static SATProver.Clause root(int index, IntSet literals) {
		return new Root(index, literals);
	}
	
	/**
	 * Returns a new learned clause with the given index and antecedents.
	 * @return {c: Caluse | c.index = index && c.antecedents = antecedents } 
	 */
	static SATProver.Clause learned(int index, Set<SATProver.Clause> antecedents) {
		return new Learned(index, antecedents);
	}
	
	/**
	 * A clause that was added to the solver (not learned).
	 */
	private static final class Root extends AbstractClause {
		final IntSet literals;
		/**
		 * Constructs a new root clause with the given index and literals.
		 */
		Root(int index, IntSet literals) {
			super(index);
			this.literals = literals;
		}
		public Set<SATProver.Clause> antecedents() { return Collections.emptySet(); }
		public boolean learned() { return false; }
		public IntSet literals() { return literals;	}
		public String toString() {
			final StringBuilder buf = new StringBuilder();
			buf.append("root ");
			buf.append(index());
			buf.append(": ");
			buf.append(literals);
			return buf.toString();
		}
	}
	
	/**
	 * A clause that was learned (from root clauses)  during solving.
	 * @author Emina Torlak
	 */
	private static final class Learned extends AbstractClause {
		final Set<SATProver.Clause> antecedents;
		IntSet literals;
		/**
		 * Constructs a new learned clause with the given index and literals.
		 */
		Learned(int index, Set<SATProver.Clause> antecedents) {
			super(index);
			this.antecedents = antecedents;
			this.literals = null;
		}
		
		public Set<SATProver.Clause> antecedents() { return antecedents; }
		public boolean learned() { return true; }

		public IntSet literals() {
			if (literals==null) {
				final IntSet litset = new IntTreeSet();
				for(SATProver.Clause a : antecedents) {
					for(IntIterator itr = a.literals().iterator(); itr.hasNext(); ) {
						int lit = itr.nextInt();
						if (!litset.remove(-lit))
							litset.add(lit);
					}
				}
				literals = Ints.asSet(litset.toIntArray());
			}
			return literals;
		}
		
		public String toString() {
			final StringBuilder buf = new StringBuilder();
			buf.append("learned ");
			buf.append(index());
			buf.append(": ");
			for(SATProver.Clause a : antecedents) {
				buf.append(a.index());
				buf.append(" ");
			}
			return buf.toString();
		}
	}
}
