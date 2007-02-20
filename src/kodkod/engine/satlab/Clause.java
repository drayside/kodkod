package kodkod.engine.satlab;

import java.util.Collections;
import java.util.Set;

import kodkod.util.ints.IntIterator;
import kodkod.util.ints.IntSet;
import kodkod.util.ints.IntTreeSet;
import kodkod.util.ints.Ints;

/**
 * A propositional clause. 
 * @specfield index: [0...)
 * @specfield literals: set int \\ clause literals 
 * @specfield antecedents: set ProofClause 
 * @invariant 0 !in literals 
 * @invariant no lit: literals | -lit in literals
 * @invariant some antecedents => literals = { lit: antecedents.literals | -lit !in antecedents.literals }
 */
public abstract class Clause {
	private final int index;
	
	/**
	 * Constructs a new clause with the given index.
	 */
	private Clause(int index) {
		this.index = index;
	}
	
	/**
	 * A non-negative integer index that uniquely identifies this
	 * clause in the {@link SATSolver sat solver} that contains it.
	 * In other words, a sat solver never contains two clauses with
	 * the same index.  Indices are kept as small as possible. 
	 * @return this.index
	 */
	public final int index() { return index; }
	
	/**
	 * Returns an unmodifiable IntSet view of this.literals.
	 * @return an unmodifiable IntSet view of this.literals
	 */
	public abstract IntSet literals();
	
	/**
	 * Returns an unmodifiable Set view of this.antecedents.
	 * @return an unmodifiable Set view of this.antecedents.
	 */
	public abstract Set<Clause> antecedents();
	
	/**
	 * Returns true if this clause was learned during solving.
	 * @return some this.antecedents
	 */
	public abstract boolean learned();
	
	/**
	 * Returns a new core clause with the given index and literals.
	 * @return {c : ProofClause | c.literals = literals and no c.antecedents } 
	 */
	static Clause core(int index, IntSet literals) {
		return new Core(index, literals);
	}
	
	/**
	 * Returns a new learned clause with the given index and antecedents.
	 * @invariant no index & antecdents.*antecedents.index
	 * @return {c: ProofClause | c.index = index && c.antecedents = antecedents } 
	 */
	static Clause learned(int index, Set<Clause> antecedents) {
		return new Learned(index, antecedents);
	}
		
	/**
	 * A clause that was added to the solver (not learned).
	 */
	private static final class Core extends Clause {
		final IntSet literals;
		/**
		 * Constructs a new root clause with the given index and literals.
		 */
		Core(int index, IntSet literals) {
			super(index);
			this.literals = literals;
		}
		public Set<Clause> antecedents() { return Collections.emptySet(); }
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
	private static final class Learned extends Clause {
		final Set<Clause> antecedents;
		IntSet literals;
		/**
		 * Constructs a new learned clause with the given index and literals.
		 */
		Learned(int index, Set<Clause> antecedents) {
			super(index);
			this.antecedents = antecedents;
			this.literals = null;
		}
		
		public Set<Clause> antecedents() { return antecedents; }
		public boolean learned() { return true; }

		public IntSet literals() {
			if (literals==null) {
				final IntSet litset = new IntTreeSet();
				for(Clause a : antecedents) {
					for(IntIterator itr = a.literals().iterator(); itr.hasNext(); ) {
						int lit = itr.nextInt();
						if (!litset.remove(-lit))
							litset.add(lit);
					}
				}
				literals = Ints.asSet(litset.toArray());
			}
			return literals;
		}
		
		public String toString() {
			final StringBuilder buf = new StringBuilder();
			buf.append("learned ");
			buf.append(index());
			buf.append(": ");
			for(Clause a : antecedents) {
				buf.append(a.index());
				buf.append(" ");
			}
			return buf.toString();
		}
	}
}