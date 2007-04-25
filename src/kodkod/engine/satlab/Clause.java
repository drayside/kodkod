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

import java.util.Collections;
import java.util.List;

import kodkod.util.ints.IntIterator;
import kodkod.util.ints.IntSet;
import kodkod.util.ints.IntTreeSet;
import kodkod.util.ints.Ints;

/**
 * A propositional clause. 
 * @specfield literals: set int \\ clause literals 
 * @specfield antecedents: Clause[] 
 * @invariant 0 !in literals 
 * @invariant no lit: literals | -lit in literals
 * @invariant !antecedents.isEmpty() => 
 *  literals = { lit: antecedents[int].literals | 
 *   no i: int | lit in antecedents[i].literals && -lit in antecedents[i+1].literals }
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
	 * clause in all {@link ResolutionTrace resolution traces} that contain it.
	 * In other words, a resolution trace never contains two clauses with
	 * the same index.  Indices are kept as small as possible.
	 * @return a non-negative integer index that uniquely identifies this
	 * clause in all {@link ResolutionTrace resolution traces} that contain it.
	 */
	public final int index() { return index; }
	
	/**
	 * Returns an unmodifiable IntSet view of this.literals.
	 * @return an unmodifiable IntSet view of this.literals
	 */
	public abstract IntSet literals();
	
	
	/**
	 * Returns an unmodifiable List view of this.antecedents.
	 * @return an unmodifiable List view of this.antecedents.
	 */
	public abstract List<Clause> antecedents();
	
	/**
	 * Returns true if this clause was learned during solving.
	 * @return some this.antecedents
	 */
	public abstract boolean learned();
	
	/**
	 * Returns a new core clause with the given index and literals.
	 * @return {c : Clause | c.literals = literals and no c.antecedents } 
	 */
	static Clause core(int index, IntSet literals) {
		return new Core(index, literals);
	}
	
	/**
	 * Returns a new learned clause with the given index and antecedents.
	 * @invariant no index & antecdents.*antecedents.index()
	 * @return {c: Clause | c.index() = index && c.antecedents = antecedents } 
	 */
	static Clause learned(int index, List<Clause> antecedents) {
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
		public List<Clause> antecedents() { return Collections.emptyList(); }
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
		final List<Clause> antecedents;
		IntSet literals;
		/**
		 * Constructs a new learned clause with the given index and literals.
		 */
		Learned(int index, List<Clause> antecedents) {
			super(index);
			this.antecedents = antecedents;
			this.literals = null;
		}
		
		public List<Clause> antecedents() { return antecedents; }
		public boolean learned() { return true; }

		public IntSet literals() {
			if (literals==null) {
				final IntSet litset = new IntTreeSet();
				for(Clause a : antecedents) {
					for(IntIterator itr = a.literals().iterator(); itr.hasNext(); ) {
						int lit = itr.next();
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