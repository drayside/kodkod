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
package kodkod.engine;

import java.util.Iterator;

import kodkod.engine.fol2sat.TranslationLog;
import kodkod.engine.fol2sat.TranslationRecord;
import kodkod.engine.satlab.Clause;
import kodkod.engine.satlab.ReductionStrategy;
import kodkod.engine.satlab.SATFactory;
import kodkod.engine.satlab.SATProver;
import kodkod.engine.ucore.EmptyClauseConeStrategy;
import kodkod.util.ints.IntSet;
import kodkod.util.ints.IntTreeSet;

/**
 * Contains a proof of unsatisfiability of a
 * given FOL formula.
 * 
 * @specfield formula: Formula // the unsatisfiable formula
 * @specfield bounds: Bounds // the bounds with respect to which the formula is unsatisfiable
 */
public final class Proof {
	private final TranslationLog log;
	private SATProver solver;

	/**
	 * Constructs a new Proof that will extract the 
	 * unsatisfiable core for this.formula from the given solver.  
	 * @requires the given factory produces SATProver instances
	 * @requires solver.solve() has been called and 
	 * it returned false.
	 * @requires log.formula is the formula whose translation
	 * resulted in the given SATProver
	 */
	Proof(SATFactory factory, SATProver solver, TranslationLog log) {
		this.solver = solver;
		this.log = log;
	}
	
	/**
	 * Returns the magnitude of the literal with the greatest
	 * absolute value in the given clause.  This number uniquely
	 * ties each clause to its corresponding  subformula of this.formula.
	 * @return the magnitude of the literal with the greatest
	 * absolute value in the given clause
	 */
	private static int idLiteral(Clause clause) {
		final IntSet literals = clause.literals();
		return StrictMath.max(StrictMath.abs(literals.min()), StrictMath.abs(literals.max()));
	}
	
	/**
	 * Collects the {@link #idLiteral(kodkod.engine.satlab.Clause) identifying literals}
	 * of the core clauses in an instance of IntSet.
	 * @return an IntSet initialized with the identifying literals of the core clauses
	 */
	private IntSet coreLiterals() {
		final IntSet idLits = new IntTreeSet();
		
		for(Clause clause : solver.proof().core()) {
//			System.out.println(clause);
			int id = idLiteral(clause);
			idLits.add(id);
			idLits.add(-id);
		}
		
//		for(Iterator<TranslationLog.Record> itr = log.replay(); itr.hasNext();) {
//			System.out.println(itr.next());
//		}
		return idLits;
	}

	
	/**
	 * Refines the proof of this.formula's unsatisfiability
	 * until a fixed point is reached, using the Empty-Clause Cone
	 * algorithm.  The resulting proof is  not guaranteed to be minimal. 
	 * @effects refines this proof until a fixed point is reached.
	 * @see <a href="http://research.microsoft.com/users/lintaoz/papers/SAT_2003_core.pdf">L. Zhang and S. Malik. <i>Extracting small unsatisfiable cores from unsatisfiable
	 * Boolean formula.</i>  In Proceedings of Sixth International Conference on Theory and Applications of 
	 * Satisfiability Testing (SAT '03). 2003.</a>
	 */
	public void refine() {
		solver.proof(new EmptyClauseConeStrategy());
	}
	
	/**
	 * Returns the relative hardness of the proof of this.formula's
	 * unsatisfiability.  The higher this number, the harder the proof 
	 * is to minimize.  
	 * @return the relative hardness of the proof of this.formula's unsatisfiability.
	 */
	public double relativeHardness() {
		return solver.proof().relativeHardness();
	}


	/**
	 * Minimizes the proof of this.formula's unsatisfiability
	 * using the specified proof reduction strategy.  
	 * @effects minimizes the proof of this.formula's unsatisfiability
	 * using the specified proof reduction strategy. 
	 * @see kodkod.engine.satlab.ReductionStrategy
	 */
	public void minimize(ReductionStrategy strategy) {
		solver.proof(strategy);
//		System.out.println("testing minimality ...");
//		ResolutionTrace trace = solver.proof();
//		List<int[]> core = new ArrayList<int[]>(trace.core().size());
//		for(Clause c : trace.core()) {
//			core.add(c.literals().toArray());
//		}
//		for(int i = 0, max = core.size(); i < max; i++) {
//			SATSolver s = SATFactory.MiniSat.instance();
//			s.addVariables(solver.numberOfVariables());
//			for(int j = 0; j < max; j++) {
//				if (i!=j)
//					s.addClause(core.get(j));
//			}
//			if (!s.solve())
//				throw new IllegalStateException("");
//		}
	}
	
	/**
	 * Returns an iterator over the {@link TranslationRecord log records} for the formulas
	 * that are in the unsatisfiable core of this.formula.   The record objects returned by the iterator are not 
	 * guaranteed to be immutable.  In particular, the state of a record object
	 * returned by <tt>next()</tt> is guaranteed to remain the same only until the
	 * subsequent call to <tt>next()</tt>.
	 * @return  an iterator over the {@link TranslationRecord log records} for the formulas
	 * that are in the unsatisfiable core of this.formula.
	 */
	public Iterator<TranslationRecord> core() { 
		return log.replay(coreLiterals());
	}
	
	/**
	 * Returns the log of the translation that resulted
	 * in this proof.
	 * @return log of the translation that resulted in this proof
	 */
	public TranslationLog log() {
		return log;
	}
}