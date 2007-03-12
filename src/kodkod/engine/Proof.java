package kodkod.engine;

import java.util.Iterator;

import kodkod.engine.fol2sat.TranslationLog;
import kodkod.engine.satlab.Clause;
import kodkod.engine.satlab.SATFactory;
import kodkod.engine.satlab.SATProver;
import kodkod.engine.ucore.EmptyClauseConeStrategy;
import kodkod.engine.ucore.HybridStrategy;
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
	 * algorithm (L. Zhang and S. Malik. <i>Extracting small unsatisfiable cores from unsatisfiable
	 * Boolean formula.</i>  In Proceedings of Sixth International Conference on Theory and Applications of 
	 * Satisfiability Testing (SAT '03). 2003.).  The resulting proof is 
	 * not guaranteed to be minimal. 
	 * @effects refines this proof until a fixed point is reached.
	 * @see L. Zhang and S. Malik. <i>Extracting small unsatisfiable cores from unsatisfiable
	 * Boolean formula.</i>  In Proceedings of Sixth International Conference on Theory and Applications of 
	 * Satisfiability Testing (SAT '03). 2003.
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
		System.out.println("coresize: " + solver.proof().core().size());
		return solver.proof().relativeHardness();
	}


	/**
	 * Minimizes the proof of this.formula's unsatisfiability
	 * using a variant of the Complete Resolution Refutation algorithm 
	 * (N. Dershowitz, Z. Hanna, and A. Nadel.  <i>A scalable algorithm for minimal unsatisfiable core
	 * extraction.</i>  In Proceedings of Ninth International Conference on Theory and Applications of 
	 * Satisfiability Testing (SAT '06). 2006.).  The speed of minimization
	 * corresponds, roughly, to the {@link #relativeHardness() relative hardness} of the proof.  In other words,
	 * the higher the relative hardness, the longer the minimization process.
	 * @effects minimizes the proof of this.formula's unsatisfiability
	 * using a variant of the Complete Resolution Refutation algorithm
	 * @see N. Dershowitz, Z. Hanna, and A. Nadel.  <i>A scalable algorithm for minimal unsatisfiable core
	 * extraction.</i>  In Proceedings of Ninth International Conference on Theory and Applications of 
	 * Satisfiability Testing (SAT '06). 2006.
	 */
	public void minimize() {
//		solver.proof(new DistExtremumCRRStrategy(false));
		solver.proof(new HybridStrategy(log));
//		solver.proof(new NaiveStrategy());
//		solver.proof(new FreqExtremumCRRStrategy(true));
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
	 * Returns an iterator over the {@link TranslationLog.Record log records} for the formulas
	 * that are in the unsatisfiable core of this.formula.   The record objects returned by the iterator are not 
	 * guaranteed to be immutable.  In particular, the state of a record object
	 * returned by <tt>next()</tt> is guaranteed to remain the same only until the
	 * subsequent call to <tt>next()</tt>.
	 * @return  an iterator over the {@link TranslationLog.Record log records} for the formulas
	 * that are in the unsatisfiable core of this.formula.
	 */
	public Iterator<TranslationLog.Record> core() { 
		return log.replay(coreLiterals());
	}
	
}