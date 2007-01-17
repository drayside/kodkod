package kodkod.engine;

import java.util.Iterator;

import kodkod.engine.fol2sat.TranslationLog;
import kodkod.engine.satlab.SATProver;

/**
 * Contains a proof of unsatisfiability of a
 * given FOL formula.
 * 
 * @specfield formula: Formula // the unsatisfiable formula
 * @specfield bounds: Bounds // the bounds with respect to which the formula is unsatisfiable
 */
public final class Proof {
	private final SATProver solver;
	private final TranslationLog log;
	private boolean fixed;
	/**
	 * Constructs a new Proof that will extract the 
	 * unsatisfiable core for this.formula from 
	 * the given solver.  
	 * @requires solver.solve() has been called and 
	 * it returned false.
	 * @requires log.formula is the formula whose translation
	 * resulted in the given SATProver
	 */
	Proof(SATProver solver, TranslationLog log) {
		this.solver = (SATProver)solver;
		this.fixed = false;
		this.log = log;
	}
	
	/**
	 * Returns the size of the proof:  the
	 * number of clauses in this.formula's
	 * unsatisfiable core.
	 * @return the size of this proof
	 */
	public int size() {
		return solver.coreSize();
	}
	
	/**
	 * Returns an iterator over the CNF clauses 
	 * that constitute the proof of this.formula's
	 * unsatisfiability with respect to this.bounds.
	 * @return an iterator over this formula's 
	 * unsatisfiable core.
	 */
	public Iterator<int[]> clauses() {
		return solver.unsatCore();
	}
	
	/**
	 * Refines the proof of this.formula's unsatisfiability
	 * until a fixed point is reached; that is, until the 
	 * formula's unsatisfiable core cannot be minimized any
	 * further.
	 * @effects refines this proof until a fixed point is reached.
	 */
	public void refine() {
		if (fixed) return;
		
		int size;
		do {
			size = solver.coreSize();
			solver.retainCore();
			solver.solve();
		} while (solver.coreSize()<size);
		
		fixed = true;
	}
	
	/**
	 * Refines the proof of this.formula's unsatisfiability
	 * <code>numOfIterations</code> times or until a fixed point is reached,
	 * whichever comes first.
	 * @effects refines this proof 
	 * <code>numOfIterations</code> times or until a fixed point is reached,
	 * whichever comes first.
	 * @throws IllegalArgumentException - numOfIterations < 0 
	 */
	public void refine(int numOfIterations) {
		if (numOfIterations < 0)
			throw new IllegalArgumentException("numOfIterations < 0: " + numOfIterations);
		if (fixed || numOfIterations==0) return;
		
		int size;
		do {
			size = solver.coreSize();
			solver.retainCore();
			solver.solve();
		} while (solver.coreSize()<size && --numOfIterations>0);
		fixed = numOfIterations > 0;
	}
	
	/**
	 * Returns the log of the translation events resulting in this.proof.
	 * @return this.log
	 */
	public TranslationLog log() { return log; }
	
}