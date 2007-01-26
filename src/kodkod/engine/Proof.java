package kodkod.engine;

import java.util.Iterator;

import kodkod.engine.fol2sat.TranslationLog;
import kodkod.engine.satlab.SATFactory;
import kodkod.engine.satlab.SATProver;
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
	private final SATFactory factory;
	private SATProver solver;
	private boolean fixed;
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
		this.fixed = false;
		this.log = log;
		this.factory = factory;
	}
	
	/**
	 * Collects the set of clause-identifying literals for the 
	 * root clauses at the leaves of the given clause.  A clause-identifying
	 * literal is the literal with the largest magnitude.  The method should
	 * be initially called with this.solver.proof() and two modifiable, empty sets
	 * as arguments.
	 * @effects the set of clause-identifying literals for the 
	 * root clauses at the leaves of the given clause are added to the core set.
	 * @effects the indices of the given clause and its descendents are added to the visited set.
	 * @return core
	 */
	private static IntSet core(SATProver.Clause clause, IntSet core, IntSet visited) {
		if (visited.add(clause.index())) {
			if (clause.learned()) {
				for(SATProver.Clause c : clause.antecedents()) {
					core(c, core, visited);
				}
			} else {
				final IntSet literals = clause.literals();
				final int idLit = StrictMath.max(StrictMath.abs(literals.min()), StrictMath.abs(literals.max()));
				core.add(idLit);
				core.add(-idLit);
			}
		}
		return core;
	}
	
	/**
	 * Collects the root clauses at the leaves of the given clause. The method should
	 * be initially called with an instance of this.factory.solver() initialized with
	 * this.solver.numberOfVariables() variables and a modifiable, empty set
	 * as arguments.
	 * @effects the root clauses at the leaves of the given clause are added to the core 
	 * @effects the indices of the given clause and its descendents are added to the visited set.
	 * @return core
	 */
	private static SATProver core(SATProver.Clause clause, SATProver core, IntSet visited) {
		if (visited.add(clause.index())) {
			if (clause.learned()) {
				for(SATProver.Clause c : clause.antecedents()) {
					core(c, core, visited);
				}
			} else {
				core.addClause(clause.literals().toIntArray());
			}
		}
		return core;
	}
	
	/**
	 * Returns a new SATProver with no clauses and the same
	 * number of variables as this.solver.
	 * @return a new SATProver with no clauses and the same
	 * number of variables as this.solver.
	 */
	private SATProver newProver() {
		final SATProver prover = (SATProver) factory.instance();
		prover.addVariables(this.solver.numberOfVariables());
		return prover;
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
		
		SATProver next = core(solver.proof(), newProver(), new IntTreeSet());
		
		while(next.numberOfClauses() < solver.numberOfClauses()) {
			solver = next;
			solver.solve();
			next = core(solver.proof(), newProver(), new IntTreeSet());
		}
		
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
		
		SATProver next = core(solver.proof(), newProver(), new IntTreeSet());
		
		while(next.numberOfClauses() < solver.numberOfClauses() && numOfIterations-- > 0) {
			solver = next;
			solver.solve();
			next = core(solver.proof(), newProver(), new IntTreeSet());
		}
		
		fixed = (next.numberOfClauses() == solver.numberOfClauses());
	}
	
	/**
	 * Returns an iterator over translation records for the formulas
	 * that are in the unsatisfiable core of this.formula.
	 * @return  an iterator over translation records for the formulas
	 * that are in the unsatisfiable core of this.formula.
	 */
	public Iterator<TranslationLog.Record> core() { 
		return log.replay(core(solver.proof(), new IntTreeSet(), new IntTreeSet())); 
	}
	
}