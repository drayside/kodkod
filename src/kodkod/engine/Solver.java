/*
 * Solver.java
 * Created on May 18, 2005
 */
package kodkod.engine;

import kodkod.ast.Formula;
import kodkod.ast.Relation;
import kodkod.engine.fol2sat.Translation;
import kodkod.engine.fol2sat.Translator;
import kodkod.engine.satlab.SATSolver;
import kodkod.instance.Bounds;
import kodkod.instance.Instance;
import kodkod.util.ints.IntIterator;

import org.sat4j.specs.TimeoutException;


/** 
 * Implementation of a computational engine for solving relational formulae.
 * A {@link kodkod.ast.Formula formula} is solved with respect to given 
 * {@link kodkod.instance.Bounds bounds} and {@link kodkod.engine.Options options}.
 * 
 * @specfield options: Options 
 * @author Emina Torlak 
 */
public final class Solver {
	private final Options options;
	
	/**
	 * Constructs a new Solver with the default options.
	 * @effects this.options' = new Options()
	 */
	public Solver() {
		this.options = new Options();
	}
	
	/**
	 * Constructs a new Solver with the given options.
	 * @effects this.options' = options
	 * @throws NullPointerException - options = null
	 */
	public Solver(Options options) {
		if (options==null)
			throw new NullPointerException();
		this.options = options;
	}
	/**
	 * Returns the Options object used by this Solver
	 * to guide translation of formulas from first-order
	 * logic to cnf.
	 * @return this.options
	 */
	public Options options() {
		return options;
	}
	
	/**
	 * "Pads" the argument instance with the mappings that occur in bounds.lowerBound
	 * but not in the instance. 
	 * @requires instance.relations in bounds.relations
	 * @effects instance.relations' = bounds.relations' &&
	 *          instance.tuples' = bounds.lowerBound ++ instance.tuples
	 * @return instance
	 */
	private static Instance padInstance(Instance instance, Bounds bounds) {
		for(Relation r: bounds.relations()) {
			if (!instance.contains(r)) {
				instance.add(r, bounds.lowerBound(r));
			}
		}
		for(IntIterator iter = bounds.ints().iterator(); iter.hasNext(); ) {
			int i = iter.nextInt();
			instance.add(i, bounds.exactBound(i));
		}
		return instance;
	}
	
	/**
	 * Creates an instance from the given Bounds.  The instance
	 * is simply the mapping bounds.lowerBound.
	 * @return the instance corresponding to bounds.lowerBound
	 */
	private static Instance toInstance(Bounds bounds) {
		final Instance instance = new Instance(bounds.universe());
		for (Relation r : bounds.relations()) {
			instance.add(r, bounds.lowerBound(r));
		}
		for(IntIterator iter = bounds.ints().iterator(); iter.hasNext(); ) {
			int i = iter.nextInt();
			instance.add(i, bounds.exactBound(i));
		}
		return instance;
	}
	
	/**
	 * Attempts to satisfy the given formula with respect to the specified bounds or
	 * prove the formula's unsatisfiability.
	 * If the operation is successful, the method returns a Solution that contains either
	 * an instance of the formula or an unsatisfiability proof.  Note that an unsatisfiability
	 * proof will be constructed iff this.options specifies the use of a core extracting SATSolver.
	 * Additionally, the CNF variables in the proof can be related back to the nodes in the given formula 
	 * iff this.options has variable tracking enabled.  
	 * If the solver runs out of time, a TimeoutException is thrown.  
	 * 
	 * @return Solution to the formula with respect to the given bounds
	 * @throws NullPointerException - formula = null || bounds = null
	 * @throws IllegalArgumentException - the formula contains an unbound variable
	 * @throws IllegalArgumentException - the formula contains a relation not mapped by the given bounds object
	 * @throws TimeoutException - it takes more than this.timeout of seconds to solve the formula
	 * @see Solution
	 * @see Options
	 * @see Proof
	 */
	public Solution solve(Formula formula, Bounds bounds) throws kodkod.engine.TimeoutException {
		long startTransl = System.currentTimeMillis(), endTransl;
		try {
			final Translation translation = Translator.translate(formula, bounds, options);
			endTransl = System.currentTimeMillis();
			
			final SATSolver cnf = translation.cnf();
			
			final long startSolve = System.currentTimeMillis();
			final boolean isSat = cnf.solve();
			final long endSolve = System.currentTimeMillis();
			
			final Statistics stats = 
				new Statistics(cnf.numberOfVariables(), translation.numberOfPrimaryVariables(), 
						       cnf.numberOfClauses(), endTransl-startTransl, endSolve-startSolve);

			if (isSat) {
				return new Solution(Solution.Outcome.SATISFIABLE, stats,
									translation.skolems(),
							        padInstance(translation.interpret(), bounds),
							        null, null);
			} else {
				return new Solution(Solution.Outcome.UNSATISFIABLE, stats,
									translation.skolems(),
								    null, null, new Proof(cnf, translation.variableUsage()));
			}

		} catch (TrivialFormulaException trivial) {
			endTransl = System.currentTimeMillis();
			final Statistics stats = new Statistics(0,0,0,endTransl-startTransl,0);
			if (trivial.formulaValue().booleanValue()) {
				return new Solution(Solution.Outcome.TRIVIALLY_SATISFIABLE, stats,
									trivial.skolems(),
								    padInstance(toInstance(trivial.bounds()), bounds), 
								    trivial.reduction(), null);
			} else {
				return new Solution(Solution.Outcome.TRIVIALLY_UNSATISFIABLE, stats,
					    	            trivial.skolems(), null, trivial.reduction(), null);
			}
		}
	}
	
	/**
	 * {@inheritDoc}
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return options.toString();
	}
	
//	public static void main(String[] args) {
//		System.out.print("stuff");
//	}
 
}
