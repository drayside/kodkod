/*
 * Solver.java
 * Created on May 18, 2005
 */
package kodkod.engine;

import java.util.Map;

import kodkod.ast.Formula;
import kodkod.ast.Node;
import kodkod.ast.Relation;
import kodkod.engine.fol2sat.HigherOrderDeclException;
import kodkod.engine.fol2sat.Translation;
import kodkod.engine.fol2sat.Translator;
import kodkod.engine.fol2sat.TrivialFormulaException;
import kodkod.engine.fol2sat.UnboundLeafException;
import kodkod.engine.satlab.SATMinSolver;
import kodkod.engine.satlab.SATProver;
import kodkod.engine.satlab.SATSolver;
import kodkod.engine.settings.Options;
import kodkod.instance.Bounds;
import kodkod.instance.Instance;
import kodkod.util.ints.IntIterator;
import kodkod.util.ints.IntSet;

import org.sat4j.specs.TimeoutException;

/** 
 * Implementation of a computational engine for solving relational formulae.
 * A {@link kodkod.ast.Formula formula} is solved with respect to given 
 * {@link kodkod.instance.Bounds bounds} and {@link kodkod.engine.settings.Options options}.
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
		if (options == null)
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
		for (Relation r : bounds.relations()) {
			if (!instance.contains(r)) {
				instance.add(r, bounds.lowerBound(r));
			}
		}
		for (IntIterator iter = bounds.ints().iterator(); iter.hasNext();) {
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
		for (IntIterator iter = bounds.ints().iterator(); iter.hasNext();) {
			int i = iter.nextInt();
			instance.add(i, bounds.exactBound(i));
		}
		return instance;
	}

	/**
	 * Returns the result of solving a trivially (un)sat formula.
	 * @param originalBounds Bounds with which solve()  was called
	 * @param desc TrivialFormulaException thrown as the result of the formula simplifying to a constant
	 * @param translTime translation time
	 * @return the result of solving a trivially (un)sat formula.
	 */
	private static Solution trivial(Bounds originalBounds, TrivialFormulaException desc, long translTime) {
		final Statistics stats = new Statistics(0, 0, 0, translTime, 0);
		if (desc.formulaValue().booleanValue()) {
			return new Solution(Solution.Outcome.TRIVIALLY_SATISFIABLE, stats,  
					padInstance(toInstance(desc.bounds()), originalBounds), desc.reduction(), null);
		} else {
			return new Solution(Solution.Outcome.TRIVIALLY_UNSATISFIABLE,
					stats,  null, desc.reduction(), null);
		}
	}

	/**
	 * Returns the statistics corresponding to the given translation, translation time, and solving time.
	 * @return the statistics corresponding to the given translation, translation time, and solving time.
	 */
	private static Statistics stats(Translation translation, long translTime, long solveTime) {
		return new Statistics(translation.cnf().numberOfVariables(), translation.numberOfPrimaryVariables(), 
				translation.cnf().numberOfClauses(), translTime, solveTime);
	}
	
	/**
	 * Returns the result of solving a sat formula.
	 * @param originalBounds Bounds with which  solve() was called
	 * @param translation the translation
	 * @param stats translation / solving stats
	 * @return the result of solving a sat formula.
	 */
	private static Solution sat(Bounds originalBounds, Translation translation, Statistics stats) {
		final Solution sol = new Solution(Solution.Outcome.SATISFIABLE, stats, 
				padInstance(translation.interpret(), originalBounds), null, null);
		translation.cnf().free();
		return sol;
	}

	/**
	 * Returns the result of solving an unsat formula.
	 * @param translation the translation 
	 * @param stats translation / solving stats
	 * @return the result of solving an unsat formula.
	 */
	private static Solution unsat(Translation translation, Statistics stats) {
		final SATSolver cnf = translation.cnf();
		if (cnf instanceof SATProver) {
			return new Solution(Solution.Outcome.UNSATISFIABLE, stats,  
					null, null, new Proof((SATProver) cnf, translation.variableUsage()));
		} else { // can free memory
			final Solution sol = new Solution(Solution.Outcome.UNSATISFIABLE, stats,  null, null, null);
			cnf.free();
			return sol;
		}
	}
	
	/**
	 * Attempts to satisfy the given formula with respect to the specified bounds, while
	 * minimizing the specified cost function.
	 * If the operation is successful, the method returns a Solution that contains either a minimal-cost
	 * instance of the formula or a proof of unsatisfiability.  The latter is generated iff 
	 * the SAT solver generated by this.options.solver() is a {@link SATProver SATProver} in  addition
	 * to being a {@link kodkod.engine.satlab.SATMinSolver SATMinSolver}.
	 * If the solver runs out of time, a TimeoutException is thrown.  
	 * 
	 * @return Solution to the formula with respect to the given bounds and cost
	 * @throws NullPointerException - formula = null || bounds = null || cost = null
	 * @throws kodkod.engine.fol2sat.UnboundLeafException - the formula contains an undeclared variable or
	 * a relation not mapped by the given bounds
	 * @throws kodkod.engine.fol2sat.HigherOrderDeclException - the formula contains a higher order declaration that cannot
	 * be skolemized, or it can be skolemized but this.options.skolemize is false.
	 * @throws TimeoutException - it takes more than this.timeout of seconds to solve the formula
	 * @throws IllegalArgumentException - !this.options.solver.minimizers || some cost.relations - (formula.^children & Relation)
	 * @see Solution
	 * @see Options
	 * @see Cost
	 */
	public Solution solve(Formula formula, Bounds bounds, Cost cost)
			throws HigherOrderDeclException, UnboundLeafException {
		
		if (!options.solver().minimizers())
			throw new IllegalArgumentException(options.solver() + " is not a minimizing solver.");

		long startTransl = System.currentTimeMillis(), endTransl;
		try {
			final Translation translation = Translator.translate(formula, bounds, options);
			endTransl = System.currentTimeMillis();

			final SATMinSolver cnf = (SATMinSolver)translation.cnf();
			final Map<Node, IntSet> varUsage = translation.variableUsage();
			for(Relation r : bounds.relations()) {
				IntSet vars = varUsage.get(r);
				if (vars != null) {
					int rcost = cost.edgeCost(r);
					for(IntIterator iter = vars.iterator();  iter.hasNext(); ) {
						cnf.setCost(iter.nextInt(), rcost);
					}
				}
			}
			
			options.reporter().solvingCNF(0, cnf.numberOfVariables(), cnf.numberOfClauses());
			final long startSolve = System.currentTimeMillis();
			final boolean isSat = cnf.solve();
			final long endSolve = System.currentTimeMillis();

			final Statistics stats = stats(translation, endTransl - startTransl, endSolve - startSolve);
			
			return isSat ? sat(bounds, translation, stats) : unsat(translation, stats);
		} catch (TrivialFormulaException trivial) {
			endTransl = System.currentTimeMillis();
			return trivial(bounds, trivial, endTransl - startTransl);
		}
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
	 * @throws kodkod.engine.fol2sat.UnboundLeafException - the formula contains an undeclared variable or
	 * a relation not mapped by the given bounds
	 * @throws kodkod.engine.fol2sat.HigherOrderDeclException - the formula contains a higher order declaration that cannot
	 * be skolemized, or it can be skolemized but this.options.skolemize is false.
	 * @throws TimeoutException - it takes more than this.timeout of seconds to solve the formula
	 * @see Solution
	 * @see Options
	 * @see Proof
	 */
	public Solution solve(Formula formula, Bounds bounds)
			throws HigherOrderDeclException, UnboundLeafException {
		long startTransl = System.currentTimeMillis(), endTransl;
		try {
			final Translation translation = Translator.translate(formula, bounds, options);
			endTransl = System.currentTimeMillis();

			final SATSolver cnf = translation.cnf();
			
			options.reporter().solvingCNF(translation.numberOfPrimaryVariables(), cnf.numberOfVariables(), cnf.numberOfClauses());
			final long startSolve = System.currentTimeMillis();
			final boolean isSat = cnf.solve();
			final long endSolve = System.currentTimeMillis();

			final Statistics stats = stats(translation, endTransl - startTransl, endSolve - startSolve);
			return isSat ? sat(bounds, translation, stats) : unsat(translation, stats);
		} catch (TrivialFormulaException trivial) {
			endTransl = System.currentTimeMillis();
			return trivial(bounds, trivial, endTransl - startTransl);
		}
	}

	/**
	 * {@inheritDoc}
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return options.toString();
	}

}
