/*
 * Solver.java
 * Created on May 18, 2005
 */
package kodkod.engine;

import static kodkod.engine.Solution.Outcome.SATISFIABLE;
import static kodkod.engine.Solution.Outcome.TRIVIALLY_SATISFIABLE;
import static kodkod.engine.Solution.Outcome.TRIVIALLY_UNSATISFIABLE;
import static kodkod.engine.Solution.Outcome.UNSATISFIABLE;

import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;

import kodkod.ast.Formula;
import kodkod.ast.Relation;
import kodkod.engine.config.Options;
import kodkod.engine.fol2sat.HigherOrderDeclException;
import kodkod.engine.fol2sat.Translation;
import kodkod.engine.fol2sat.TranslationAbortedException;
import kodkod.engine.fol2sat.Translator;
import kodkod.engine.fol2sat.TrivialFormulaException;
import kodkod.engine.fol2sat.UnboundLeafException;
import kodkod.engine.satlab.SATAbortedException;
import kodkod.engine.satlab.SATMinSolver;
import kodkod.engine.satlab.SATProver;
import kodkod.engine.satlab.SATSolver;
import kodkod.instance.Bounds;
import kodkod.instance.Instance;
import kodkod.instance.TupleSet;
import kodkod.util.ints.IntIterator;
import kodkod.util.ints.IntSet;


/** 
 * Implementation of a computational engine for solving relational formulae.
 * A {@link kodkod.ast.Formula formula} is solved with respect to given 
 * {@link kodkod.instance.Bounds bounds} and {@link kodkod.engine.config.Options options}.
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
	 * @param bounds Bounds with which solve()  was called
	 * @param desc TrivialFormulaException thrown as the result of the formula simplifying to a constant
	 * @param translTime translation time
	 * @return the result of solving a trivially (un)sat formula.
	 */
	private static Solution trivial(Bounds bounds, TrivialFormulaException desc, long translTime) {
		final Statistics stats = new Statistics(0, 0, 0, translTime, 0);
		if (desc.formulaValue().booleanValue()) {
			return new Solution(TRIVIALLY_SATISFIABLE, stats, padInstance(toInstance(desc.bounds()), bounds), desc.reduction(), null);
		} else {
			return new Solution(TRIVIALLY_UNSATISFIABLE, stats, null, desc.reduction(), null);
		}
	}

	/**
	 * Returns the statistics corresponding to the given translation, translation time, and solving time.
	 * @return the statistics corresponding to the given translation, translation time, and solving time.
	 */
	private static Statistics stats(Translation translation, long translTime, long solveTime) {
		return new Statistics(translation.cnf().numberOfVariables(), translation.numPrimaryVariables(), 
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
		final Solution sol = new Solution(SATISFIABLE, stats, padInstance(translation.interpret(), originalBounds), null, null);
		translation.cnf().free();
		return sol;
	}

	/**
	 * Returns the result of solving an unsat formula.
	 * @param translation the translation 
	 * @param stats translation / solving stats
	 * @return the result of solving an unsat formula.
	 */
	private static Solution unsat(Options options, Translation translation, Statistics stats) {
		final SATSolver cnf = translation.cnf();
		if (cnf instanceof SATProver) {
			return new Solution(UNSATISFIABLE, stats, null, null, new Proof(options.solver(), (SATProver) cnf, translation.log()));
		} else { // can free memory
			final Solution sol = new Solution(UNSATISFIABLE, stats,  null, null, null);
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
	 * 
	 * @requires !this.options.logTranslation && this.options.solver.minimizer
	 * @return Solution to the formula with respect to the given bounds and cost
	 * @throws NullPointerException - formula = null || bounds = null || cost = null
	 * @throws kodkod.engine.fol2sat.UnboundLeafException - the formula contains an undeclared variable or
	 * a relation not mapped by the given bounds
	 * @throws kodkod.engine.fol2sat.HigherOrderDeclException - the formula contains a higher order declaration that cannot
	 * be skolemized, or it can be skolemized but this.options.skolemize is false.
	 * @throws AbortedException - this solving task was interrupted with a call to Thread.interrupt on this thread
	 * @throws IllegalArgumentException -  some  (formula.^children & Relation) - cost.relations
	 * @throws IllegalStateException - !this.options.solver.minimizer || this.options.logTranslation
	 * @see Solution
	 * @see Options
	 * @see Cost
	 */
	public Solution solve(Formula formula, Bounds bounds, Cost cost)
			throws HigherOrderDeclException, UnboundLeafException, AbortedException {
		
		if (options.logTranslation() || !options.solver().minimizer())
			throw new IllegalStateException();
		
		final long startTransl = System.currentTimeMillis();
		try {
			
			options.setLogTranslation(false);
			final Translation translation = Translator.translate(formula, bounds, options);
			final long endTransl = System.currentTimeMillis();

			final SATMinSolver cnf = (SATMinSolver)translation.cnf();
			for(Relation r : bounds.relations()) {
				IntSet vars = translation.primaryVariables(r);
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
			
			return isSat ? sat(bounds, translation, stats) : unsat(options, translation, stats);
		} catch (TrivialFormulaException trivial) {
			final long endTransl = System.currentTimeMillis();
			return trivial(bounds, trivial, endTransl - startTransl);
		} catch (TranslationAbortedException tie) {
			throw new AbortedException(tie);
		} catch (SATAbortedException sae) {
			throw new AbortedException(sae);
		}
	}

	/**
	 * Attempts to satisfy the given formula with respect to the specified bounds or
	 * prove the formula's unsatisfiability.
	 * If the operation is successful, the method returns a Solution that contains either
	 * an instance of the formula or an unsatisfiability proof.  Note that an unsatisfiability
	 * proof will be constructed iff this.options specifies the use of a core extracting SATSolver.
	 * Additionally, the CNF variables in the proof can be related back to the nodes in the given formula 
	 * iff this.options has translation logging enabled.  Translation logging also requires that 
	 * there are no subnodes in the given formula that are both syntactically shared and contain free variables.  
	 * 
	 * @requires this.options.logTranslation => (all n: formula.*children | #n.~children > 1 => no freeVariables(n))
	 * @return Solution to the formula with respect to the given bounds
	 * @throws NullPointerException - formula = null || bounds = null
	 * @throws kodkod.engine.fol2sat.UnboundLeafException - the formula contains an undeclared variable or
	 * a relation not mapped by the given bounds
	 * @throws kodkod.engine.fol2sat.HigherOrderDeclException - the formula contains a higher order declaration that cannot
	 * be skolemized, or it can be skolemized but this.options.skolemize is false.
	 * @throws AbortedException - this solving task was interrupted with a call to Thread.interrupt on this thread
	 * @see Solution
	 * @see Options
	 * @see Proof
	 */
	public Solution solve(Formula formula, Bounds bounds)
			throws HigherOrderDeclException, UnboundLeafException, AbortedException {
		
		final long startTransl = System.currentTimeMillis();
		
		try {		
		
			final Translation translation = Translator.translate(formula, bounds, options);
			final long endTransl = System.currentTimeMillis();

			final SATSolver cnf = translation.cnf();
			
			options.reporter().solvingCNF(translation.numPrimaryVariables(), cnf.numberOfVariables(), cnf.numberOfClauses());
			final long startSolve = System.currentTimeMillis();
			final boolean isSat = cnf.solve();
			final long endSolve = System.currentTimeMillis();

			final Statistics stats = stats(translation, endTransl - startTransl, endSolve - startSolve);
			return isSat ? sat(bounds, translation, stats) : unsat(options, translation, stats);
			
		} catch (TrivialFormulaException trivial) {
			final long endTransl = System.currentTimeMillis();
			return trivial(bounds, trivial, endTransl - startTransl);
		} catch (TranslationAbortedException tie) {
			throw new AbortedException(tie);
		} catch (SATAbortedException sae) {
			throw new AbortedException(sae);
		}
	}
	
	/**
	 * Attempts to find all solutions to the given formula with respect to the specified bounds or
	 * to prove the formula's unsatisfiability.
	 * If the operation is successful, the method returns an iterator over n Solution objects. The outcome
	 * of the first n-1 solutions is SAT or trivially SAT, and the outcome of the nth solution is UNSAT
	 * or tirivally  UNSAT.  Note that an unsatisfiability
	 * proof will be constructed for the last solution iff this.options specifies the use of a core extracting SATSolver.
	 * Additionally, the CNF variables in the proof can be related back to the nodes in the given formula 
	 * iff this.options has variable tracking enabled.  Translation logging also requires that 
	 * there are no subnodes in the given formula that are both syntactically shared and contain free variables.  
	 * 
	 * @requires this.options.logTranslation => (all n: formula.*children | #n.~children > 1 => no freeVariables(n))
	 * @return an iterator over all the Solutions to the formula with respect to the given bounds
	 * @throws NullPointerException - formula = null || bounds = null
	 * @throws kodkod.engine.fol2sat.UnboundLeafException - the formula contains an undeclared variable or
	 * a relation not mapped by the given bounds
	 * @throws kodkod.engine.fol2sat.HigherOrderDeclException - the formula contains a higher order declaration that cannot
	 * be skolemized, or it can be skolemized but this.options.skolemize is false.
	 * @throws AbortedException - this solving task was interrupted with a call to Thread.interrupt on this thread
	 * @throws IllegalStateException - !this.options.solver().incremental()
	 * @see Solution
	 * @see Options
	 * @see Proof
	 */
	public Iterator<Solution> solveAll(final Formula formula, final Bounds bounds) 
		throws HigherOrderDeclException, UnboundLeafException, AbortedException {
		
		if (!options.solver().incremental())
			throw new IllegalArgumentException("cannot enumerate solutions without an incremental solver.");
		
		return new SolutionIterator(formula, bounds, options);
		
	}

	/**
	 * {@inheritDoc}
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return options.toString();
	}
	
	/**
	 * An iterator over all solutions of a model.
	 * @author Emina Torlak
	 */
	private static final class SolutionIterator implements Iterator<Solution> {
		private final Options options;
		private Formula formula;
		private Bounds bounds;
		private Translation translation;
		private long translTime;
		private int trivial;
		
		/**
		 * Constructs a solution iterator for the given formula, bounds, and options.
		 */
		SolutionIterator(Formula formula, Bounds bounds, Options options) {
			this.formula = formula;
			this.bounds = bounds;
			this.options = options;
			this.translation = null;
			this.trivial = 0;
		}
		
		/**
		 * Returns true if there is another solution.
		 * @see java.util.Iterator#hasNext()
		 */
		public boolean hasNext() {
			return formula != null;
		}
		/**
		 * Solves translation.cnf and adds the negation of the
		 * found model to the set of clauses.
		 * @requires this.translation != null
		 * @effects this.translation.cnf is modified to eliminate
		 * the  current solution from the set of possible solutions
		 * @return current solution
		 */
		private Solution nonTrivialSolution() {
			try {
				final SATSolver cnf = translation.cnf();
				options.reporter().solvingCNF(translation.numPrimaryVariables(), cnf.numberOfVariables(), cnf.numberOfClauses());
				final long startSolve = System.currentTimeMillis();
				final boolean isSat = cnf.solve();
				final long endSolve = System.currentTimeMillis();

				final Statistics stats = stats(translation, translTime, endSolve - startSolve);
				if (isSat) {
					// extract the current solution
					final Solution sol = new Solution(SATISFIABLE, stats,	padInstance(translation.interpret(), bounds), null, null);
					// add the negation of the current model to the solver
					final int primary = translation.numPrimaryVariables();
					final int[] notModel = new int[primary];
					for(int i = 1; i <= primary; i++) {
						notModel[i-1] = cnf.valueOf(i) ? -i : i;
					}
					cnf.addClause(notModel);
					return sol;
				} else {
					formula = null; bounds = null; // unsat, no more solutions, free up some space
					return unsat(options, translation, stats);
				}
			} catch (SATAbortedException sae) {
				throw new AbortedException(sae);
			}
		}
		
		/**
		 * Packages the information from the given trivial formula exception
		 * into a solution and returns it.  If the formula is satisfiable, 
		 * this.formula and this.bounds are modified to eliminate the current
		 * trivial solution from the set of possible solutions.
		 * @requires this.translation = null
		 * @effects this.formula and this.bounds are modified to eliminate the current
		 * trivial solution from the set of possible solutions.
		 * @return current solution
		 */
		private Solution trivialSolution(TrivialFormulaException tfe) {
			final Statistics stats = new Statistics(0, 0, 0, translTime, 0);
			if (tfe.formulaValue().booleanValue()) {
				trivial++;
				final Instance raw = toInstance(tfe.bounds());
				final Solution sol = new Solution(TRIVIALLY_SATISFIABLE, stats, padInstance(raw, bounds), tfe.reduction(), null);
				bounds = bounds.clone();
				Formula notModel = Formula.FALSE;
				for(Map.Entry<Relation, TupleSet> entry: raw.relationTuples().entrySet()) {
					Relation r = entry.getKey();
					Relation rmodel = Relation.nary(r.name()+"_"+trivial, r.arity());
					bounds.boundExactly(rmodel, entry.getValue());
					notModel = notModel.or(r.eq(rmodel).not());
				}
				formula = formula.and(notModel);
				return sol;
			} else {
				formula = null; bounds = null;
				return new Solution(TRIVIALLY_UNSATISFIABLE, stats,  null, tfe.reduction(), null);
			}
		}
		/**
		 * Returns the next solution if any.
		 * @see java.util.Iterator#next()
		 */
		public Solution next() {
			if (!hasNext()) throw new NoSuchElementException();
			if (translation==null) {
				try {
					translTime = System.currentTimeMillis();
					translation = Translator.translate(formula, bounds, options);
					translTime = System.currentTimeMillis() - translTime;
					return nonTrivialSolution();
				} catch (TrivialFormulaException tfe) {
					translTime = System.currentTimeMillis() - translTime;
					return trivialSolution(tfe);
				} catch (TranslationAbortedException tae) {
					throw new AbortedException(tae);
				}
			} else {
				return nonTrivialSolution();
			}
		}

		/**
		 * @see java.util.Iterator#remove()
		 */
		public void remove() {
			throw new UnsupportedOperationException();
		}
		
	}
}
