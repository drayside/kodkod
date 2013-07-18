package kodkod.multiobjective.algorithms;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import kodkod.ast.Formula;
import kodkod.engine.Solution;
import kodkod.engine.CheckpointedSolver;
import kodkod.instance.Bounds;
import kodkod.multiobjective.MetricPoint;
import kodkod.multiobjective.MultiObjectiveOptions;
import kodkod.multiobjective.MultiObjectiveProblem;
import kodkod.multiobjective.concurrency.SolutionNotifier;
import kodkod.multiobjective.statistics.StatKey;
import kodkod.multiobjective.statistics.StepCounter;

public final class CheckpointedGuidedImprovementAlgorithm extends MultiObjectiveAlgorithm {

	/*
	 * turn on this variable to enable the step counter. It will
	 * count how many steps were taken from a base point to a Pareto point
	 * and report the results in a file specified over the variable filename
	 */
	final boolean doEvaluation = false;
	
	final String filename;
	
	final long startTime = System.currentTimeMillis();

	private static final Logger logger = Logger.getLogger(CheckpointedGuidedImprovementAlgorithm.class.toString());

	public CheckpointedGuidedImprovementAlgorithm(final String desc, final MultiObjectiveOptions options) {
		super(desc, options);
		this.filename = desc.replace("$", "");
	}

	@Override
	public void multiObjectiveSolve(final MultiObjectiveProblem problem, final SolutionNotifier notifier) {
		// set the bit width
		setBitWidth(problem.getBitWidth());

		// for the evaluation we need a step counter
		this.counter = new StepCounter();

		CheckpointedSolver solver = CheckpointedSolver.solver(getOptions());

		//begin, amongst others, start the timer
		begin();

		final Bounds emptyBounds = new Bounds(problem.getBounds().universe());

		// Throw a dart and get a starting point.
		Solution solution = incrementalSolveFirst(solver, problem.getConstraints(), problem.getBounds(), problem, null);
		solver.checkpoint();

		int stepsToFront = 1;
		// While the current solution is satisfiable try to find a better one.
		while (isSat(solution)) {
			MetricPoint currentValues = null;
			Solution previousSolution = null;

			// Work our way up to the pareto front.
			while (isSat(solution)) {
				currentValues = MetricPoint.measure(solution, problem.getObjectives(), getOptions());
				logger.log(Level.FINE, "Found a solution. At time: {0}, Improving on {1}", new Object[] { Integer.valueOf((int)((System.currentTimeMillis()-startTime)/1000)),  currentValues.values() });

				final Formula improvementConstraints = currentValues.parametrizedImprovementConstraints();
				
				previousSolution = solution;
				solution = incrementalSolveOne(solver, improvementConstraints, emptyBounds, problem, improvementConstraints);
				solver.checkpoint();
				
				stepsToFront += 1;

				counter.countStep();
			}

			// We can't find anything better, so the previous solution is a pareto point.
			foundMetricPoint();
			logger.log(Level.FINE, "Found Pareto point with values: {0}", currentValues.values());

			if (!options.allSolutionsPerPoint()) {
				tell(notifier, previousSolution, currentValues);
			} else {
				// magnifying glass				
				final Collection<Formula> assignmentsConstraints = currentValues.assignmentConstraints();
				assignmentsConstraints.add(problem.getConstraints());
				int solutionsFound = magnifier(Formula.and(assignmentsConstraints), problem.getBounds(), currentValues, notifier);
				logger.log(Level.FINE, "Magnifying glass found {0} solution(s). At time: {1}", new Object[] {Integer.valueOf(solutionsFound), Integer.valueOf((int)((System.currentTimeMillis()-startTime)/1000))});
			}

			while (stepsToFront > 0) {
				solver.rollback();
				stepsToFront -= 1;
			}

			// Find another starting point.
			solution = incrementalSolveOne(solver, currentValues.exclusionConstraint(), emptyBounds, problem, null);
			solver.checkpoint();
      stepsToFront += 1;

			//count this step but first go to new index because it's a new base point
			counter.nextIndex();
			counter.countStep();
		}
		end(notifier);
		debugWriteStatistics();	
	}

	protected Solution incrementalSolveOne(final CheckpointedSolver solver, final Formula formula, final Bounds bounds, final MultiObjectiveProblem problem, final Formula improvementConstraints) {
		return incrementalSolveOne(solver, formula, bounds, false, problem, improvementConstraints);
	}

	protected Solution incrementalSolveFirst(final CheckpointedSolver solver, final Formula formula, final Bounds bounds, final MultiObjectiveProblem problem, final Formula improvementConstraints) {
		final Solution solution = incrementalSolveOne(solver, formula, bounds, true, problem, improvementConstraints);
		getStats().set(StatKey.CLAUSES, solution.stats().clauses());
		getStats().set(StatKey.VARIABLES, solution.stats().primaryVariables());
		return solution;
	}

	private Solution incrementalSolveOne(CheckpointedSolver solver, final Formula formula, final Bounds bounds, final boolean first, final MultiObjectiveProblem problem, final Formula improvementConstraints) {
		final Solution solution = solver.solve(formula, bounds);
		if (isSat(solution)) {
			getStats().increment(StatKey.REGULAR_SAT_CALL);

			getStats().increment(StatKey.REGULAR_SAT_TIME, solution.stats().translationTime());
			getStats().increment(StatKey.REGULAR_SAT_TIME, solution.stats().solvingTime());
			
			
			getStats().increment(StatKey.REGULAR_SAT_TIME_TRANSLATION, solution.stats().translationTime());
			getStats().increment(StatKey.REGULAR_SAT_TIME_SOLVING, solution.stats().solvingTime());
			
			// Adding Individual instance.
			MetricPoint obtainedValues = MetricPoint.measure(solution, problem.getObjectives(), getOptions());
			this.getStats().addSummaryIndividualCall(StatKey.REGULAR_SAT_CALL, solution.stats().translationTime(), solution.stats().solvingTime(), formula, bounds, first, obtainedValues, improvementConstraints);
		} else {
			getStats().increment(StatKey.REGULAR_UNSAT_CALL);

			getStats().increment(StatKey.REGULAR_UNSAT_TIME, solution.stats().translationTime());
			getStats().increment(StatKey.REGULAR_UNSAT_TIME, solution.stats().solvingTime());

			getStats().increment(StatKey.REGULAR_UNSAT_TIME_TRANSLATION, solution.stats().translationTime());
			getStats().increment(StatKey.REGULAR_UNSAT_TIME_SOLVING, solution.stats().solvingTime());

			this.getStats().addSummaryIndividualCall(StatKey.REGULAR_UNSAT_CALL, solution.stats().translationTime(), solution.stats().solvingTime(), formula, bounds, first, null, improvementConstraints);
		}
		return solution;
	}

	private void debugWriteStatistics(){
		logger.log(Level.FINE, "# Sat Call: {0}", this.getStats().get(StatKey.REGULAR_SAT_CALL));
		logger.log(Level.FINE, "# Unsat Call:  {0}", this.getStats().get( StatKey.REGULAR_UNSAT_CALL));

		logger.log(Level.FINE, "Total Time in Sat Calls: {0}", this.getStats().get(StatKey.REGULAR_SAT_TIME));
		logger.log(Level.FINE, "Total Time in Sat Calls Solving: {0}", this.getStats().get(StatKey.REGULAR_SAT_TIME_SOLVING));
		logger.log(Level.FINE, "Total Time in Sat Calls Translating: {0}", this.getStats().get(StatKey.REGULAR_SAT_TIME_TRANSLATION));

		logger.log(Level.FINE, "Total Time in Unsat Calls: {0}", this.getStats().get( StatKey.REGULAR_UNSAT_TIME));
		logger.log(Level.FINE, "Total Time in Unsat Calls Solving: {0}", this.getStats().get( StatKey.REGULAR_UNSAT_TIME_SOLVING));
		logger.log(Level.FINE, "Total Time in Unsat Calls Translating: {0}", this.getStats().get( StatKey.REGULAR_UNSAT_TIME_TRANSLATION));

		logger.log(Level.FINE, "# Magnifier Sat Call: {0}", this.getStats().get(StatKey.MAGNIFIER_SAT_CALL));
		logger.log(Level.FINE, "# Magnifier Unsat Call: {0}", this.getStats().get(StatKey.MAGNIFIER_UNSAT_CALL));
		logger.log(Level.FINE, "Total Time in Magnifier: {0}", this.getStats().get(StatKey.MAGNIFIER_TIME));

	}

}
	
