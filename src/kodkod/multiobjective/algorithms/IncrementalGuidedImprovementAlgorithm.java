package kodkod.multiobjective.algorithms;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import kodkod.ast.Formula;
import kodkod.engine.Solution;
import kodkod.engine.IncrementalSolver;
import kodkod.instance.Bounds;
import kodkod.multiobjective.MetricPoint;
import kodkod.multiobjective.MultiObjectiveOptions;
import kodkod.multiobjective.MultiObjectiveProblem;
import kodkod.multiobjective.concurrency.SolutionNotifier;
import kodkod.multiobjective.statistics.StatKey;
import kodkod.multiobjective.statistics.StepCounter;

public final class IncrementalGuidedImprovementAlgorithm extends MultiObjectiveAlgorithm {

	/*
	 * turn on this variable to enable the step counter. It will
	 * count how many steps were taken from a base point to a Pareto point
	 * and report the results in a file specified over the variable filename
	 */
	final boolean doEvaluation = false;
	
	final String filename;
	
	final long startTime = System.currentTimeMillis();

	private static final Logger logger = Logger.getLogger(IncrementalGuidedImprovementAlgorithm.class.toString());

	public IncrementalGuidedImprovementAlgorithm(final String desc, final MultiObjectiveOptions options) {
		super(desc, options);
		this.filename = desc.replace("$", "");
	}

	@Override
	public void multiObjectiveSolve(final MultiObjectiveProblem problem, final SolutionNotifier notifier) {
		// set the bit width
		setBitWidth(problem.getBitWidth());

		// for the evaluation we need a step counter
		this.counter = new StepCounter();

		IncrementalSolver solver = IncrementalSolver.solver(getOptions());

		//begin, amongst others, start the timer
		begin();
		
		final List<Formula> exclusionConstraints = new ArrayList<Formula>();
		exclusionConstraints.add(problem.getConstraints());

		final Bounds emptyBounds = new Bounds(problem.getBounds().universe());

		// Throw a dart and get a starting point.
		Solution solution = incrementalSolveFirst(solver, Formula.and(exclusionConstraints), problem.getBounds(), problem, null);

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

				counter.countStep();
			}

			// We can't find anything better, so the previous solution is a pareto point.
			foundMetricPoint();
			logger.log(Level.FINE, "Found Pareto point with values: {0}", currentValues.values());

      // Free the solver's resources since we will be creating a new solver.
			solver.free();

			if (!options.allSolutionsPerPoint()) {
				tell(notifier, previousSolution, currentValues);
			} else {
				// magnifying glass				
				final Collection<Formula> assignmentsConstraints = currentValues.assignmentConstraints();
				assignmentsConstraints.add(problem.getConstraints());
				int solutionsFound = magnifier(Formula.and(assignmentsConstraints), problem.getBounds(), currentValues, notifier);
				logger.log(Level.FINE, "Magnifying glass found {0} solution(s). At time: {1}", new Object[] {Integer.valueOf(solutionsFound), Integer.valueOf((int)((System.currentTimeMillis()-startTime)/1000))});
			}

			// Find another starting point.
			solver = IncrementalSolver.solver(getOptions());
			exclusionConstraints.add(currentValues.exclusionConstraint());

			solution = incrementalSolveOne(solver, Formula.and(exclusionConstraints), problem.getBounds(), problem, null);

			//count this step but first go to new index because it's a new base point
			counter.nextIndex();
			counter.countStep();
		}
		end(notifier);
		debugWriteStatistics();	
	}

	protected Solution incrementalSolveOne(final IncrementalSolver solver, final Formula formula, final Bounds bounds, final MultiObjectiveProblem problem, final Formula improvementConstraints) {
		return incrementalSolveOne(solver, formula, bounds, false, problem, improvementConstraints);
	}

	protected Solution incrementalSolveFirst(final IncrementalSolver solver, final Formula formula, final Bounds bounds, final MultiObjectiveProblem problem, final Formula improvementConstraints) {
		final Solution solution = incrementalSolveOne(solver, formula, bounds, true, problem, improvementConstraints);
		getStats().set(StatKey.CLAUSES, solution.stats().clauses());
		getStats().set(StatKey.VARIABLES, solution.stats().primaryVariables());
		return solution;
	}

	private Solution incrementalSolveOne(IncrementalSolver solver, final Formula formula, final Bounds bounds, final boolean first, final MultiObjectiveProblem problem, final Formula improvementConstraints) {
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
        StringBuilder sb = new StringBuilder("Solver statistics:\n");
        sb.append("\t# Sat Call: ");
        sb.append(this.getStats().get(StatKey.REGULAR_SAT_CALL));
        sb.append("\n");
        sb.append("\t# Unsat Call: ");
        sb.append(this.getStats().get(StatKey.REGULAR_UNSAT_CALL));
        sb.append("\n");

        sb.append("\t# Total Time in Sat Calls: ");
        sb.append(this.getStats().get(StatKey.REGULAR_SAT_TIME));
        sb.append("\n");
        sb.append("\t# Total Time in Sat Calls Solving: ");
        sb.append(this.getStats().get(StatKey.REGULAR_SAT_TIME_SOLVING));
        sb.append("\n");
        sb.append("\t# Total Time in Sat Calls Translating: ");
        sb.append(this.getStats().get(StatKey.REGULAR_SAT_TIME_TRANSLATION));
        sb.append("\n");

        sb.append("\t# Total Time in Unsat Calls: ");
        sb.append(this.getStats().get(StatKey.REGULAR_UNSAT_TIME));
        sb.append("\n");
        sb.append("\t# Total Time in Unsat Calls Solving: ");
        sb.append(this.getStats().get(StatKey.REGULAR_UNSAT_TIME_SOLVING));
        sb.append("\n");
        sb.append("\t# Total Time in Unsat Calls Translating: ");
        sb.append(this.getStats().get(StatKey.REGULAR_UNSAT_TIME_TRANSLATION));
        sb.append("\n");

        sb.append("\t# Magnifier Sat Call: ");
        sb.append(this.getStats().get(StatKey.MAGNIFIER_SAT_CALL));
        sb.append("\n");
        sb.append("\t# Magnifier Unsat Call: ");
        sb.append(this.getStats().get(StatKey.MAGNIFIER_UNSAT_CALL));
        sb.append("\n");
        sb.append("\t# Total Time in Magnifier: ");
        sb.append(this.getStats().get(StatKey.MAGNIFIER_TIME));

        logger.log(Level.FINE, sb.toString());
	}

}
	
