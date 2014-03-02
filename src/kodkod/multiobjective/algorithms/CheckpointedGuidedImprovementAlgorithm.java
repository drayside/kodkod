package kodkod.multiobjective.algorithms;

import java.util.Collection;
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
	public CheckpointedGuidedImprovementAlgorithm(final String desc, final MultiObjectiveOptions options) {
		super(desc, options, Logger.getLogger(CheckpointedGuidedImprovementAlgorithm.class.toString()));
	}

	@Override
	protected void multiObjectiveSolveImpl(final MultiObjectiveProblem problem, final SolutionNotifier notifier) {
		// set the bit width
		setBitWidth(problem.getBitWidth());

		// for the evaluation we need a step counter
		this.counter = new StepCounter();

		CheckpointedSolver solver = CheckpointedSolver.solver(getOptions());

		//begin, amongst others, start the timer
		begin();

		final Bounds emptyBounds = new Bounds(problem.getBounds().universe());

		// Throw a dart and get a starting point.
		Solution solution = solver.solve(problem.getConstraints(), problem.getBounds());

		incrementStats(solution, problem, problem.getConstraints(), true, null);
		solveFirstStats(solution);
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
				solution = solver.solve(improvementConstraints, emptyBounds);
				incrementStats(solution, problem, improvementConstraints, false, improvementConstraints);
				solver.checkpoint();
				
				stepsToFront += 1;

				counter.countStep();
			}

			// We can't find anything better, so the previous solution is a pareto point.
			foundParetoPoint(currentValues);
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
			solution = solver.solve(currentValues.exclusionConstraint(), emptyBounds);
			incrementStats(solution, problem, currentValues.exclusionConstraint(), false, null);
			solver.checkpoint();
			stepsToFront += 1;

			//count this step but first go to new index because it's a new base point
			counter.nextIndex();
			counter.countStep();
		}
		end(notifier);
		debugWriteStatistics();	
	}
}
	
