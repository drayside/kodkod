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
import kodkod.multiobjective.statistics.StepCounter;

public final class IncrementalGuidedImprovementAlgorithm extends MultiObjectiveAlgorithm {

	public IncrementalGuidedImprovementAlgorithm(final String desc, final MultiObjectiveOptions options) {
		super(desc, options, Logger.getLogger(IncrementalGuidedImprovementAlgorithm.class.toString()));
	}
	
	@Override
	protected void multiObjectiveSolveImpl(final MultiObjectiveProblem problem, final SolutionNotifier notifier) {
		// set the bit width
		setBitWidth(problem.getBitWidth());

		// for the evaluation we need a step counter
		this.counter = new StepCounter();

		IncrementalSolver solver = IncrementalSolver.solver(getOptions());

		//begin, amongst others, start the timer
		begin();
		
		final List<Formula> exclusionConstraints = new ArrayList<Formula>();
		exclusionConstraints.add(problem.getConstraints());
		
		// Throw a dart and get a starting point.
		Formula constraint = Formula.and(exclusionConstraints);
		Solution solution = solver.solve(constraint, problem.getBounds());
		
		incrementStats(solution, problem, constraint, true, null);
		solveFirstStats(solution);
		counter.countStep();

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
				solution = solver.solve(improvementConstraints, new Bounds(problem.getBounds().universe()));
				incrementStats(solution, problem, improvementConstraints, false, improvementConstraints);

				counter.countStep();
			}

			// We can't find anything better, so the previous solution is a pareto point.
			foundParetoPoint(currentValues);

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
			
			constraint = Formula.and(exclusionConstraints);
			solution = solver.solve(constraint, problem.getBounds());
			incrementStats(solution, problem, constraint, false, null);
			
			//count this step but first go to new index because it's a new base point
			counter.nextIndex();
			counter.countStep();
		}
		logger.log(Level.FINE, "All Pareto points found. At time: {0}", Integer.valueOf((int)(System.currentTimeMillis()-startTime)/1000));

		end(notifier);
		debugWriteStatistics();	
	}
}

