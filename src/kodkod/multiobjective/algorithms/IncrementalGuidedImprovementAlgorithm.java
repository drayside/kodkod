package kodkod.multiobjective.algorithms;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

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

	public IncrementalGuidedImprovementAlgorithm(final String desc, final MultiObjectiveOptions options) {
		super(desc, options);
		this.filename = desc.replace("$", "");
	}

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
		
		// Throw a dart and get a starting point.
		Solution solution = incrementalSolveFirst(solver, Formula.and(exclusionConstraints), problem, null);
		solveFirstStats(solution);

		// While the current solution is satisfiable try to find a better one.
		while (isSat(solution)) {
			MetricPoint currentValues = null;
			Solution previousSolution = null;

			// Work our way up to the pareto front.
			while (isSat(solution)) {
				currentValues = MetricPoint.measure(solution, problem.getObjectives(), getOptions());

				final Formula improvementConstraints = currentValues.parametrizedImprovementConstraints();
				
				previousSolution = solution;
				solution = incrementalSolveOne(solver, improvementConstraints, problem, improvementConstraints);

				counter.countStep();
			}

			// We can't find anything better, so the previous solution is a pareto point.
			foundMetricPoint(currentValues);

			if (!options.allSolutionsPerPoint()) {
				tell(notifier, previousSolution, currentValues);
			} else {
				// magnifying glass				
				final Collection<Formula> assignmentsConstraints = currentValues.assignmentConstraints();
				assignmentsConstraints.add(problem.getConstraints());
				magnifier(Formula.and(assignmentsConstraints), problem.getBounds(), currentValues, notifier);
			}

			// Find another starting point.
			solver = IncrementalSolver.solver(getOptions());
			exclusionConstraints.add(currentValues.exclusionConstraint());

			solution = incrementalSolveFirst(solver, Formula.and(exclusionConstraints),  problem, null);
			
			//count this step but first go to new index because it's a new base point
			counter.nextIndex();
			counter.countStep();
		}
		end(notifier);
		debugWriteStatistics();	
	}


	protected Solution incrementalSolveFirst(final IncrementalSolver solver, final Formula formula, final MultiObjectiveProblem problem, final Formula improvementConstraints) {
		final Solution solution = solver.solve(formula, problem.getBounds());
		incrementStats(solution, problem, formula, true, improvementConstraints);
		return solution;
	}

	protected Solution incrementalSolveOne(IncrementalSolver solver, final Formula formula, final MultiObjectiveProblem problem, final Formula improvementConstraints) {
		final Solution solution = solver.solve(formula, new Bounds(problem.getBounds().universe()));
		incrementStats(solution, problem, formula, false, improvementConstraints);
		return solution;
	}
}
	
