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

				final Formula improvementConstraints = currentValues.parametrizedImprovementConstraints();
				
				previousSolution = solution;
				solution = incrementalSolveOne(solver, improvementConstraints, emptyBounds, problem, improvementConstraints);

				counter.countStep();
			}

			// We can't find anything better, so the previous solution is a pareto point.
			foundMetricPoint();

			if (!options.allSolutionsPerPoint()) {
				tell(notifier, previousSolution, currentValues);
			} else {
				// magnifying glass				
				final Collection<Formula> assignmentsConstraints = currentValues.assignmentConstraints();
				assignmentsConstraints.add(problem.getConstraints());
				magnifier(Formula.and(assignmentsConstraints), problem.getBounds(), currentValues, notifier);
			}

			// Find another starting point.
			solver.free();
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
		System.out.println("\t # Sat Call: " +  this.getStats().get(StatKey.REGULAR_SAT_CALL));
		System.out.println("\t # Unsat Call:  " +this.getStats().get( StatKey.REGULAR_UNSAT_CALL));		

		System.out.println("\t Total Time in Sat Calls: " +  this.getStats().get(StatKey.REGULAR_SAT_TIME));
		System.out.println("\t Total Time in Sat Calls Solving: " +  this.getStats().get(StatKey.REGULAR_SAT_TIME_SOLVING));
		System.out.println("\t Total Time in Sat Calls Translating: " +  this.getStats().get(StatKey.REGULAR_SAT_TIME_TRANSLATION));		

		System.out.println("\t Total Time in Unsat Calls:  " +this.getStats().get( StatKey.REGULAR_UNSAT_TIME));		
		System.out.println("\t Total Time in Unsat Calls Solving:  " +this.getStats().get( StatKey.REGULAR_UNSAT_TIME_SOLVING));
		System.out.println("\t Total Time in Unsat Calls Translating:  " +this.getStats().get( StatKey.REGULAR_UNSAT_TIME_TRANSLATION));
	}

}
	
