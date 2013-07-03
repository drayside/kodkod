package kodkod.multiobjective.algorithms;

import java.util.Iterator;
import kodkod.ast.Formula;
import kodkod.engine.Solution;
import kodkod.engine.Solver;
import kodkod.engine.config.Options;
import kodkod.engine.satlab.SATFactory;
import kodkod.instance.Bounds;
import kodkod.multiobjective.MeasuredSolution;
import kodkod.multiobjective.MetricPoint;
import kodkod.multiobjective.MultiObjectiveOptions;
import kodkod.multiobjective.MultiObjectiveProblem;
import kodkod.multiobjective.concurrency.SolutionNotifier;
import kodkod.multiobjective.statistics.StatKey;
import kodkod.multiobjective.statistics.Stats;
import kodkod.multiobjective.statistics.StepCounter;

public abstract class MultiObjectiveAlgorithm {

	private final String desc;
	private final Solver solver;
	private final Stats stats;
	
	protected StepCounter counter;
	protected final MultiObjectiveOptions options;

	public MultiObjectiveAlgorithm(final String desc, final MultiObjectiveOptions options) {
		this.desc = desc;
		this.solver = new Solver(options.getKodkodOptions());
		this.stats = new Stats(this.getClass().getName(), desc);
		this.options = options;
	}

	public Options getOptions() {
		return solver.options();
	}
	
	public StepCounter getCountCallsOnEachMovementToParetoFront(){
		return this.counter;
	}

	public void setCNFOutputFile(final String filePath) {
		final Options options = solver.options();
		final String executable = null;
		final String tempInput = filePath;
		final String tempOutput = "";
		final String cnfOptions = "";
		final SATFactory cnfSolver = SATFactory.externalFactory(executable, tempInput, tempOutput, cnfOptions);
		options.setSolver(cnfSolver);		
	}
	
	public void setSymmetryBreaking(int value) {
		final Options options = solver.options();
		options.setSymmetryBreaking(value);
	}

	public void setBitWidth(final int bitWidth) {
		solver.options().setBitwidth(bitWidth);
	}

	public static boolean isSat(final Solution solution) {
		return solution.outcome().equals(Solution.Outcome.SATISFIABLE) || solution.outcome().equals(Solution.Outcome.TRIVIALLY_SATISFIABLE);
	}
	
	protected void foundMetricPoint() {
		stats.increment(StatKey.OPTIMAL_METRIC_POINTS);
	}

	protected void begin() {
		stats.begin();
	}
	
	protected void end(final SolutionNotifier notifier) {
		stats.end();
		stats.checkForValidFinalState();
		notifier.done();
	}
	
	protected Solution solveOne(final Formula formula, final Bounds bounds, final MultiObjectiveProblem problem, final Formula improvementConstraints) {
		return solveOne(formula, bounds, false, problem, improvementConstraints);
	}

	protected Solution solveFirst(final Formula formula, final Bounds bounds, final MultiObjectiveProblem problem, final Formula improvementConstraints) {
		final Solution solution = solveOne(formula, bounds, true, problem, improvementConstraints);
		stats.set(StatKey.CLAUSES, solution.stats().clauses());
		stats.set(StatKey.VARIABLES, solution.stats().primaryVariables());
		return solution;
	}

	private Solution solveOne(final Formula formula, final Bounds bounds, final boolean first, final MultiObjectiveProblem problem, final Formula improvementConstraints) {
		final Solution solution = solver.solve(formula, bounds);
		if (isSat(solution)) {
			stats.increment(StatKey.REGULAR_SAT_CALL);

			stats.increment(StatKey.REGULAR_SAT_TIME, solution.stats().translationTime());
			stats.increment(StatKey.REGULAR_SAT_TIME, solution.stats().solvingTime());
			
			
			stats.increment(StatKey.REGULAR_SAT_TIME_TRANSLATION, solution.stats().translationTime());
			stats.increment(StatKey.REGULAR_SAT_TIME_SOLVING, solution.stats().solvingTime());
			
			// Adding Individual instance.
			MetricPoint obtainedValues = MetricPoint.measure(solution, problem.getObjectives(), getOptions());
			this.stats.addSummaryIndividualCall(StatKey.REGULAR_SAT_CALL, solution.stats().translationTime(), solution.stats().solvingTime(), formula, bounds, first, obtainedValues, improvementConstraints);
		} else {
			stats.increment(StatKey.REGULAR_UNSAT_CALL);

			stats.increment(StatKey.REGULAR_UNSAT_TIME, solution.stats().translationTime());
			stats.increment(StatKey.REGULAR_UNSAT_TIME, solution.stats().solvingTime());

			stats.increment(StatKey.REGULAR_UNSAT_TIME_TRANSLATION, solution.stats().translationTime());
			stats.increment(StatKey.REGULAR_UNSAT_TIME_SOLVING, solution.stats().solvingTime());

			this.stats.addSummaryIndividualCall(StatKey.REGULAR_UNSAT_CALL, solution.stats().translationTime(), solution.stats().solvingTime(), formula, bounds, first, null, improvementConstraints);
		}
		return solution;
	}
	
	// Returns an int specifying the number of solutions found at the Pareto point
	protected int magnifier(final Formula formula, final Bounds bounds, final MetricPoint metricPoint, final SolutionNotifier notifier) {
		boolean isFirst = true;
		int numberSolutions = 0;
		for (final Iterator<Solution> i = solver.solveAll(formula, bounds); i.hasNext(); ) {
			final Solution solution = i.next();
			if (isSat(solution)) {
				stats.increment(StatKey.MAGNIFIER_SAT_CALL);
				numberSolutions++;
				tell(notifier, solution, metricPoint);
			} else {
				stats.increment(StatKey.MAGNIFIER_UNSAT_CALL);
			}
			if (isFirst) {
				// we only need to translate once here, so only count that once
				isFirst = false;
				stats.increment(StatKey.MAGNIFIER_TIME, solution.stats().translationTime());
			}
			stats.increment(StatKey.MAGNIFIER_TIME, solution.stats().solvingTime());
		}
		return numberSolutions;
	}

	protected void tell(final SolutionNotifier notifier, final Solution solution, final MetricPoint metricPoint) {
		stats.increment(StatKey.OPTIMAL_SOLNS);
		notifier.tell(solution, metricPoint);
	}

	protected void tell(final SolutionNotifier notifier, final MeasuredSolution solution) {
		stats.increment(StatKey.OPTIMAL_SOLNS);
		notifier.tell(solution);
	}

	/**
	 * Asynchronous solve.
	 */
	public abstract void multiObjectiveSolve(final MultiObjectiveProblem problem, SolutionNotifier notifier);
	
	public Stats getStats() {
		return stats;
	}

	public String getDesc() {
		return desc;
	}

	@Override
	public String toString() {
		return "MultiObjectiveSolver [stats=" + stats + "]";
	}
}
