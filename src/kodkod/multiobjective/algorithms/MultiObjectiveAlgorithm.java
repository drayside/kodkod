package kodkod.multiobjective.algorithms;

import java.util.Iterator;
import java.util.logging.Level;
import java.util.logging.Logger;

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
	private final Solver internalSolver;
	private final Stats stats;

	protected final long startTime;
	protected final Logger logger;
	protected StepCounter counter;
	protected final MultiObjectiveOptions options;

	public MultiObjectiveAlgorithm(final String desc, final MultiObjectiveOptions options, Logger logger) {
		this.logger = logger;
		this.desc = desc;
		this.internalSolver = new Solver(options.getKodkodOptions());
		this.stats = new Stats(this.getClass().getName(), desc);
		this.options = options;
		this.startTime = System.currentTimeMillis();
	}	
 
  protected abstract void multiObjectiveSolveImpl(final MultiObjectiveProblem proble, SolutionNotifier notifier);

	public final void multiObjectiveSolve(final MultiObjectiveProblem problem, SolutionNotifier notifier) {
    try {
      multiObjectiveSolveImpl(problem, notifier);
    } catch (Throwable e) {
      notifier.exception(e);
    }
  }

  public Options getOptions() {
		return internalSolver.options();
	}

	public StepCounter getCountCallsOnEachMovementToParetoFront(){
		return this.counter;
	}

	protected void setCNFOutputFile(final String filePath) {
		final Options options = internalSolver.options();
		final String executable = null;
		final String tempInput = filePath;
		final String tempOutput = "";
		final String cnfOptions = "";
		final SATFactory cnfSolver = SATFactory.externalFactory(executable, tempInput, tempOutput, cnfOptions);
		options.setSolver(cnfSolver);
	}

	protected void setSymmetryBreaking(int value) {
		final Options options = internalSolver.options();
		options.setSymmetryBreaking(value);
	}

	protected void setBitWidth(final int bitWidth) {
		internalSolver.options().setBitwidth(bitWidth);
	}

	public static boolean isSat(final Solution solution) {
		return solution.outcome().equals(Solution.Outcome.SATISFIABLE) || solution.outcome().equals(Solution.Outcome.TRIVIALLY_SATISFIABLE);
	}

	protected void foundParetoPoint(MetricPoint metricpoint) {
		stats.increment(StatKey.OPTIMAL_METRIC_POINTS);
		logger.log(Level.FINE, "Found Pareto point with values: {0}", metricpoint.values());
	}

	protected void begin() {
		stats.begin();
	}

	protected void end(final SolutionNotifier notifier) {
		stats.end();
		stats.checkForValidFinalState();
		notifier.done();
	}

	protected int magnifier(final Formula formula, final Bounds bounds, final MetricPoint metricPoint, final SolutionNotifier notifier) {
	    return magnifier(formula, bounds, metricPoint, notifier, internalSolver);
	}

	// Returns an int specifying the number of solutions found at the Pareto point
	protected int magnifier(final Formula formula, final Bounds bounds, final MetricPoint metricPoint, final SolutionNotifier notifier, final Solver solver) {
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

	public Stats getStats() {
		return stats;
	}

	public String getDesc() {
		return desc;
	}

	protected Solver getSolver() {
		return internalSolver;
	}

	/**
	 * Method to increment Stats counters each time a solution is found
	 * - Also adds the summary for the specific call using detailed information about the specific SAT call
	 */
	protected void incrementStats(final Solution solution, final MultiObjectiveProblem problem, final Formula formula, final boolean first, final Formula improvementConstraints){
		if (isSat(solution)) {
			getStats().increment(StatKey.REGULAR_SAT_CALL);

			getStats().increment(StatKey.REGULAR_SAT_TIME, solution.stats().translationTime());
			getStats().increment(StatKey.REGULAR_SAT_TIME, solution.stats().solvingTime());


			getStats().increment(StatKey.REGULAR_SAT_TIME_TRANSLATION, solution.stats().translationTime());
			getStats().increment(StatKey.REGULAR_SAT_TIME_SOLVING, solution.stats().solvingTime());

			// Adding Individual instance.
			MetricPoint obtainedValues = MetricPoint.measure(solution, problem.getObjectives(), getOptions());
			this.getStats().addSummaryIndividualCall(StatKey.REGULAR_SAT_CALL, solution.stats().translationTime(), solution.stats().solvingTime(), formula, problem.getBounds(), first, obtainedValues, improvementConstraints);
		} else {
			getStats().increment(StatKey.REGULAR_UNSAT_CALL);

			getStats().increment(StatKey.REGULAR_UNSAT_TIME, solution.stats().translationTime());
			getStats().increment(StatKey.REGULAR_UNSAT_TIME, solution.stats().solvingTime());

			getStats().increment(StatKey.REGULAR_UNSAT_TIME_TRANSLATION, solution.stats().translationTime());
			getStats().increment(StatKey.REGULAR_UNSAT_TIME_SOLVING, solution.stats().solvingTime());

			this.getStats().addSummaryIndividualCall(StatKey.REGULAR_UNSAT_CALL, solution.stats().translationTime(), solution.stats().solvingTime(), formula, problem.getBounds(), first, null, improvementConstraints);
		}
	}

	protected void solveFirstStats(Solution solution){
		getStats().set(StatKey.CLAUSES, solution.stats().clauses());
		getStats().set(StatKey.VARIABLES, solution.stats().primaryVariables());
	}

	/**
	 * Method to print debug statistics after computing the pareto front at the end of algorithm
	 */
	protected void debugWriteStatistics(){
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

	@Override
	public String toString() {
		return "MultiObjectiveSolver [stats=" + stats + "]";
	}
}
