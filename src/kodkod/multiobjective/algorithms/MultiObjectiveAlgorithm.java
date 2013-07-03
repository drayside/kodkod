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
	
	public abstract void multiObjectiveSolve(final MultiObjectiveProblem problem, SolutionNotifier notifier);

	public Options getOptions() {
		return solver.options();
	}
	
	public StepCounter getCountCallsOnEachMovementToParetoFront(){
		return this.counter;
	}

	protected void setCNFOutputFile(final String filePath) {
		final Options options = solver.options();
		final String executable = null;
		final String tempInput = filePath;
		final String tempOutput = "";
		final String cnfOptions = "";
		final SATFactory cnfSolver = SATFactory.externalFactory(executable, tempInput, tempOutput, cnfOptions);
		options.setSolver(cnfSolver);		
	}
	
	protected void setSymmetryBreaking(int value) {
		final Options options = solver.options();
		options.setSymmetryBreaking(value);
	}

	protected void setBitWidth(final int bitWidth) {
		solver.options().setBitwidth(bitWidth);
	}

	public static boolean isSat(final Solution solution) {
		return solution.outcome().equals(Solution.Outcome.SATISFIABLE) || solution.outcome().equals(Solution.Outcome.TRIVIALLY_SATISFIABLE);
	}
	
	protected void foundMetricPoint(MetricPoint metricpoint) {
		stats.increment(StatKey.OPTIMAL_METRIC_POINTS);
		System.out.println("Found metric point with values: " + metricpoint.values());
	}

	protected void begin() {
		stats.begin();
	}
	
	protected void end(final SolutionNotifier notifier) {
		stats.end();
		stats.checkForValidFinalState();
		notifier.done();
	}
	
	protected void magnifier(final Formula formula, final Bounds bounds, final MetricPoint metricPoint, final SolutionNotifier notifier) {
		boolean isFirst = true;
		for (final Iterator<Solution> i = solver.solveAll(formula, bounds); i.hasNext(); ) {
			final Solution solution = i.next();
			if (isSat(solution)) {
				stats.increment(StatKey.MAGNIFIER_SAT_CALL);
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
	
	public Solver getSolver() {
		return solver;
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
		System.out.println("\t # Sat Call: " +  this.getStats().get(StatKey.REGULAR_SAT_CALL));
		System.out.println("\t # Unsat Call:  " +this.getStats().get( StatKey.REGULAR_UNSAT_CALL));		

		System.out.println("\t Total Time in Sat Calls: " +  this.getStats().get(StatKey.REGULAR_SAT_TIME));
		System.out.println("\t Total Time in Sat Calls Solving: " +  this.getStats().get(StatKey.REGULAR_SAT_TIME_SOLVING));
		System.out.println("\t Total Time in Sat Calls Translating: " +  this.getStats().get(StatKey.REGULAR_SAT_TIME_TRANSLATION));		

		System.out.println("\t Total Time in Unsat Calls:  " +this.getStats().get( StatKey.REGULAR_UNSAT_TIME));		
		System.out.println("\t Total Time in Unsat Calls Solving:  " +this.getStats().get( StatKey.REGULAR_UNSAT_TIME_SOLVING));
		System.out.println("\t Total Time in Unsat Calls Translating:  " +this.getStats().get( StatKey.REGULAR_UNSAT_TIME_TRANSLATION));
	}
	
	@Override
	public String toString() {
		return "MultiObjectiveSolver [stats=" + stats + "]";
	}
}
