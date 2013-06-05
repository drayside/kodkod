package kodkod.multiobjective.api;

import java.util.Iterator;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import kodkod.ast.Formula;
import kodkod.engine.Solution;
import kodkod.engine.Solver;
import kodkod.engine.config.Options;
import kodkod.engine.satlab.SATFactory;
import kodkod.instance.Bounds;
import kodkod.multiobjective.Poison;

public abstract class MultiObjectiveSolver {

	public final String desc;
	
	private final Solver solver;
	
	private final Stats stats;

	private final ExecutorService executor;
	
	
	public MultiObjectiveSolver(final String desc, final boolean parallelize) {
		this.desc = desc;
		this.solver = new Solver();
		this.stats = new Stats(this.getClass().getName(), desc);
		setDefaultOptions();
		
		if (parallelize) {
			// start up an ExecutorService to run tasks in other threads
			final int cores = Runtime.getRuntime().availableProcessors();
			this.executor = Executors.newFixedThreadPool(cores);	
		} else {
			// all in this thread
			this.executor = new DummyExecutorService();
		}
	}
	
	public MultiObjectiveSolver(final String desc) {
		this(desc, false);
	}

	public Options getOptions() {
		return solver.options();
	}
	
	public void setDefaultOptions() {
		final Options options = solver.options();
		options.setSolver(SATFactory.MiniSat);
		options.setBitwidth(32);	
		options.setSymmetryBreaking(0);

//		// uncomment this to save the CNF file
//		final String executable = null;
//		final String tempInput = "/tmp/moolloy.cnf";
//		final String tempOutput = "";
//		final String cnfOptions = "";
//		final SATFactory cnfSolver = SATFactory.externalFactory(executable, tempInput, tempOutput, cnfOptions);
//		options.setSolver(cnfSolver);
	}
	
	public void setSymmetryBreaking(int value) {
		final Options options = solver.options();
		options.setSymmetryBreaking(value);
	}
	
	public void setBitWidth(final int bitWidth) {
		solver.options().setBitwidth(bitWidth);
	}

	public static boolean isSat(final Solution s) {
		return s.outcome().equals(Solution.Outcome.SATISFIABLE) || s.outcome().equals(Solution.Outcome.TRIVIALLY_SATISFIABLE);
	}
	
	protected void foundMetricPoint() {
		stats.increment(StatKey.OPTIMAL_METRIC_POINTS);
	}

	protected void begin() {
		stats.begin();
	}
	
	protected void end(final SolutionNotifier n) {
		// first wait for asynchronous tasks to complete
		executor.shutdown();
		try {
			executor.awaitTermination(30, TimeUnit.MINUTES);
			//System.out.println("executor isTerminated for " + getClass().getSimpleName() + " : " + executor.isTerminated() + " " + stats.get(StatKey.OPTIMAL_METRIC_POINTS));
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
		// ok, now we're really done
		stats.end();
		assert stats.isValidFinalState();
		n.done();
	}
	
	protected Solution solveOne(final Formula f, final Bounds b, final MultiObjectiveProblem p, final Formula ImprovementConstraints) {
		return solveOne(f, b, false, p, ImprovementConstraints);
	}

	protected Solution solveFirst(final Formula f, final Bounds b, final MultiObjectiveProblem p, final Formula ImprovementConstraints) {
		final Solution s = solveOne(f, b, true, p, ImprovementConstraints);
		stats.set(StatKey.CLAUSES, s.stats().clauses());
		stats.set(StatKey.VARIABLES, s.stats().primaryVariables());
		return s;
	}

	private Solution solveOne(final Formula f, final Bounds b, final boolean first, final MultiObjectiveProblem p, final Formula ImprovementConstraints) {
		final Solution soln = solver.solve(f, b);
		if (isSat(soln)) {
			stats.increment(StatKey.REGULAR_SAT_CALL);

			stats.increment(StatKey.REGULAR_SAT_TIME, soln.stats().translationTime());
			stats.increment(StatKey.REGULAR_SAT_TIME, soln.stats().solvingTime());
			
			
			stats.increment(StatKey.REGULAR_SAT_TIME_TRANSLATION, soln.stats().translationTime());
			stats.increment(StatKey.REGULAR_SAT_TIME_SOLVING, soln.stats().solvingTime());
			
			// Adding Individual instance.
			MetricPoint obtainedValues = MetricPoint.measure(soln, p.objectives, getOptions());
			this.stats.addSummaryIndividualCall(StatKey.REGULAR_SAT_CALL, soln.stats().translationTime(), soln.stats().solvingTime(), f, b, first, obtainedValues, ImprovementConstraints);
		} else {
			stats.increment(StatKey.REGULAR_UNSAT_CALL);

			stats.increment(StatKey.REGULAR_UNSAT_TIME, soln.stats().translationTime());
			stats.increment(StatKey.REGULAR_UNSAT_TIME, soln.stats().solvingTime());

			stats.increment(StatKey.REGULAR_UNSAT_TIME_TRANSLATION, soln.stats().translationTime());
			stats.increment(StatKey.REGULAR_UNSAT_TIME_SOLVING, soln.stats().solvingTime());

			this.stats.addSummaryIndividualCall(StatKey.REGULAR_UNSAT_CALL, soln.stats().translationTime(), soln.stats().solvingTime(), f, b, first, null, ImprovementConstraints);
		}
		return soln;
	}
	
	protected void magnifier(final Formula f, final Bounds b, final MetricPoint v, final SolutionNotifier n) {
		execute(new Runnable(){
			@Override
			public void run() {
				boolean isFirst = true;
				for (final Iterator<Solution> i = solver.solveAll(f, b); i.hasNext(); ) {
					final Solution s = i.next();
					if (isSat(s)) {
						stats.increment(StatKey.MAGNIFIER_SAT_CALL);
						tell(n, s, v);
					} else {
						stats.increment(StatKey.MAGNIFIER_UNSAT_CALL);
					}
					if (isFirst) {
						// we only need to translate once here, so only count that once
						isFirst = false;
						stats.increment(StatKey.MAGNIFIER_TIME, s.stats().translationTime());
					}
					stats.increment(StatKey.MAGNIFIER_TIME, s.stats().solvingTime());
				}
			}});
	}
	
	protected void execute(final Runnable r) {
		executor.execute(r);
	}

	protected void tell(final SolutionNotifier n, final Solution s, final MetricPoint v) {
		stats.increment(StatKey.OPTIMAL_SOLNS);
		n.tell(s, v);
	}

	protected void tell(final SolutionNotifier n, final MeasuredSolution s) {
		stats.increment(StatKey.OPTIMAL_SOLNS);
		n.tell(s);
	}

	
	/**
	 * Asynchronous solve with magnifying glass.
	 */
	public final void moosolve(final MultiObjectiveProblem p, final SolutionNotifier n) {
		moosolve(p, n, true);
	}

	/**
	 * Asynchronous solve.
	 */
	public abstract void moosolve(final MultiObjectiveProblem p, SolutionNotifier n, boolean magnifyingGlass);

	/**
	 * Synchronous solve where we are not interested in the actual solutions -- just the runtime.
	 */
	public final Stats moosolve(final MultiObjectiveProblem p, final boolean magnifyingGlass) {
		final BlockingQueue<MeasuredSolution> q = new LinkedBlockingQueue<MeasuredSolution>();
		final SolutionNotifier notifier = new BlockingQueueSolutionNotifier(q);

		// set the bit width
		setBitWidth(p.bitWidth);
		
		// solve
		final Thread thread = new Thread() {
			@Override
			public void run() {
				moosolve(p, notifier, magnifyingGlass);
			}
		};
		thread.start();

		// drain the solutions
		Object soln = null;
		do {
			try {
				soln = q.take();
			} catch (InterruptedException e1) {
				// just continue on ... we're just trying to drain the queue here ...
				e1.printStackTrace();
			}
		} while (!Poison.PILL.equals(soln));

		// all done.
		return getStats();
	}

	
	public Stats getStats() {
		return stats;
	}
	
}
