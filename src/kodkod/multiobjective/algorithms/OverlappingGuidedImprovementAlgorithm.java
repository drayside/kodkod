package kodkod.multiobjective.algorithms;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Queue;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.logging.Level;
import java.util.logging.Logger;

import kodkod.ast.Formula;
import kodkod.engine.IncrementalSolver;
import kodkod.engine.Solution;
import kodkod.engine.Solver;
import kodkod.instance.Bounds;
import kodkod.multiobjective.MetricPoint;
import kodkod.multiobjective.MultiObjectiveOptions;
import kodkod.multiobjective.MultiObjectiveProblem;
import kodkod.multiobjective.concurrency.SolutionDeduplicator;
import kodkod.multiobjective.concurrency.SolutionNotifier;
import kodkod.multiobjective.statistics.StepCounter;

public class OverlappingGuidedImprovementAlgorithm extends MultiObjectiveAlgorithm{

    private final Queue<Solver> magnifyingGlassSolverPool;
    private final SolutionDeduplicator paretoPointDeduplicator;
    private Formula initialPointConstraint;

    public OverlappingGuidedImprovementAlgorithm(String desc, MultiObjectiveOptions options) {
        super(desc, options, Logger.getLogger(OverlappingGuidedImprovementAlgorithm.class.toString()));

        magnifyingGlassSolverPool = new ConcurrentLinkedQueue<Solver>();
        magnifyingGlassSolverPool.add(getSolver());
        paretoPointDeduplicator = new SolutionDeduplicator();
        initialPointConstraint = Formula.constant(true);
    }

    private Solver getMagnifyingGlassSolver() {
        Solver solverToReturn = magnifyingGlassSolverPool.poll();
        if( solverToReturn == null ) {
            solverToReturn = new Solver(options.getKodkodOptions());
        }
        return solverToReturn;
    }

    private void putBackMagnifyingGlassSolver(Solver solver) {
        magnifyingGlassSolverPool.add(solver);
    }

    private class SolverSolutionPair {
        public final IncrementalSolver solver;
        public final Solution solution;

        public SolverSolutionPair( IncrementalSolver solver, Solution solution ) {
            this.solver = solver;
            this.solution = solution;
        }
    }

    /*
     * Creates a new instance of incremental solver and returns a unique starting point. This method is NOT thread-safe.
     */
    private SolverSolutionPair getNewStartingPoint(final MultiObjectiveProblem problem) {
        IncrementalSolver solver = IncrementalSolver.solver(getOptions());
        final List<Formula> problemExclusionConstraints = new ArrayList<Formula>();
        problemExclusionConstraints.add(problem.getConstraints());
        Formula problemExclusionConstraint = Formula.and(problemExclusionConstraints);

        // Throw a dart and get a starting point.
        Solution solution = solver.solve(problemExclusionConstraint.and(initialPointConstraint), problem.getBounds());

        incrementStats(solution, problem, problemExclusionConstraint, true, null);
        solveFirstStats(solution);

        if( !isSat(solution) ) {
            return null;
        } else {
            initialPointConstraint = initialPointConstraint.and(MetricPoint.measure(solution, problem.getObjectives(), getOptions()).exclusionConstraint());
            return new SolverSolutionPair(solver, solution);
        }
    }

    @Override
    public void multiObjectiveSolveImpl(final MultiObjectiveProblem problem, final SolutionNotifier notifier) {

        // set the bit width
        setBitWidth(problem.getBitWidth());

        // for the evaluation we need a step counter
        this.counter = new StepCounter();

        int numberOfThreads = Math.min(8, Runtime.getRuntime().availableProcessors());

        final ExecutorService executorService = Executors.newFixedThreadPool(numberOfThreads);

        final ConcurrentLinkedQueue<Future<?>> waitQueue = new ConcurrentLinkedQueue<Future<?>>();

        for( int i=0; i<numberOfThreads; i++ ) {
            SolverSolutionPair solverSolutionPair = getNewStartingPoint(problem);

            if( solverSolutionPair == null ) {
                break;
            }

            waitQueue.add( executorService.submit( new SolverSubtask( solverSolutionPair.solver, problem, solverSolutionPair.solution, notifier, executorService, waitQueue) ) );
        }

        try {
            // There is only one consumer, so if the following condition is satisfied, it is guaranteed to be non-empty.
            while( !waitQueue.isEmpty() ) {
                // get() method is blocking, so we essentially block until the last SolverSubtask is done.
                waitQueue.remove().get();
            }
        } catch (InterruptedException e) {
            e.printStackTrace();
            throw new RuntimeException(e);
        } catch (ExecutionException e) {
            e.printStackTrace();
            throw new RuntimeException(e);
        }

        logger.log(Level.FINE, "All Pareto points found. At time: {0}", Integer.valueOf((int)(System.currentTimeMillis()-startTime)/1000));

        executorService.shutdown();


        end(notifier);
        debugWriteStatistics();
    }

    private class MagnifyingGlassSubtask implements Runnable {
        private final Formula formula;
        private final Bounds bounds;
        private final MetricPoint metricPoint;
        private final SolutionNotifier notifier;

        public MagnifyingGlassSubtask(final Formula formula, final Bounds bounds, final MetricPoint metricPoint, final SolutionNotifier notifier) {
            this.formula = formula;
            this.bounds = bounds;
            this.metricPoint = metricPoint;
            this.notifier = notifier;
        }

        @Override
        public void run() {

            Solver solverToUse = getMagnifyingGlassSolver();
            int solutionsFound = magnifier(formula, bounds, metricPoint, notifier, solverToUse);
            putBackMagnifyingGlassSolver(solverToUse);

            logger.log(Level.FINE, "Magnifying glass found {0} solution(s). At time: {1}", new Object[] {Integer.valueOf(solutionsFound), Integer.valueOf((int)((System.currentTimeMillis()-startTime)/1000))});
        }
    }

    private class SolverSubtask implements Runnable {

        private final MultiObjectiveProblem problem;
        private final SolutionNotifier notifier;
        private final ExecutorService executorService;
        private final Queue<Future<?>> waitQueue;
        private final Solution initialSolution;
        private IncrementalSolver solver;

        SolverSubtask(final IncrementalSolver solver, final MultiObjectiveProblem problem, final Solution initialSolution, final SolutionNotifier notifier, final ExecutorService executorService, final Queue<Future<?>> waitQueue) {
            this.problem = problem;
            this.notifier = notifier;
            this.executorService = executorService;
            this.waitQueue = waitQueue;
            this.initialSolution = initialSolution;
            this.solver = solver;
        }

        @Override
        public void run() {
            final List<Formula> problemExclusionConstraints = new ArrayList<Formula>();
            problemExclusionConstraints.add(problem.getConstraints());
            final Formula problemExclusionConstraint = Formula.and(problemExclusionConstraints);

            // Use the solution that has been passed in.
            Solution solution = initialSolution;

            // While the current solution is satisfiable try to find a better one.
            while (isSat(solution)) {
                MetricPoint currentValues = null;
                Solution previousSolution = null;

                // Temporary step counting for parallel GIAs.
                int step = 0;

                // Work our way up to the pareto front.
                while (isSat(solution)) {
                    currentValues = MetricPoint.measure(solution, problem.getObjectives(), getOptions());
                    logger.log(Level.FINE, "Found a solution. At time: {0}, Improving on {1}", new Object[] { Integer.valueOf((int)((System.currentTimeMillis()-startTime)/1000)),  currentValues.values() });

                    final Formula improvementConstraints = currentValues.parametrizedImprovementConstraints();

                    previousSolution = solution;
                    solution = solver.solve(improvementConstraints, new Bounds(problem.getBounds().universe()));
                    incrementStats(solution, problem, improvementConstraints, false, improvementConstraints);
                    step++;
                }

                // We can't find anything better, so the previous solution is a pareto point.
                foundParetoPoint(currentValues);

                // Free the solver's resources since we will be creating a new solver.
                solver.free();

                if( paretoPointDeduplicator.pushNewSolution(currentValues)) {

                    // Temporary step counting for parallel GIAs.
                    logger.log(Level.FINE, "Found a unique pareto point after stepping {0} times: {1}", new Object[] { Integer.valueOf(step), currentValues.values() });

                    if (!options.allSolutionsPerPoint()) {
                        tell(notifier, previousSolution, currentValues);
                    } else {
                        // magnifying glass
                        final Collection<Formula> assignmentsConstraints = currentValues.assignmentConstraints();
                        assignmentsConstraints.add(problem.getConstraints());
                        // The values in currentValues will now be shared with its MagnifyingGlassSubtask. Therefore, these values should not be modified after this point.
                        waitQueue.add( executorService.submit(new MagnifyingGlassSubtask(Formula.and(assignmentsConstraints), problem.getBounds(), currentValues, notifier)));
                    }
                } else {
                    // Temporary step counting for parallel GIAs.
                    logger.log(Level.FINE, "Found a duplicate pareto point after stepping {0} times: {1}", new Object[] { Integer.valueOf(step), currentValues.values() });
                }



                // Find another starting point.
                solver = IncrementalSolver.solver(getOptions());
                Formula exclusionConstraints = Formula.and(paretoPointDeduplicator.getGlobalExclusionConstraint(),problemExclusionConstraint);
                solution = solver.solve(exclusionConstraints, problem.getBounds());
                incrementStats(solution, problem, exclusionConstraints, false, null);
            }
        }

    }
}
