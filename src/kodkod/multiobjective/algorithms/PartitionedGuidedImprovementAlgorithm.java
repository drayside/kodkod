package kodkod.multiobjective.algorithms;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.CompletionService;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorCompletionService;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.logging.Level;
import java.util.logging.Logger;

import kodkod.ast.Formula;
import kodkod.engine.IncrementalSolver;
import kodkod.engine.Solution;
import kodkod.instance.Bounds;
import kodkod.multiobjective.MetricPoint;
import kodkod.multiobjective.MultiObjectiveOptions;
import kodkod.multiobjective.MultiObjectiveProblem;
import kodkod.multiobjective.Objective;
import kodkod.multiobjective.concurrency.SolutionNotifier;
import kodkod.multiobjective.statistics.StepCounter;

public class PartitionedGuidedImprovementAlgorithm extends MultiObjectiveAlgorithm {

    public PartitionedGuidedImprovementAlgorithm(String desc, MultiObjectiveOptions options) {
        super(desc, options, Logger.getLogger(PartitionedGuidedImprovementAlgorithm.class.toString()));
    }

    @Override
    public void multiObjectiveSolveImpl(MultiObjectiveProblem problem, SolutionNotifier notifier) {
        // set the bit width
        setBitWidth(problem.getBitWidth());

        // for the evaluation we need a step counter
        this.counter = new StepCounter();

        //begin, amongst others, start the timer
        begin();

        final List<Formula> exclusionConstraints = new ArrayList<Formula>();
        exclusionConstraints.add(problem.getConstraints());

        // Throw a dart and get a starting point
        Solution solution = getSolver().solve(problem.getConstraints(), problem.getBounds());
        incrementStats(solution, problem, problem.getConstraints(), true, null);
        solveFirstStats(solution);

        MetricPoint startingValues = MetricPoint.measure(solution, problem.getObjectives(), getOptions());
        logger.log(Level.FINE, "Found a solution. At time: {0}, with values {1}", new Object[] { Integer.valueOf((int)(System.currentTimeMillis()-startTime)/1000), startingValues.values() });

        // Find the boundaries of the search space
        Formula boundaryConstraints = findBoundaries(problem, startingValues);
        exclusionConstraints.add(boundaryConstraints);

        IncrementalSolver solver = IncrementalSolver.solver(getOptions());

        // Restart the search with an IncrementalSolver
        // We want the boundaries and problem constraints in the solver, before incrementally adding improvement constraints
        Formula constraint = Formula.and(exclusionConstraints);
        solution = solver.solve(constraint, problem.getBounds());
        incrementStats(solution, problem, constraint, false, constraint);
        counter.countStep();

        // While the current solution is satisfiable try to find a better one
        while (isSat(solution)) {
            MetricPoint currentValues = null;
            Solution previousSolution = null;

            // Work up to the Pareto front
            while (isSat(solution)) {
                currentValues = MetricPoint.measure(solution, problem.getObjectives(), getOptions());
                logger.log(Level.FINE, "Found a solution. At time: {0}, Improving on {1}", new Object[] { Integer.valueOf((int)(System.currentTimeMillis()-startTime)/1000), currentValues.values() });

                final Formula improvementConstraints = currentValues.parametrizedImprovementConstraints();

                previousSolution = solution;
                solution = solver.solve(improvementConstraints, new Bounds(problem.getBounds().universe()));
                incrementStats(solution, problem, improvementConstraints, false, improvementConstraints);

                counter.countStep();
            }
            foundParetoPoint(currentValues);

            // Free the solver's resources since we will be creating a new solver
            solver.free();

            if (!options.allSolutionsPerPoint()) {
                // no magnifying glass
                // previous solution was on the pareto front: report it
                tell(notifier, previousSolution, currentValues);
            } else {
                // magnifying glass
                final Collection<Formula> assignmentsConstraints = currentValues.assignmentConstraints();
                assignmentsConstraints.add(problem.getConstraints());
                int solutionsFound = magnifier(Formula.and(assignmentsConstraints), problem.getBounds(), currentValues, notifier);
                logger.log(Level.FINE, "Magnifying glass found {0} solution(s). At time: {1}", new Object[] {Integer.valueOf(solutionsFound), Integer.valueOf((int)((System.currentTimeMillis()-startTime)/1000))});
            }

            // Find another starting point
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

    private Formula findBoundaries(MultiObjectiveProblem problem, MetricPoint startingValues) {
        List<Formula> boundaries = new ArrayList<Formula>(problem.getObjectives().size());

        // Disable MetricPoint logger temporarily
        // Multiple threads will be calling the logger, so the output won't make sense
        Logger metricPointLogger = Logger.getLogger(MetricPoint.class.toString());
        Level metricPointLevel = metricPointLogger.getLevel();
        metricPointLogger.setLevel(Level.INFO);

        // Create a thread pool to execute the tasks
        // Number of threads is MIN(user value, # cores, # objectives)
        // TODO: Make "user value" configurable
        int numObjectives = problem.getObjectives().size();
        int numThreads = Math.min(8, Math.min(Runtime.getRuntime().availableProcessors(), numObjectives));
        ExecutorService threadPool = Executors.newFixedThreadPool(numThreads);
        CompletionService<Formula> ecs = new ExecutorCompletionService<Formula>(threadPool);

        logger.log(Level.FINE, "Starting thread pool with {0} threads and {1} jobs", new Object[] { numThreads, numObjectives });

        // Create new BoundaryFinder tasks for each objective
        for (final Objective objective : problem.getObjectives()) {
            ecs.submit(new BoundaryFinder(problem, objective, startingValues));
        }

        // take() returns a Future<Formula> if one exists; blocks otherwise
        // Once we have the Formula result, add it to our list
        for (int i = 0; i < numObjectives; i++) {
            Formula result;
            try {
                result = ecs.take().get();
            } catch (InterruptedException | ExecutionException e) {
                e.printStackTrace();
                throw new RuntimeException();
            }
            boundaries.add(result);
        }

        // Now we can shutdown the threadPool
        threadPool.shutdown();

        // Re-enable MetricPoint logging
        metricPointLogger.setLevel(metricPointLevel);

        StringBuilder sb = new StringBuilder("All boundaries found. At time: ");
        sb.append(Integer.valueOf((int)(System.currentTimeMillis()-startTime)/1000));
        sb.append(", Boundaries are conjunction of ");
        for (Formula boundary : boundaries) {
            sb.append("\n\t");
            sb.append(boundary);
        }
        logger.log(Level.FINE, sb.toString());

        return Formula.and(boundaries);
    }

    // A Callable is like a Runnable, but it returns a result
    // In this case, we want to return the Formula representing the boundary for the given objective
    private class BoundaryFinder implements Callable<Formula> {

        private final MultiObjectiveProblem problem;
        private final Objective objective;
        private MetricPoint currentValues;

        BoundaryFinder(MultiObjectiveProblem problem, Objective objective, MetricPoint startingValues) {
            this.problem = problem;
            this.objective = objective;
            this.currentValues = startingValues;
        }

        @Override
        public Formula call() throws Exception {
            IncrementalSolver incrementalSolver = IncrementalSolver.solver(getOptions());
            Formula boundaryConstraint = currentValues.objectiveImprovementConstraint(objective);

            Formula constraint = problem.getConstraints().and(boundaryConstraint);
            Solution solution = incrementalSolver.solve(constraint, problem.getBounds());
            incrementStats(solution, problem, constraint, false, null);

            // Work up to the boundary of this metric
            while (isSat(solution)) {
                currentValues = MetricPoint.measure(solution, problem.getObjectives(), getOptions());
                boundaryConstraint = currentValues.objectiveImprovementConstraint(objective);
                solution = incrementalSolver.solve(boundaryConstraint, new Bounds(problem.getBounds().universe()));
                incrementStats(solution, problem, constraint, false, null);
            }
            incrementalSolver.free();
            return currentValues.boundaryConstraint(objective);
        }

    }
}
