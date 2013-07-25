package kodkod.multiobjective.algorithms;

import java.util.ArrayList;
import java.util.BitSet;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.Callable;
import java.util.concurrent.CompletionService;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorCompletionService;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.LinkedBlockingQueue;
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
import kodkod.multiobjective.statistics.StatKey;
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

        // Disable MetricPoint logger temporarily
        // Multiple threads will be calling the logger, so the output won't make sense
        Logger metricPointLogger = Logger.getLogger(MetricPoint.class.toString());
        Level metricPointLevel = metricPointLogger.getLevel();
        metricPointLogger.setLevel(Level.INFO);

        // Skip boundary finding if there's only one objective
        if (problem.getObjectives().size() > 1) {
            // Throw a dart and get a starting point
            Solution solution = getSolver().solve(problem.getConstraints(), problem.getBounds());
            incrementStats(solution, problem, problem.getConstraints(), true, null);
            solveFirstStats(solution);

            MetricPoint startingValues = MetricPoint.measure(solution, problem.getObjectives(), getOptions());
            logger.log(Level.FINE, "Found a solution. At time: {0}, with values {1}", new Object[] { Integer.valueOf((int)(System.currentTimeMillis()-startTime)/1000), startingValues.values() });

            // Find the boundaries of the search space
            Formula boundaryConstraints = findBoundaries(problem, startingValues);
            exclusionConstraints.add(boundaryConstraints);
        }

        // Work up to a single Pareto point
        // If there is only one objective, this is the only Pareto point
        // For more objectives, we use this Pareto point to split the search space
        IncrementalSolver solver = IncrementalSolver.solver(getOptions());
        Formula constraint = Formula.and(exclusionConstraints);
        Solution solution = solver.solve(constraint, problem.getBounds());

        MetricPoint currentValues = null;
        Solution previousSolution = null;
        while (isSat(solution)) {
            currentValues = MetricPoint.measure(solution, problem.getObjectives(), getOptions());
            Formula improvementConstraints = currentValues.parametrizedImprovementConstraints();
            previousSolution = solution;
            solution = solver.solve(improvementConstraints, new Bounds(problem.getBounds().universe()));
            incrementStats(solution, problem, improvementConstraints, false, improvementConstraints);
        }
        foundParetoPoint(currentValues);
        solver.free();

        if (!options.allSolutionsPerPoint()) {
            // no magnifying glass
            // previous solution was on the Pareto front, so report it
            tell(notifier, previousSolution, currentValues);
        } else {
            // magnifying glass
            final Collection<Formula> assignmentsConstraints = currentValues.assignmentConstraints();
            assignmentsConstraints.add(problem.getConstraints());
            int solutionsFound = magnifier(Formula.and(assignmentsConstraints), problem.getBounds(), currentValues, notifier);
            logger.log(Level.FINE, "Magnifying glass on {0} found {1} solution(s). At time: {2}", new Object[] {currentValues.values(), Integer.valueOf(solutionsFound), Integer.valueOf((int)((System.currentTimeMillis()-startTime)/1000))});
        }
        exclusionConstraints.add(currentValues.exclusionConstraint());

        // If we have more than one objective, split the problem up
        // Otherwise, we're done
        if (problem.getObjectives().size() > 1) {
            // Create the thread pool
            // Number of threads is MIN(user value, # cores)
            // TODO: Make "user value" configurable
            BlockingQueue<Future<?>> futures = new LinkedBlockingQueue<Future<?>>();
            int poolSize = Math.min(8,  Runtime.getRuntime().availableProcessors());
            ExecutorService threadPool = Executors.newFixedThreadPool(poolSize);
            logger.log(Level.FINE, "Starting a thread pool with {0} threads", new Object[] { poolSize });

            // Create all the tasks up front, adding them to an array
            logger.log(Level.FINE, "Partitioning the problem space");
            List<PartitionSearcherTask> tasks = new ArrayList<PartitionSearcherTask>();
            // Task at index 0 doesn't exist; it's in an excluded region
            // We only add null so all the tasks are added at the right index
            tasks.add(null);

            // Now we can split the search space based on the Pareto point and create new tasks
            // For n metrics, we want all combinations of m_i <= M_i and m_i >= M_i where M_i is the current value
            // To iterate over this, we map the bit_i of a bitset to metric_i
            // Note that bit_0 is the least significant bit
            // We skip 0 (the partition that is already dominated) and 2^n - 1 (the partition where we didn't find any solutions)
            int numObjectives = problem.getObjectives().size();
            int maxMapping = (int) Math.pow(2, numObjectives) - 1;
            for (int mapping = 1; mapping < maxMapping; mapping++) {
                BitSet bitSet = BitSet.valueOf(new long[] { mapping });
                tasks.add(new PartitionSearcherTask(mapping, problem, exclusionConstraints, currentValues.partitionConstraints(bitSet), notifier, threadPool, futures));
            }

            // Link up the dependencies
            for (int mapping = 1; mapping < maxMapping; mapping++) {
                PartitionSearcherTask task = tasks.get(mapping);
                task.linkDependencies(tasks);
            }

            // Submit starting tasks (the ones without dependencies) to the thread pool
            // Starting tasks are mapped to the ints with exactly one 0 bit
            // So iterate over the bitset and clear one bit at a time
            StringBuilder sb = new StringBuilder("Submitted initial tasks ");
            for (int bitIndex = 0; bitIndex < numObjectives; bitIndex++) {
                BitSet bitSet = BitSet.valueOf(new long[] { maxMapping });
                bitSet.clear(bitIndex);
                int taskIndex = (int) bitSet.toLongArray()[0];
                sb.append(taskIndex);
                sb.append(" (");
                sb.append(Integer.toBinaryString(taskIndex));
                sb.append("), ");
                futures.add(threadPool.submit(tasks.get(taskIndex)));
            }
            sb.append("to thread pool");
            logger.log(Level.FINE, sb.toString());

            // Wait for all tasks to complete by blocking on the queue and the futures
            for (int mapping = 1; mapping < maxMapping; mapping++) {
                try {
                    futures.take().get();
                } catch (InterruptedException | ExecutionException e) {
                    e.printStackTrace();
                    throw new RuntimeException(e);
                }
            }
            threadPool.shutdown();
        }

        logger.log(Level.FINE, "All Pareto points found. At time: {0}", Integer.valueOf((int)(System.currentTimeMillis()-startTime)/1000));

        // Re-enable MetricPoint logging
        metricPointLogger.setLevel(metricPointLevel);

        end(notifier);
        debugWriteStatistics();
    }

    protected void foundParetoPoint(int taskID, MetricPoint metricpoint) {
        getStats().increment(StatKey.OPTIMAL_METRIC_POINTS);
        logger.log(Level.FINE, "Task {0}: Found Pareto point with values: {1}", new Object[] { taskID, metricpoint.values() });
    }

    private class PartitionSearcherTask implements Runnable {

        private final int taskID;
        private final MultiObjectiveProblem problem;
        private Formula partitionConstraints;
        private final SolutionNotifier notifier;
        private final ExecutorService threadPool;
        private final BlockingQueue<Future<?>> futures;

        private Set<Formula> exclusionConstraints = new HashSet<Formula>();
        private List<PartitionSearcherTask> children = new ArrayList<PartitionSearcherTask>();
        private Map<Integer,Boolean> parentDoneStatus = new HashMap<Integer,Boolean>();

        private boolean started = false;
        private boolean submitted = false;

        public PartitionSearcherTask(int taskID, MultiObjectiveProblem problem, List<Formula> exclusionConstraints, Formula partitionConstraints, SolutionNotifier notifier, ExecutorService threadPool, BlockingQueue<Future<?>> futures) {
            this.taskID = taskID;
            this.problem = problem;
            this.partitionConstraints = partitionConstraints;
            this.notifier = notifier;
            this.threadPool = threadPool;
            this.futures = futures;
            this.exclusionConstraints.addAll(exclusionConstraints);
        }

        public int getID() {
            return taskID;
        }

        // Each task is represented by a binary string
        // "Adjacent" tasks have only one bit changed
        // If the adjacent task has one more 1 than this task, then it's a parent (this task depends on it)
        // If the adjacent task has one more 0 than this task, then it's a child (depends on this task)
        public void linkDependencies(List<PartitionSearcherTask> tasks) {
            if (started) {
                throw new IllegalStateException("Cannot link dependencies after task has already started.");
            }

            int numObjectives = problem.getObjectives().size();
            StringBuilder sb;

            // Initialize parent done status to false
            sb = new StringBuilder("Parents of task ");
            sb.append(taskID);
            sb.append(" (");
            sb.append(Integer.toBinaryString(taskID));
            sb.append(")");
            sb.append(" are: ");
            for (int bitIndex = 0; bitIndex < numObjectives; bitIndex++) {
                BitSet parent = BitSet.valueOf(new long[] { taskID });
                // If this bit is a 0, set it to 1; otherwise skip over it
                if (!parent.get(bitIndex)) {
                    parent.set(bitIndex);

                    // Skip over task if all bits are set (= 2^n - 1) since that task doesn't exist
                    if (parent.cardinality() == numObjectives) {
                        continue;
                    }

                    int parentIndex = (int) parent.toLongArray()[0];
                    parentDoneStatus.put(parentIndex, false);
                    sb.append(parentIndex);
                    sb.append(" (");
                    sb.append(Integer.toBinaryString(parentIndex));
                    sb.append("), ");
                }
            }
            logger.log(Level.FINE, sb.toString());

            // Add the children
            sb = new StringBuilder("Children of task ");
            sb.append(taskID);
            sb.append(" (");
            sb.append(Integer.toBinaryString(taskID));
            sb.append(")");
            sb.append(" are: ");
            for (int bitIndex = 0; bitIndex < numObjectives; bitIndex++) {
                BitSet child = BitSet.valueOf(new long[] { taskID });
                // If this bit is a 1, clear it to 0; otherwise skip over it
                if (child.get(bitIndex)) {
                    child.clear(bitIndex);

                    // Skip over task of all bits are cleared (= 0) since that task doesn't exist
                    if (child.cardinality() == 0) {
                        continue;
                    }

                    int childIndex = (int) child.toLongArray()[0];
                    children.add(tasks.get(childIndex));
                    sb.append(childIndex);
                    sb.append(" (");
                    sb.append(Integer.toBinaryString(childIndex));
                    sb.append("), ");
                }
            }
            logger.log(Level.FINE, sb.toString());
        }

        // Called by this task's dependencies when the dependency has completed
        private synchronized void notifyDone(int parentID, Collection<Formula> exclusionConstraints) {
            if (started) {
                throw new IllegalStateException("Cannot update exclusion constraints after task has already started.");
            }

            // Take in the new constraints
            this.exclusionConstraints.addAll(exclusionConstraints);

            // Mark this dependency as completed
            parentDoneStatus.put(parentID, true);

            // If the parentDoneStatus map has no false values, then all dependencies are done
            // If this task has not been submitted, then submit it
            if (!parentDoneStatus.containsValue(false) && !submitted) {
                logger.log(Level.FINE, "Scheduling task {0}.", getID());
                futures.add(threadPool.submit(this));
                submitted = true;
            } else {
                logger.log(Level.FINE, "Task {0} is not yet ready, or has already been submitted.", getID());
            }
        }

        @Override
        public void run() {
            logger.log(Level.FINE, "Entering partition {0}. At time: {1}", new Object[] { taskID, Integer.valueOf((int)((System.currentTimeMillis()-startTime)/1000)) });
            started = true;

            // Run the regular algorithm within this partition
            IncrementalSolver solver = IncrementalSolver.solver(getOptions());
            Formula constraint = Formula.and(exclusionConstraints).and(partitionConstraints);
            Solution solution = solver.solve(constraint, problem.getBounds());
            incrementStats(solution, problem, constraint, false, constraint);

            // Unsat means nothing in this partition, so we're done
            if (!isSat(solution)) {
                logger.log(Level.FINE, "No solutions found in partition {0}. At time: {1}", new Object[] { taskID, Integer.valueOf((int)((System.currentTimeMillis()-startTime)/1000)) });
            }

            // Work up to the Pareto front
            MetricPoint currentValues = null;
            Solution previousSolution = null;
            while (isSat(solution)) {
                while (isSat(solution)) {
                    currentValues = MetricPoint.measure(solution, problem.getObjectives(), getOptions());
                    Formula improvementConstraints = currentValues.parametrizedImprovementConstraints();
                    previousSolution = solution;
                    solution = solver.solve(improvementConstraints, new Bounds(problem.getBounds().universe()));
                    incrementStats(solution, problem, improvementConstraints, false, improvementConstraints);
                }
                foundParetoPoint(taskID, currentValues);

                // Free the solver's resources, since we will be creating a new solver
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
                    logger.log(Level.FINE, "Magnifying glass on {0} found {1} solution(s). At time: {2}", new Object[] {currentValues.values(), Integer.valueOf(solutionsFound), Integer.valueOf((int)((System.currentTimeMillis()-startTime)/1000))});
                }

                // Find another starting point
                solver = IncrementalSolver.solver(getOptions());
                exclusionConstraints.add(currentValues.exclusionConstraint());
                constraint = Formula.and(exclusionConstraints).and(partitionConstraints);
                solution = solver.solve(constraint, problem.getBounds());
                incrementStats(solution, problem, constraint, false, null);
            }

            logger.log(Level.FINE, "Task {0} has completed. At time: {1}", new Object[] { taskID, Integer.valueOf((int)((System.currentTimeMillis()-startTime)/1000)) });

            // Done searching in this partition, so pass this dependency to children and notify them
            // Child will schedule itself if it's done
            for (PartitionSearcherTask child : children) {
                child.notifyDone(taskID, exclusionConstraints);
            }
        }
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
