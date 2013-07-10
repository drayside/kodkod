package kodkod.multiobjective.algorithms;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import kodkod.ast.Formula;
import kodkod.engine.Solution;
import kodkod.multiobjective.MetricPoint;
import kodkod.multiobjective.MultiObjectiveOptions;
import kodkod.multiobjective.MultiObjectiveProblem;
import kodkod.multiobjective.Objective;
import kodkod.multiobjective.concurrency.SolutionNotifier;
import kodkod.multiobjective.statistics.StatKey;
import kodkod.multiobjective.statistics.StepCounter;

public class BoundedGuidedImprovementAlgorithm extends MultiObjectiveAlgorithm {

    /*
     * turn on this variable to enable the step counter. It will
     * count how many steps were taken from a base point to a Pareto point
     * and report the results in a file specified over the variable filename
     */
    final boolean doEvaluation = false;

    final String filename;

    final long startTime = System.currentTimeMillis();

    private static final Logger logger = Logger.getLogger(BoundedGuidedImprovementAlgorithm.class.toString());

    public BoundedGuidedImprovementAlgorithm(String desc, MultiObjectiveOptions options) {
        super(desc, options);
        this.filename = desc.replace("$", "");
    }

    @Override
    public void multiObjectiveSolve(MultiObjectiveProblem problem, SolutionNotifier notifier) {
        // set the bit width
        setBitWidth(problem.getBitWidth());

        // for the evaluation we need a step counter
        this.counter = new StepCounter();

        //begin, amongst others, start the timer
        begin();

        final List<Formula> exclusionConstraints = new ArrayList<Formula>();
        exclusionConstraints.add(problem.getConstraints());

        // Throw a dart and get a starting point
        Solution solution = solveFirst(problem.getConstraints(), problem.getBounds(), problem, null); // re-assigned around the loop

        List<Formula> boundaries = new ArrayList<Formula>(problem.getObjectives().size());
        MetricPoint currentValues = null;
        Solution previousSolution = null;

        // Push out along each of the objectives, to find the boundaries
        for (final Objective objective : problem.getObjectives()) {
            Formula boundaryConstraint = null;
            logger.log(Level.FINE, "Optimizing on {0}", objective.toString());

            solution = solveOne(problem.getConstraints(), problem.getBounds(), problem, null);

            // Work up to the boundary of this metric
            while (isSat(solution)) {
                currentValues = MetricPoint.measure(solution, problem.getObjectives(), getOptions());
                logger.log(Level.FINE, "Found a solution. At time: {0}, Improving on {1}", new Object[] { Integer.valueOf((int)(System.currentTimeMillis()-startTime)/1000), currentValues.values() });

                boundaryConstraint = currentValues.objectiveImprovementConstraint(objective);

                solution = solveOne(problem.getConstraints().and(boundaryConstraint), problem.getBounds(), problem, boundaryConstraint);
            }
            logger.log(Level.FINE, "Found boundary {0}", currentValues.boundaryConstraint(objective));
            boundaries.add(currentValues.boundaryConstraint(objective));
        }

        StringBuilder sb = new StringBuilder("All boundaries found. At time: ");
        sb.append(Integer.valueOf((int)(System.currentTimeMillis()-startTime)/1000));
        sb.append(", Boundaries are conjunction of ");
        for (Formula boundary : boundaries) {
            sb.append("\n\t");
            sb.append(boundary);
        }
        logger.log(Level.FINE, sb.toString());

        Formula boundaryConstraints = Formula.and(boundaries);

        // Now we can do the regular GIA, but with the boundaries as extra constraints
        solution = solveOne(problem.getConstraints().and(boundaryConstraints), problem.getBounds(), problem, null);

        counter.countStep();

        // While the current solution is satisfiable try to find a better one
        while (isSat(solution)) {
            // Work up to the Pareto front
            while (isSat(solution)) {
                currentValues = MetricPoint.measure(solution, problem.getObjectives(), getOptions());
                logger.log(Level.FINE, "Found a solution. At time: {0}, Improving on {1}", new Object[] { Integer.valueOf((int)(System.currentTimeMillis()-startTime)/1000), currentValues.values() });

                final Formula improvementConstraints = currentValues.parametrizedImprovementConstraints();

                previousSolution = solution;
                solution =  solveOne(problem.getConstraints().and(boundaryConstraints).and(improvementConstraints), problem.getBounds(),  problem, improvementConstraints);

                counter.countStep();
            }
            foundMetricPoint();
            logger.log(Level.FINE, "Found Pareto point with values: {0}", currentValues.values());

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

            // start looking for next base point
            exclusionConstraints.add(currentValues.exclusionConstraint());
            solution = solveOne(Formula.and(exclusionConstraints).and(boundaryConstraints), problem.getBounds(), problem, null);

            //count this step but first go to new index because it's a new base point
            counter.nextIndex();
            counter.countStep();
        }
        logger.log(Level.FINE, "All Pareto points found. At time: {0}", Integer.valueOf((int)(System.currentTimeMillis()-startTime)/1000));

        end(notifier);
        debugWriteStatistics();
    }

    private void debugWriteStatistics(){
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
}
