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
import kodkod.multiobjective.statistics.StepCounter;

public class PartitionedGuidedImprovementAlgorithm extends MultiObjectiveAlgorithm {

    public PartitionedGuidedImprovementAlgorithm(String desc, MultiObjectiveOptions options) {
        super(desc, options, Logger.getLogger(PartitionedGuidedImprovementAlgorithm.class.toString()));
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
        Solution solution = getSolver().solve(problem.getConstraints(), problem.getBounds());
        incrementStats(solution, problem, problem.getConstraints(), true, null);

        solveFirstStats(solution);

        List<Formula> boundaries = new ArrayList<Formula>(problem.getObjectives().size());
        MetricPoint currentValues = null;
        Solution previousSolution = null;

        // Push out along each of the objectives, to find the boundaries
        for (final Objective objective : problem.getObjectives()) {
            Formula boundaryConstraint = null;
            logger.log(Level.FINE, "Optimizing on {0}", objective.toString());

            solution = getSolver().solve(problem.getConstraints(), problem.getBounds());
            incrementStats(solution, problem, problem.getConstraints(), false, null);

            // Work up to the boundary of this metric
            while (isSat(solution)) {
                currentValues = MetricPoint.measure(solution, problem.getObjectives(), getOptions());
                logger.log(Level.FINE, "Found a solution. At time: {0}, Improving on {1}", new Object[] { Integer.valueOf((int)(System.currentTimeMillis()-startTime)/1000), currentValues.values() });

                boundaryConstraint = currentValues.objectiveImprovementConstraint(objective);

                Formula constraint = problem.getConstraints().and(boundaryConstraint);
                solution = getSolver().solve(constraint, problem.getBounds());
                incrementStats(solution, problem, constraint, false, null);
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
        Formula constraint = problem.getConstraints().and(boundaryConstraints);
        solution = getSolver().solve(constraint, problem.getBounds());
        incrementStats(solution, problem, constraint, false, null);

        counter.countStep();

        // While the current solution is satisfiable try to find a better one
        while (isSat(solution)) {
            // Work up to the Pareto front
            while (isSat(solution)) {
                currentValues = MetricPoint.measure(solution, problem.getObjectives(), getOptions());
                logger.log(Level.FINE, "Found a solution. At time: {0}, Improving on {1}", new Object[] { Integer.valueOf((int)(System.currentTimeMillis()-startTime)/1000), currentValues.values() });

                final Formula improvementConstraints = currentValues.parametrizedImprovementConstraints();

                previousSolution = solution;
                constraint = problem.getConstraints().and(boundaryConstraints).and(improvementConstraints);
                solution = getSolver().solve(constraint, problem.getBounds());
                incrementStats(solution, problem, constraint, false, improvementConstraints);

                counter.countStep();
            }
            foundParetoPoint(currentValues);

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
            constraint = Formula.and(exclusionConstraints).and(boundaryConstraints);
            solution = getSolver().solve(constraint, problem.getBounds());
            incrementStats(solution, problem, constraint, false, null);

            //count this step but first go to new index because it's a new base point
            counter.nextIndex();
            counter.countStep();
        }
        logger.log(Level.FINE, "All Pareto points found. At time: {0}", Integer.valueOf((int)(System.currentTimeMillis()-startTime)/1000));

        end(notifier);
        debugWriteStatistics();
    }
}
