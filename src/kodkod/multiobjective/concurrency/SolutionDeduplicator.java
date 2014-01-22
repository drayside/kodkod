package kodkod.multiobjective.concurrency;

import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.logging.Level;
import java.util.logging.Logger;

import kodkod.ast.Formula;
import kodkod.multiobjective.MetricPoint;

public class SolutionDeduplicator {
    private final ConcurrentHashMap<List<Integer>,Integer> acceptedSolutions;
    private Formula globalExclusionConstraint;
    protected final Logger logger;

    public SolutionDeduplicator() {
        logger = Logger.getLogger(SolutionDeduplicator.class.toString());
        acceptedSolutions = new ConcurrentHashMap<List<Integer>,Integer>();
        globalExclusionConstraint = Formula.constant(true);
    }

    public boolean pushNewSolution(MetricPoint solution) {
        if(acceptedSolutions.put(solution.values(), new Integer(1)) != null) {
            logger.log(Level.FINE, "Received a duplicate solution");
            return false;
        } else {
            synchronized (this) {
                globalExclusionConstraint = globalExclusionConstraint.and(solution.exclusionConstraint());
            }
            return true;
        }
    }

    public Formula getGlobalExclusionConstraint() {
        return globalExclusionConstraint;
    }
}
