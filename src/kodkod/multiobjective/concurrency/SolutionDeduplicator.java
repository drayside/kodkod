package kodkod.multiobjective.concurrency;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import kodkod.multiobjective.algorithms.GuidedImprovementAlgorithm;

public class SolutionDeduplicator {
    private final List<List<Integer>> acceptedSolutions;
    protected final Logger logger;

    public SolutionDeduplicator() {
        logger = Logger.getLogger(SolutionDeduplicator.class.toString());
        acceptedSolutions = new ArrayList<List<Integer>>();
    }

    public synchronized boolean pushNewSolution(List<Integer> solution) {
        for( List<Integer> acceptedSolution : acceptedSolutions ) {
            if(solution.equals(acceptedSolution)) {
                logger.log(Level.FINE, "Received a duplicate solution");
                return false;
            }
        }

        acceptedSolutions.add(solution);
        return true;
    }
}
