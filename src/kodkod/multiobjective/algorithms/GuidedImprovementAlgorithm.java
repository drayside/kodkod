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
import kodkod.multiobjective.concurrency.SolutionNotifier;
import kodkod.multiobjective.statistics.StepCounter;

public final class GuidedImprovementAlgorithm extends MultiObjectiveAlgorithm {

	public GuidedImprovementAlgorithm(final String desc, final MultiObjectiveOptions options) {
		super(desc, options, Logger.getLogger(GuidedImprovementAlgorithm.class.toString()));
	}
	
	@Override
	protected void multiObjectiveSolveImpl(final MultiObjectiveProblem p, final SolutionNotifier n) {
		// set the bit width
		setBitWidth(p.getBitWidth());

		// for the evaluation we need a step counter
		this.counter = new StepCounter();
		
		//begin, amongst others, start the timer
		begin();
				
		// NB: f is added to every list of constraints
		// the rationale is to have kodkod form bushy, rather than deep, conjunction trees
		// used to exclude solutions dominated by known pareto-optimal points
		final List<Formula> exclusionConstraints = new ArrayList<Formula>();
		exclusionConstraints.add(p.getConstraints());

		// first base point
		Solution s1 = getSolver().solve(p.getConstraints(), p.getBounds());
		incrementStats(s1, p, p.getConstraints(), true, null);
		
		solveFirstStats(s1);
		
		//count this finding
		counter.countStep();	
		
		// any solution that passess this loop condition will 
		// also pass the inner loop condition and be counted there
		while (isSat(s1)) {
			MetricPoint currentValues = null; // is re-assigned around the inner loop
			Solution sprev = null;

			// work up to the pareto front
			while (isSat(s1)) {
				currentValues = MetricPoint.measure(s1, p.getObjectives(), getOptions());
				logger.log(Level.FINE, "Found a solution. At time: {0}, Improving on {1}", new Object[] { Integer.valueOf((int)(System.currentTimeMillis()-startTime)/1000), currentValues.values() });
				
				final Formula improvementConstraints = currentValues.parametrizedImprovementConstraints();

				sprev = s1;
				Formula constraint = p.getConstraints().and(improvementConstraints);
				s1 = getSolver().solve(constraint, p.getBounds());
				incrementStats(s1, p, constraint, false, improvementConstraints);				

				//count this finding
				counter.countStep();
			}

			foundParetoPoint(currentValues);

			if (!options.allSolutionsPerPoint()) {
				// no magnifying glass
				// previous solution was on the pareto front: report it
				tell(n, sprev, currentValues);
			} else {
				// magnifying glass				
				final Collection<Formula> assignmentsConstraints = currentValues.assignmentConstraints();
				assignmentsConstraints.add(p.getConstraints());
				int solutionsFound = magnifier(Formula.and(assignmentsConstraints), p.getBounds(), currentValues, n);
				logger.log(Level.FINE, "Magnifying glass found {0} solution(s). At time: {1}", new Object[] {Integer.valueOf(solutionsFound), Integer.valueOf((int)((System.currentTimeMillis()-startTime)/1000))});
			}

			// start looking for next base point
			exclusionConstraints.add(currentValues.exclusionConstraint());
			Formula constraint = Formula.and(exclusionConstraints);
			s1 = getSolver().solve(constraint, p.getBounds());
			incrementStats(s1, p, constraint, false, null);

			//count this step but first go to new index because it's a new base point
			counter.nextIndex();
			counter.countStep();
		}
		logger.log(Level.FINE, "All Pareto points found. At time: {0}", Integer.valueOf((int)(System.currentTimeMillis()-startTime)/1000));

		end(n);
		debugWriteStatistics();	
	}
}
	
