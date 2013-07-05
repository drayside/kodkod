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
import kodkod.multiobjective.statistics.StatKey;
import kodkod.multiobjective.statistics.StepCounter;

public final class GuidedImprovementAlgorithm extends MultiObjectiveAlgorithm {

	/*
	 * turn on this variable to enable the step counter. It will
	 * count how many steps were taken from a base point to a Pareto point
	 * and report the results in a file specified over the variable filename
	 */
	final boolean doEvaluation = false;
	
	final String filename;
	
	final long startTime = System.currentTimeMillis();

	private static final Logger logger = Logger.getLogger(GuidedImprovementAlgorithm.class.toString());

	public GuidedImprovementAlgorithm(final String desc, final MultiObjectiveOptions options) {
		super(desc, options);
		this.filename = desc.replace("$", "");
	}
	
	@Override
	public void multiObjectiveSolve(final MultiObjectiveProblem p, final SolutionNotifier n) {
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
		Solution s1 = solveFirst(p.getConstraints(), p.getBounds(), p, null); // re-assigned around the loop
		
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
				s1 =  solveOne(p.getConstraints().and(improvementConstraints), p.getBounds(),  p, improvementConstraints);

				//count this finding
				counter.countStep();
			}
			foundMetricPoint();
			logger.log(Level.FINE, "Found Pareto point with values: {0}", currentValues.values());

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
			s1 = solveOne(Formula.and(exclusionConstraints), p.getBounds(), p, null);
			
			//count this step but first go to new index because it's a new base point
			counter.nextIndex();
			counter.countStep();
		}

		end(n);
		debugWriteStatistics();	
	}

	private void debugWriteStatistics(){
		logger.log(Level.FINE, "# Sat Call: {0}", this.getStats().get(StatKey.REGULAR_SAT_CALL));
		logger.log(Level.FINE, "# Unsat Call:  {0}", this.getStats().get( StatKey.REGULAR_UNSAT_CALL));

		logger.log(Level.FINE, "Total Time in Sat Calls: {0}", this.getStats().get(StatKey.REGULAR_SAT_TIME));
		logger.log(Level.FINE, "Total Time in Sat Calls Solving: {0}", this.getStats().get(StatKey.REGULAR_SAT_TIME_SOLVING));
		logger.log(Level.FINE, "Total Time in Sat Calls Translating: {0}", this.getStats().get(StatKey.REGULAR_SAT_TIME_TRANSLATION));

		logger.log(Level.FINE, "Total Time in Unsat Calls: {0}", this.getStats().get( StatKey.REGULAR_UNSAT_TIME));
		logger.log(Level.FINE, "Total Time in Unsat Calls Solving: {0}", this.getStats().get( StatKey.REGULAR_UNSAT_TIME_SOLVING));
		logger.log(Level.FINE, "Total Time in Unsat Calls Translating: {0}", this.getStats().get( StatKey.REGULAR_UNSAT_TIME_TRANSLATION));

		logger.log(Level.FINE, "# Magnifier Sat Call: {0}", this.getStats().get(StatKey.MAGNIFIER_SAT_CALL));
		logger.log(Level.FINE, "# Magnifier Unsat Call: {0}", this.getStats().get(StatKey.MAGNIFIER_UNSAT_CALL));
		logger.log(Level.FINE, "Total Time in Magnifier: {0}", this.getStats().get(StatKey.MAGNIFIER_TIME));
	}

}
	
