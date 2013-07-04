package kodkod.multiobjective.algorithms;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import kodkod.ast.Formula;
import kodkod.engine.Solution;
import kodkod.instance.Bounds;
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

	public GuidedImprovementAlgorithm(final String desc, final MultiObjectiveOptions options) {
		super(desc, options);
		this.filename = desc.replace("$", "");
	}
	
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
				System.out.println("Found a solution. At time: " + (System.currentTimeMillis()-startTime)/1000 + ", Improving on " + currentValues.values());
				
				final Formula improvementConstraints = currentValues.parametrizedImprovementConstraints();

				sprev = s1;
				s1 = getSolver().solve(p.getConstraints().and(improvementConstraints), p.getBounds());
				incrementStats(s1, p, p.getConstraints().and(improvementConstraints), false, improvementConstraints);				

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
				magnifier(Formula.and(assignmentsConstraints), p.getBounds(), currentValues, n);
			}

			// start looking for next base point
			exclusionConstraints.add(currentValues.exclusionConstraint());
			s1 = getSolver().solve(Formula.and(exclusionConstraints), p.getBounds());
			
			//count this step but first go to new index because it's a new base point
			counter.nextIndex();
			counter.countStep();
		}

		end(n);
		debugWriteStatistics();	
	}
}
	
