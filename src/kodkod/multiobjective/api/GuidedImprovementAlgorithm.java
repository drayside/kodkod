package kodkod.multiobjective.api;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.SortedSet;

import kodkod.ast.Formula;
import kodkod.engine.Solution;
import kodkod.instance.Bounds;

public final class GuidedImprovementAlgorithm extends MultiObjectiveSolver {

	/*
	 * turn on this variable to enable the step counter. It will
	 * count how many steps were taken from a base point to a Pareto point
	 * and report the results in a file specified over the variable filename
	 */
	final boolean doEvaluation = false;
	
	final String filename;
	
	final long startTime = System.currentTimeMillis();
	
	private GIAStepCounter counter;
	
	public GuidedImprovementAlgorithm(final String desc) {
		super(desc);
		this.filename = desc.replace("$", "");
	}

	public GuidedImprovementAlgorithm(final String desc, final boolean parallelize) {
		super(desc, parallelize);
		this.filename = desc.replace("$", "");
	}

	@Override
	public void moosolve(final MultiObjectiveProblem p, final SolutionNotifier n, final boolean magnifyingGlass) {
		System.out.println("Called with Magnifier Glass = " + magnifyingGlass);		
		// set the bit width
		setBitWidth(p.bitWidth);

		// for the evaluation we need a step counter
		this.counter = new GIAStepCounter();
		
		//begin, amongst others, start the timer
		begin();
				
		// NB: f is added to every list of constraints
		// the rationale is to have kodkod form bushy, rather than deep, conjunction trees
		// used to exclude solutions dominated by known pareto-optimal points
		final List<Formula> exclusionConstraints = new ArrayList<Formula>();
		exclusionConstraints.add(p.constraints);

		// first base point
		Solution s1 = solveFirst(p.constraints, p.bounds, p, null); // re-assigned around the loop

		if (isSat(s1)) {
			System.out.println("Found base solution. At time: " + (System.currentTimeMillis()-startTime)/1000 + ", Improving on " + MetricPoint.measure(s1, p.objectives, getOptions()).values());
		}
		
		//count this finding
		counter.countStep();
		
		// any solution that passess this loop condition will 
		// also pass the inner loop condition and be counted there
		while (isSat(s1)) {
			MetricPoint currentValues = null; // is re-assigned around the inner loop
			Solution sprev = null;
			// work up to the pareto front

			while (isSat(s1)) {
				currentValues = MetricPoint.measure(s1, p.objectives, getOptions());

				System.out.println("Found a better one. At time: " + (System.currentTimeMillis()-startTime)/1000 + ", Improving on " + currentValues.values());
				
				final Formula improvementConstraints = currentValues.ParametrizedImprovementConstraints();
				
				System.out.println("Improvement Constraints are " + improvementConstraints );
				sprev = s1;
				s1 =  solveOne(p.constraints.and(improvementConstraints), p.bounds,  p, improvementConstraints);

				//count this finding
				counter.countStep();
			}
			foundMetricPoint();
			System.out.println("GIA: " + currentValues.values());

			if (!magnifyingGlass) {
				// no magnifying glass
				// previous solution was on the pareto front: report it
				System.out.println("No Magnifier Glass");
				tell(n, sprev, currentValues);
				
			} else {
				System.out.println("With Magnifier Glass");
				// magnifying glass				
				final Collection<Formula> assignmentsConstraints = currentValues.assignmentConstraints();
				assignmentsConstraints.add(p.constraints);
				magnifier(Formula.and(assignmentsConstraints), p.bounds, currentValues, n);
			}

			// start looking for next base point
			exclusionConstraints.add(currentValues.exclusionConstraint());
			s1 = solveOne(Formula.and(exclusionConstraints), p.bounds, p, null);
			
			//count this step but first go to new index because it's a new base point
			counter.nextIndex();
			counter.countStep();


		}


		end(n);
		
		debugWriteStatistics();	
	}

	private void debugWriteStatistics(){
		System.out.println("\t # Sat Call: " +  this.getStats().get(StatKey.REGULAR_SAT_CALL));
		System.out.println("\t # Unsat Call:  " +this.getStats().get( StatKey.REGULAR_UNSAT_CALL));		

		System.out.println("\t Total Time in Sat Calls: " +  this.getStats().get(StatKey.REGULAR_SAT_TIME));
		System.out.println("\t Total Time in Sat Calls Solving: " +  this.getStats().get(StatKey.REGULAR_SAT_TIME_SOLVING));
		System.out.println("\t Total Time in Sat Calls Translating: " +  this.getStats().get(StatKey.REGULAR_SAT_TIME_TRANSLATION));		

		System.out.println("\t Total Time in Unsat Calls:  " +this.getStats().get( StatKey.REGULAR_UNSAT_TIME));		
		System.out.println("\t Total Time in Unsat Calls Solving:  " +this.getStats().get( StatKey.REGULAR_UNSAT_TIME_SOLVING));
		System.out.println("\t Total Time in Unsat Calls Translating:  " +this.getStats().get( StatKey.REGULAR_UNSAT_TIME_TRANSLATION));
		
	}
	
	protected MeasuredSolution findParetoPoint(final Formula f, final Bounds b, final SortedSet<Objective> e) {
		// base point
		Solution s1 = solveOne(f, b, null, null); // re-assigned around the loop
		
		// any solution that passess this loop condition will 
		// also pass the inner loop condition and be counted there
		MetricPoint currentValues = null; // is re-assigned around the inner loop
		Solution sprev = null;
		// work up to the pareto front
		while (isSat(s1)) {
			currentValues = MetricPoint.measure(s1, e, getOptions());
			final Formula improvementConstraints = currentValues.improvementConstraints();
			sprev = s1;
			s1 = solveOne(f.and(improvementConstraints), b, null, improvementConstraints);
		}
		
		if (sprev != null && isSat(sprev)) {
			// found a pareto point
			return new MeasuredSolution(sprev, currentValues);
		} else {
			// nothing found
			return null;
		}
	}
	
	public GIAStepCounter getCountCallsOnEachMovementToParetoFront(){
		return this.counter;
	}

}
	
