package kodkod.multiobjective;

import java.util.Iterator;
import java.util.SortedSet;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import kodkod.ast.Formula;
import kodkod.engine.Solution;
import kodkod.instance.Bounds;
import kodkod.multiobjective.api.GIAStepCounter;
import kodkod.multiobjective.api.GuidedImprovementAlgorithm;
import kodkod.multiobjective.api.MultiObjectiveProblem;
import kodkod.multiobjective.api.Objective;
import kodkod.multiobjective.api.SolutionNotifier;
import kodkod.multiobjective.api.Stats;

public final class AlloySolver extends Thread {

	private final MultiObjectiveProblem problem;
	private final SolutionNotifier notifier;
	
	// Find all solutions at each Pareto point
	private final boolean allSolutionsPerPoint;

	final BlockingQueue<Solution> q;
	final MoolloyBlockingSolutionIterator it;
	final SolutionNotifier n;
	final GuidedImprovementAlgorithm gia;

	public AlloySolver(final Formula formula, final Bounds bounds, final SortedSet<Objective> objectives, 
			Boolean magnifyingGlass) {
		q = new LinkedBlockingQueue<Solution>();
		it = new MoolloyBlockingSolutionIterator(q);
		n = new TranslatingBlockingQueueSolutionNotifier(q);
		gia = new GuidedImprovementAlgorithm("GIA");
		
		problem = new MultiObjectiveProblem(bounds, formula, objectives);
		notifier = new TranslatingBlockingQueueSolutionNotifier(q);
		allSolutionsPerPoint = magnifyingGlass;
	}

	public void run() {
		// TODO 
		gia.setSymmetryBreaking(1000);
		gia.moosolve(problem, notifier, allSolutionsPerPoint);
	}

	// TODO: This code may need revision. Currently, user of AlloySolver should
	// call run() before calling solveAll(), which returns an iterator over the
	// solutions in the BlockingQueue.
	public Iterator<Solution> solveAll() {
//		System.out.println("Iterator over all solutions, it has a next solution: " + it.hasNext());
		return it;
	}
	
	public GIAStepCounter getGIACountCallsOnEachMovementToParetoFront(){
		return this.gia.getCountCallsOnEachMovementToParetoFront();
	}

	public Stats getStats() {
		return this.gia.getStats();
	}
}
