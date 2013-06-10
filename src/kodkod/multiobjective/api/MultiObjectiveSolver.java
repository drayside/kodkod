package kodkod.multiobjective.api;

import java.util.Iterator;
import java.util.SortedSet;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import kodkod.ast.Formula;
import kodkod.engine.AbortedException;
import kodkod.engine.KodkodSolver;
import kodkod.engine.Solution;
import kodkod.engine.Solver;
import kodkod.engine.config.Options;
import kodkod.engine.fol2sat.HigherOrderDeclException;
import kodkod.engine.fol2sat.UnboundLeafException;
import kodkod.instance.Bounds;
import kodkod.multiobjective.MoolloyBlockingSolutionIterator;
import kodkod.multiobjective.TranslatingBlockingQueueSolutionNotifier;

public final class MultiObjectiveSolver implements KodkodSolver {

	final MultiObjectiveAlgorithm algorithm;
	final SolutionNotifier solutionNotifier;
	final MoolloyBlockingSolutionIterator solutionIterator;
	final BlockingQueue<Solution> solutionQueue;
	MultiObjectiveProblem problem;

	final Solver kodkodSolver;
	final MultiObjectiveOptions options;

	public MultiObjectiveSolver() {
		options = new MultiObjectiveOptions();
		kodkodSolver = new Solver(options.getKodkodOptions());

		solutionQueue = new LinkedBlockingQueue<Solution>();
		solutionIterator = new MoolloyBlockingSolutionIterator(solutionQueue);
		algorithm = new GuidedImprovementAlgorithm("GIA", options);
		solutionNotifier = new TranslatingBlockingQueueSolutionNotifier(solutionQueue);
	}
	
	public StepCounter getCountCallsOnEachMovementToParetoFront(){
		return algorithm.getCountCallsOnEachMovementToParetoFront();
	}
	
	public Solver getKodkodSolver() {
		return kodkodSolver;
	}

	public MultiObjectiveOptions multiObjectiveOptions() {
		return options;
	}
	
	@Override
	public Options options() {
		return kodkodSolver.options();
	}

	@Override
	public Solution solve(Formula formula, Bounds bounds)
			throws HigherOrderDeclException, UnboundLeafException,
			AbortedException {
		return kodkodSolver.solve(formula, bounds);
	}

	@Override
	public void free() {
		kodkodSolver.free();
	}
	
	public Iterator<Solution> solveAll(final Formula formula, final Bounds bounds, final SortedSet<Objective>objectives ) 
			throws HigherOrderDeclException, UnboundLeafException, AbortedException {
		if (objectives != null) {
			problem = new MultiObjectiveProblem(bounds, formula, objectives);

			Thread solverThread = new Thread(new Runnable() {
				public void run() {
					algorithm.moosolve(problem, solutionNotifier);
				}
			});
			solverThread.start();
			
			return solutionIterator;
		}
		else {
			return kodkodSolver.solveAll(formula, bounds);
		}
	}
	
	public Stats getStats() {
		return algorithm.getStats();
	}
}
