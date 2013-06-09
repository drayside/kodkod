package kodkod.multiobjective.api;

import java.util.Iterator;
import java.util.SortedSet;

import kodkod.ast.Formula;
import kodkod.engine.AbortedException;
import kodkod.engine.KodkodSolver;
import kodkod.engine.Solution;
import kodkod.engine.Solver;
import kodkod.engine.config.Options;
import kodkod.engine.fol2sat.HigherOrderDeclException;
import kodkod.engine.fol2sat.UnboundLeafException;
import kodkod.instance.Bounds;
import kodkod.multiobjective.AlloySolver;

public final class MultiObjectiveSolver implements KodkodSolver {

	AlloySolver mooSolver;
	Solver solver;
	
	
	public MultiObjectiveSolver() {
		solver = new Solver();
	}
	
	public MultiObjectiveSolver(Options options) {
		if (options == null) {
			throw new NullPointerException();
		}
		
		solver = new Solver( options );
	}
	
	// [TeamAmalgam] - Adding for Alloy support
	public GIAStepCounter getGIACountCallsOnEachMovementToParetoFront(){
		return this.mooSolver.getGIACountCallsOnEachMovementToParetoFront();
	}

	// [TeamAmalgam] - Adding for Alloy support
	public Stats getStats() {
		return this.mooSolver.getStats();
	}
	
	public Solver getKodkodSolver() {
		return solver;
	}

	@Override
	public Options options() {
		return solver.options();
	}

	@Override
	public Solution solve(Formula formula, Bounds bounds)
			throws HigherOrderDeclException, UnboundLeafException,
			AbortedException {
		return solver.solve(formula, bounds);
	}

	@Override
	public void free() {
		solver.free();
	}
	
	public Iterator<Solution> solveAll(final Formula formula, final Bounds bounds, final SortedSet<Objective>objectives, Boolean magnifyingGlass ) 
			throws HigherOrderDeclException, UnboundLeafException, AbortedException {
		if (objectives != null) {
			mooSolver = new AlloySolver(formula, bounds, objectives, magnifyingGlass);
			mooSolver.run();
			
			return mooSolver.solveAll();
		}
		else {
			return solver.solveAll(formula, bounds);
		}
		
	}
	
}
