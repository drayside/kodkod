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
	final Solver kodkodSolver;
	
	final MultiObjectiveOptions options;
	
	
	public MultiObjectiveSolver() {
		
		options = new MultiObjectiveOptions();
		kodkodSolver = new Solver(options.getKodkodOptions());
	}
	
	public MultiObjectiveSolver(Options options) {
		if (options == null) {
			throw new NullPointerException();
		}
		
		this.options = new MultiObjectiveOptions( options );
		
		kodkodSolver = new Solver( this.options.getKodkodOptions() );
	}
	
	public MultiObjectiveSolver(MultiObjectiveOptions options) {
		
		if (options == null) {
			throw new NullPointerException();
		}
		
		this.options = options;
		
		kodkodSolver = new Solver( options.getKodkodOptions() );

	}
	
	public GIAStepCounter getGIACountCallsOnEachMovementToParetoFront(){
		return this.mooSolver.getGIACountCallsOnEachMovementToParetoFront();
	}

	public Stats getStats() {
		return this.mooSolver.getStats();
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
	
	public Iterator<Solution> solveAll(final Formula formula, final Bounds bounds, final SortedSet<Objective>objectives, Boolean magnifyingGlass ) 
			throws HigherOrderDeclException, UnboundLeafException, AbortedException {
		if (objectives != null) {
			mooSolver = new AlloySolver(formula, bounds, objectives, magnifyingGlass);
			mooSolver.run();
			
			return mooSolver.solveAll();
		}
		else {
			return kodkodSolver.solveAll(formula, bounds);
		}
		
	}
	
}
