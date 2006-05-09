package tests;

import examples.Bigconfig;
import examples.CeilingsAndFloors;
import examples.Dijkstra;
import examples.Pigeonhole;
import examples.RingElection;
import examples.Sudoku;
import examples.Toughnut;
import junit.framework.TestCase;
import kodkod.ast.Formula;
import kodkod.engine.Solution;
import kodkod.engine.Solver;
import kodkod.engine.TimeoutException;
import kodkod.engine.satlab.SATFactory;
import kodkod.instance.Bounds;

public class ExamplesTest extends TestCase {

	private final Solver solver;
	
	public ExamplesTest(String arg0) {
		super(arg0);
		this.solver = new Solver();
		this.solver.options().setSolver(SATFactory.ZChaff);
	}

	private Solution solve(Formula formula, Bounds bounds) {
		try {
			return solver.solve(formula, bounds);
		} catch (TimeoutException e) {
			e.printStackTrace();
			throw new AssertionError();
		}
	}
	
	/**
	 * Runs the Bigconfig example for 1 hq, 9 subs, 4 unwindings.
	 */
	public void testBigconfig() {
		final Bigconfig model = new Bigconfig(4);
		assertEquals(Solution.Outcome.SATISFIABLE, 
				    solve(model.show(), model.bounds(1, 9, 10)).outcome());
	}
	
	/**
	 * Runs the CeilingsAndFloors example for 6 Man, 6 Platform.
	 */
	public void testCeilingsAndFloors() {
		final CeilingsAndFloors model = new CeilingsAndFloors();
		final Formula assertion = model.declarations().and(model.belowTooDoublePrime());
		assertEquals(Solution.Outcome.UNSATISFIABLE, 
				    solve(assertion, model.bounds(6, 6)).outcome());
	}
	
	/**
	 * Runs the Dijkstra example for 6 States, 6 Processes, and 6 Mutexes.
	 */
	public void testDijkstra() {
		final Dijkstra model = new Dijkstra();
		final Formula noDeadlocks = model.declarations().and(model.dijkstraPreventsDeadlocks().not());
		assertEquals(Solution.Outcome.UNSATISFIABLE, 
				    solve(noDeadlocks, model.bounds(6,6,6)).outcome());
	}
	
	/**
	 * Runs the Pigeonhole example for 10 Pigeons, 9 Holes.
	 */
	public void testPigeonhole() {
		final Pigeonhole model = new Pigeonhole();
		final Formula show = model.declarations().and(model.pigeonPerHole());
		assertEquals(Solution.Outcome.UNSATISFIABLE, 
			    solve(show, model.bounds(10,9)).outcome());
	}
	
	/**
	 * Runs the RingElection example for 10 Times, 5 Proceses.
	 */
	public void testRingElection() {
		final RingElection model = new RingElection();
		final Formula checkAtMostOneElected = model.declsAndFacts().and(model.atMostOneElected().not());
		assertEquals(Solution.Outcome.UNSATISFIABLE, 
			    solve(checkAtMostOneElected, model.bounds(5,10)).outcome());
	}

	/**
	 * Runs the Sudoku example.
	 */
	public void testSudoku() {
		final Sudoku model = new Sudoku(3);
		assertEquals(Solution.Outcome.SATISFIABLE, 
				    solve(model.rules(), model.puzzle1()).outcome());
	}
	
	/**
	 * Runs the Toughnut example for an 8x8 board.
	 */
	public void testToughnut() {
		final Toughnut model = new Toughnut();
		assertEquals(Solution.Outcome.TRIVIALLY_UNSATISFIABLE, 
					solve(model.covering(), model.board(8)).outcome());
	}
	
}
