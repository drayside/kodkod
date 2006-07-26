package examples;

import kodkod.ast.Formula;
import kodkod.engine.Solution;
import kodkod.engine.Solver;
import kodkod.engine.TimeoutException;
import kodkod.engine.satlab.SATFactory;
import kodkod.instance.Bounds;

class ExamplesTest  {

	private final Solver solver;
	
	public ExamplesTest(SATFactory factory) {
		this.solver = new Solver();
		this.solver.options().setSolver(factory);
	}

	private Solution solve(Formula formula, Bounds bounds) {
		try {
			return solver.solve(formula, bounds);
		} catch (TimeoutException e) {
			e.printStackTrace();
			throw new AssertionError();
		}
	}
	private void assertEquals(Object o1, Object o2) {
		if (!o1.equals(o2))
			throw new IllegalArgumentException();
	}
	private void assertEquals(int o1, int o2) {
		if (o1!=o2)
			throw new IllegalArgumentException();
	}
	/**
	 * Runs the Bigconfig example for 1 hq, 9 subs, 4 unwindings.
	 */
	public void testBigconfig() {
		final Bigconfig model = new Bigconfig(4);
		final Solution sol = solve(model.show(), model.bounds(1, 9, 10));
//		SATISFIABLE
//		p cnf 2227 4171
//		primary variables: 100
		assertEquals(Solution.Outcome.SATISFIABLE,sol.outcome());
		assertEquals(100, sol.stats().primaryVariables());
		assertEquals(2227, sol.stats().variables());
		assertEquals(4171, sol.stats().clauses());
	}
	
	/**
	 * Runs the CeilingsAndFloors example for 6 Man, 6 Platform.
	 */
	public void testCeilingsAndFloors() {
		final CeilingsAndFloors model = new CeilingsAndFloors();
		final Formula assertion = model.declarations().and(model.belowTooDoublePrime());
		final Solution sol = solve(assertion, model.bounds(6, 6));
//		UNSATISFIABLE
//		p cnf 1686 3447
//		primary variables: 90
		assertEquals(Solution.Outcome.UNSATISFIABLE, sol.outcome());
		assertEquals(90, sol.stats().primaryVariables());
		assertEquals(1686, sol.stats().variables());
		assertEquals(3447, sol.stats().clauses());
	}
	
	/**
	 * Runs the Dijkstra example for 6 States, 6 Processes, and 6 Mutexes.
	 */
	public void testDijkstra() {
		final Dijkstra model = new Dijkstra();
		final Formula noDeadlocks = model.declarations().and(model.dijkstraPreventsDeadlocks().not());
		final Solution sol = solve(noDeadlocks, model.bounds(6,6,6));
//		UNSATISFIABLE
//		p cnf 4341 18623
//		primary variables: 444
		assertEquals(Solution.Outcome.UNSATISFIABLE, sol.outcome());
		assertEquals(444, sol.stats().primaryVariables());
		assertEquals(4341, sol.stats().variables());
		assertEquals(18623, sol.stats().clauses());
	}
	
	/**
	 * Runs the Pigeonhole example for 10 Pigeons, 9 Holes.
	 */
	public void testPigeonhole() {
		final Pigeonhole model = new Pigeonhole();
		final Formula show = model.declarations().and(model.pigeonPerHole());
		final Solution sol = solve(show, model.bounds(10,9));
//		UNSATISFIABLE
//		p cnf 983 1613
//		primary variables: 90
		assertEquals(Solution.Outcome.UNSATISFIABLE, sol.outcome());
		assertEquals(90, sol.stats().primaryVariables());
		assertEquals(983, sol.stats().variables());
		assertEquals(1613, sol.stats().clauses());
	}
	
	/**
	 * Runs the RingElection example for 10 Times, 5 Proceses.
	 */
	public void testRingElection() {
		final RingElection model = new RingElection();
		final Formula checkAtMostOneElected = model.declsAndFacts().and(model.atMostOneElected().not());
		final Solution sol = solve(checkAtMostOneElected, model.bounds(5,10));
//		UNSATISFIABLE
//		p cnf 8654 29705
//		primary variables: 325
		assertEquals(Solution.Outcome.UNSATISFIABLE, sol.outcome());
		assertEquals(325, sol.stats().primaryVariables());
		assertEquals(8654, sol.stats().variables());
		assertEquals(29705, sol.stats().clauses());
	}

	/**
	 * Runs the Sudoku example.
	 */
	public void testSudoku() {
		final Sudoku model = new Sudoku(3);
		final Solution sol = solve(model.rules(), model.puzzle1());
//		SATISFIABLE
//		p cnf 1544 6974
//		primary variables: 702
		assertEquals(Solution.Outcome.SATISFIABLE, sol.outcome());
		assertEquals(702, sol.stats().primaryVariables());
		assertEquals(1544, sol.stats().variables());
		assertEquals(6974, sol.stats().clauses());
	}
	
	/**
	 * Runs the Toughnut example for an 8x8 board.
	 */
	public void testToughnut() {
		final Toughnut model = new Toughnut();
		final Solution sol = solve(model.covering(), model.board(8));
//		TRIVIALLY_UNSATISFIABLE
//		p cnf 0 0
//		primary variables: 0
		assertEquals(Solution.Outcome.TRIVIALLY_UNSATISFIABLE, sol.outcome());
		assertEquals(0, sol.stats().primaryVariables());
		assertEquals(0, sol.stats().variables());
		assertEquals(0, sol.stats().clauses());
	}
	
	public static void main(String[] args) {
		
		ExamplesTest t = new ExamplesTest(SATFactory.ZChaffBasic);
		
		System.out.println("testBigconfig ");
		t.testBigconfig();
		
		t = new ExamplesTest(SATFactory.ZChaffMincost);
		System.out.println("testSudoku ");
		t.testSudoku();
		
//		System.out.println("testCeilingsAndFloors ");
//		t.testCeilingsAndFloors();
		
		
//		t = new ExamplesTest();
//		System.out.println("testDijkstra ");
//		t.testDijkstra();
//		t = new ExamplesTest();
//		System.out.println("testPigeonhole ");
//		t.testPigeonhole();
//		t = new ExamplesTest();
//		System.out.println("testRingElection ");
//		t.testRingElection();

//		t = new ExamplesTest();
//		System.out.println("testToughnut ");
//		t.testToughnut();
	}
	
}
