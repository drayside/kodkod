/**
 * 
 */
package tests;

import java.util.Iterator;

import junit.framework.TestCase;
import kodkod.ast.Formula;
import kodkod.engine.Solution;
import kodkod.engine.Solver;
import kodkod.engine.satlab.SATFactory;
import examples.CeilingsAndFloors;
import examples.Dijkstra;

/**
 * Tests the solution enumeration functionality of the Solver class.
 * @author Emina Torlak
 */
public class EnumerationTest extends TestCase {
	private final Solver solver;
	/**
	 * Constructs a new EnumerationTest.
	 */
	public EnumerationTest(String arg0) {
		super(arg0);
		solver = new Solver();
		solver.options().setSolver(SATFactory.MiniSat);
	}

	public final void testCeilingsAndFloors() {
		final CeilingsAndFloors model = new CeilingsAndFloors();
		final Formula f = model.belowTooAssertion();
		
		// has exactly one instance
		Iterator<Solution> sol = solver.solveAll(f, model.bounds(2,2));
		assertNotNull(sol.next().instance());
		assertNull(sol.next().instance());
		assertFalse(sol.hasNext());

		// has more than one instance
		sol = solver.solveAll(f, model.bounds(3,3));
		assertNotNull(sol.next().instance());
		assertNotNull(sol.next().instance());
		assertTrue(sol.hasNext());
		
		// has no instances
		sol = solver.solveAll(model.belowTooDoublePrime(), model.bounds(3,3));
		assertNull(sol.next().instance());
	}
	
	public final void testDijkstra() {
		final Dijkstra model = new Dijkstra();
		final Formula f = model.showDijkstra();

		Iterator<Solution> sol = solver.solveAll(f, model.bounds(5,2,2));
		int i = 0;
		while(sol.hasNext()) {
			i++;
			System.out.println(sol.next());
		}
		System.out.println(i);
	}
	
}
