package tests;

import java.util.Arrays;

import junit.framework.TestCase;

import org.sat4j.core.VecInt;
import org.sat4j.minisat.SolverFactory;
import org.sat4j.specs.ContradictionException;
import org.sat4j.specs.ISolver;
import org.sat4j.specs.IVecInt;
import org.sat4j.specs.TimeoutException;

public class SolverTest extends TestCase {
	private final int NUM_VARS = 10;
	private ISolver solver;
	
	@Override
	protected void setUp() {
		solver = (new SolverFactory()).defaultSolver();
		solver.newVar(NUM_VARS);
	}
	
	/**
	 * Adds the specified clause to the solver.
	 */
	private void addClause(int... vars) {
		try {
			final IVecInt clause = new VecInt(vars.length);
			for(int i: vars) clause.push(i);
			solver.addClause(clause);
		} catch (ContradictionException ce) {
			System.out.println("contradiction");
		}
	}
	
	public final void testSolver() {
		addClause(1);
		addClause(-5);
		addClause(-6);
		addClause(4);
		try {
			if (solver.isSatisfiable()) {
				System.out.println(Arrays.toString(solver.model()));
			}
		} catch (TimeoutException e) {
			fail();
		}
	}
	
	public final void testContradiction() {
		addClause(1, -2);
		addClause(-1);
		addClause(2);
		
		try {
			if (solver.isSatisfiable()) {
				System.out.println(Arrays.toString(solver.model()));
			} 
		} catch (TimeoutException e) {
			fail();
		}
	}
}
