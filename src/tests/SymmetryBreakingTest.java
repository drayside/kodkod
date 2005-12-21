package tests;

import static relcalc.ast.Relation.Property.ACYCLIC;
import static relcalc.ast.Relation.Property.TOTALLY_ORDERED;

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;

import junit.framework.TestCase;
import relcalc.ast.Formula;
import relcalc.ast.Relation;
import relcalc.engine.Solver;
import relcalc.engine.TimeoutException;
import relcalc.instance.Bounds;
import relcalc.instance.Instance;
import relcalc.instance.TupleFactory;
import relcalc.instance.TupleSet;
import relcalc.instance.Universe;

/** 
 * Tests symmetry breaking code.
 *
 * @author Emina Torlak 
 */
public class SymmetryBreakingTest extends TestCase {
	private static final int USIZE = 10;
	private final TupleFactory factory;
	private final Solver solver;
	private final Relation to1, to2, to3, ac1, ac2, ac3, r1, r2;
	private Bounds bounds;
	
	public SymmetryBreakingTest(String arg0) {
		super(arg0);
		this.solver = new Solver(Solver.SATSolverName.Mini3SAT);
		
		List<String> atoms = new ArrayList<String>(USIZE);
		for (int i = 0; i < USIZE; i++) {
			atoms.add(""+i);
		}
		final Universe universe = new Universe(atoms);
		this.factory = universe.factory();
		
		to1 = Relation.binary("to1", EnumSet.of(TOTALLY_ORDERED));
		to2 = Relation.binary("to2", EnumSet.of(TOTALLY_ORDERED));
		to3 = Relation.binary("to3", EnumSet.of(TOTALLY_ORDERED));
		ac1 = Relation.binary("ac1", EnumSet.of(ACYCLIC));
		ac2 = Relation.binary("ac2", EnumSet.of(ACYCLIC));
		ac3 = Relation.binary("ac3", EnumSet.of(ACYCLIC));
		r1 = Relation.unary("r1");
		r2 = Relation.binary("r2");
		
		
	}
	
	protected void setUp() throws Exception {
		super.setUp();
		bounds = new Bounds(factory.universe());		
	}

	
	private Instance solve(Formula f, Bounds b) {
		try {
			return solver.solve(f, b);
		} catch (TimeoutException te) {
			fail("Timed out solving " + f);
			return null;
		}
	}
	
	private Instance solve(Formula f) {
		return solve(f, bounds);
	}
		
	private void assertPrimVarNum(int primVars) {
		assertEquals(primVars, solver.numberOfPrimaryVariables());
	}
	
	private void assertAuxVarNum(int auxVars) {
		assertEquals(auxVars, solver.numberOfIntermediateVariables());
	}
	
	private void assertClauseNum(int clauses) {
		assertEquals(clauses, solver.numberOfClauses());
	}
	
	public void testTotalOrdering() {
		bounds.bound(to1, factory.area(factory.tuple("0","0"), factory.tuple("4","4")));
		assertNotNull(solve(to1.some()));
		assertPrimVarNum(0); assertAuxVarNum(0); assertClauseNum(0);
		
		bounds.bound(r1, factory.range(factory.tuple("0"), factory.tuple("4")));
		assertNotNull(solve(to1.join(r1).some()));
		assertPrimVarNum(bounds.upperBound(r1).size());
		
		bounds.bound(to2, factory.setOf(factory.tuple("5","7"),factory.tuple("5","6"), factory.tuple("7","8"), 
				                        factory.tuple("7","8"), factory.tuple("8","9")));
		assertNotNull(solve(to1.difference(to2).some()));
		assertPrimVarNum(0); assertAuxVarNum(0); assertClauseNum(0);
		
		bounds.bound(to3, factory.allOf(2));
		assertNotNull(solve(to3.product(to1).some()));
		assertPrimVarNum(bounds.upperBound(to1).size());
		
		assertNotNull(solve(to3.union(to1).join(to2).some()));
		assertPrimVarNum(bounds.upperBound(to1).size() + bounds.upperBound(to2).size());
	}
	
	public void testAcyclic() {
		bounds.bound(ac1, factory.area(factory.tuple("0","0"), factory.tuple("4","4")));
		assertNotNull(solve(ac1.some()));
		assertPrimVarNum(10);
		
		bounds.bound(r1, factory.range(factory.tuple("1"), factory.tuple("3")));
		assertNotNull(solve(ac1.join(r1).some()));
		assertPrimVarNum(10 + bounds.upperBound(r1).size());
		
		bounds.bound(r2, factory.setOf(factory.tuple("2", "3")), factory.allOf(2));
		
		bounds.bound(ac2, factory.setOf(factory.tuple("5","6"),factory.tuple("6","5"), factory.tuple("7","8"), 
                                        factory.tuple("8","7"), factory.tuple("8","9"), factory.tuple("9", "8")));
		assertNotNull(solve(ac1.difference(ac2).some()));
		assertPrimVarNum(10 + 3);
		
		assertNotNull(solve(ac1.difference(ac2).join(r2).some()));
		assertPrimVarNum(bounds.upperBound(ac1).size() + bounds.upperBound(r2).size() - 1 + 3);
		
		final TupleSet ac3Bound = factory.allOf(2);
		ac3Bound.remove(factory.tuple("9", "8"));
		bounds.bound(ac3, ac3Bound);
		
		assertNotNull(solve(ac1.difference(ac2).union(ac3).some()));
		assertPrimVarNum(ac3Bound.size() + 10 + 3);
		
		bounds.bound(to3, factory.allOf(2));
		assertNotNull(solve(to3.product(ac1).some()));
		assertPrimVarNum(bounds.upperBound(ac1).size());
	}
	
}
