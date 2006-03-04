package tests;

import java.util.ArrayList;
import java.util.List;

import junit.framework.TestCase;
import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.Relation;
import kodkod.ast.Variable;
import kodkod.engine.Solution;
import kodkod.engine.Solver;
import kodkod.engine.TimeoutException;
import kodkod.engine.satlab.SATFactory;
import kodkod.instance.Bounds;
import kodkod.instance.TupleFactory;
import kodkod.instance.Universe;

/**
 * Tests the reduction algorithm for trivially
 * (un)satisfiable formulas, and does limited
 * testing of core extraction.
 * 
 * @author Emina Torlak
 */
public class ReductionAndProofTest extends TestCase {
	private final int USIZE = 10;
	private final TupleFactory factory;
	private final Solver solver;
	private final Relation ra, rb, rab, rba;
	private final Bounds bounds;
	
	public ReductionAndProofTest(String arg0) {
		super(arg0);
		this.solver = new Solver();
		solver.options().setTrackVars(true);
		List<String> atoms = new ArrayList<String>(USIZE);
		for (int i = 0; i < USIZE; i++) {
			atoms.add(""+i);
		}
		final Universe universe = new Universe(atoms);
		this.factory = universe.factory();
		this.ra = Relation.unary("ra");
		this.rb = Relation.unary("rb");
		this.rab = Relation.binary("rab");
		this.rba = Relation.binary("rba");
		this.bounds = new Bounds(universe);
	}
	
	protected void setUp() throws Exception {
		super.setUp();
		bounds.bound(ra, factory.setOf("0","1","2","3","4"));
		bounds.bound(rb, factory.setOf("5","6","7","8","9"));
		bounds.bound(rab, bounds.upperBound(ra).product(bounds.upperBound(rb)));
		bounds.bound(rba, bounds.upperBound(rb).product(bounds.upperBound(ra)));
	}
	
	private Formula reduce(Formula formula) {
		try {
			return solver.solve(formula, bounds).reduction();
		} catch (TimeoutException te) {
			fail("Timed out solving " + formula);
			return null;
		}
	}

	public final void testReduction() {
		Formula f0, f1, f2, f3, f4, f5, f6;
		
		f0 = ra.difference(rb).eq(ra); // T
		assertEquals(f0, reduce(f0));
		
		f1 = rab.join(rba).some();
		assertEquals(f0, reduce(f0.or(f1)));
		
		final Relation 	fa = Relation.unary("fa"), la = Relation.unary("la"),
		 			   	oa = Relation.unary("oa"), ta = Relation.binary("ta");
		bounds.bound(fa, bounds.upperBound(ra));
		bounds.bound(la, bounds.upperBound(ra));
		bounds.bound(oa, bounds.upperBound(ra));
		bounds.bound(ta, bounds.upperBound(ra).product(bounds.upperBound(ra)));
		
		f2 = ta.totalOrder(oa, fa, la);
		f3 = fa.join(ta).no(); // F
		
		assertEquals(f3.and(f2), reduce(f3.and(f0).and(f1).and(f2)));
		 			   	
		f4 = ta.acyclic();
		f5 = ta.closure().intersection(Expression.IDEN).some(); // F
		
		assertEquals(f5.and(f4), reduce(f4.and(f1).and(f0).and(f5)));
		
		bounds.boundExactly(rba, bounds.upperBound(rba));
		f6 = rba.function(ra, rb); // F
		
		assertEquals(f6, reduce(f1.and(f2).and(f4).and(f6)));
		
	}
	
	public final void testProof() {
		Variable v0 = Variable.unary("v0"), v1 = Variable.unary("v1"),
		         v2 = Variable.unary("v2");
		Formula f0 = v0.join(rab).eq(v1.union(v2)).and(v1.eq(v2).not());
		Formula f1 = f0.forSome(v0.oneOf(ra).and(v1.oneOf(rb)).and(v2.oneOf(rb)));
		Formula f2 = rab.function(ra, rb); 
		Formula f3 = f1.and(f2);
		
	    Solution sol = null;
	    
	    try {
	    		solver.options().setTrackVars(false);
			sol = solver.solve(f3, bounds);
			assertEquals(Solution.Outcome.UNSATISFIABLE, sol.outcome());
			try {
				System.out.println(sol.proof().size());
				fail("expected no proof for non-core extracting solver");
			} catch (UnsupportedOperationException uoe) {
				// good
			}
			try {
				System.out.println(sol.proof().variablesFor(ra));
				fail("expected no variables for non-tracking translation");
			} catch (UnsupportedOperationException uoe) {
				// good
			}
			solver.options().setTrackVars(true);
			solver.options().setSkolemize(false);
			sol = solver.solve(f3, bounds);
			try {
				System.out.println(sol.proof().size());
				fail("expected no proof for non-core extracting solver");
			} catch (UnsupportedOperationException uoe) {
				// good
			}
			
			assertSame(5, sol.proof().variablesFor(ra).size());
			assertSame(5, sol.proof().variablesFor(rb).size());
			assertSame(25, sol.proof().variablesFor(rab).size());
			assertSame(1, sol.proof().variablesFor(f1).size());
			assertSame(1, sol.proof().variablesFor(f2).size());
			assertSame(1, sol.proof().variablesFor(f3).size());
			
			solver.options().setSolver(SATFactory.ZChaffPlus);
			sol = solver.solve(f3, bounds);
			assertTrue(sol.proof().size() > 0);
			
		} catch (TimeoutException te) {
			fail("Timed out solving " + f0);
		}
		
	}
	
 }
