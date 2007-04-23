package tests;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import junit.framework.TestCase;
import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.Relation;
import kodkod.ast.Variable;
import kodkod.engine.Solution;
import kodkod.engine.Solver;
import kodkod.engine.satlab.SATFactory;
import kodkod.engine.ucore.EmptyClauseConeStrategy;
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
	private final Relation fa, la, oa, ta;
	private final Bounds bounds;
	
	public ReductionAndProofTest(String arg0) {
		super(arg0);
		this.solver = new Solver();
		solver.options().setLogTranslation(true);
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
		this.fa = Relation.unary("fa");
		this.la = Relation.unary("la");
		this.oa = Relation.unary("oa");
		this.ta = Relation.binary("ta");
		this.bounds = new Bounds(universe);
	}
	
	protected void setUp() throws Exception {
		super.setUp();
		bounds.bound(ra, factory.setOf("0","1","2","3","4"));
		bounds.bound(rb, factory.setOf("5","6","7","8","9"));
		bounds.bound(rab, bounds.upperBound(ra).product(bounds.upperBound(rb)));
		bounds.bound(rba, bounds.upperBound(rb).product(bounds.upperBound(ra)));
		
		bounds.boundExactly(fa, factory.setOf("0"));
		bounds.boundExactly(la, factory.setOf("4"));
		bounds.boundExactly(oa, bounds.upperBound(ra));
		bounds.boundExactly(ta, factory.setOf(factory.tuple("0","1"), factory.tuple("1","2"),
				factory.tuple("2","3"), factory.tuple("3","4")));
	}
	
	private Set<Formula> reduce(Formula formula) {
		
			//return solver.solve(formula, bounds).reduction();
		final Solution sol = solver.solve(formula, bounds);
		assertEquals(Solution.Outcome.TRIVIALLY_UNSATISFIABLE, sol.outcome());
		return sol.proof().highLevelCore();
		
	}
	


	public final void testReduction() {
		Formula f0, f1, f2, f3, f4, f5, f6;
		
		f0 = ra.difference(rb).eq(ra); // T
		assertEquals(Solution.Outcome.TRIVIALLY_SATISFIABLE, solver.solve(f0, bounds).outcome());
		
		f1 = rab.join(rba).some();
		assertEquals(Solution.Outcome.TRIVIALLY_SATISFIABLE, solver.solve(f0.or(f1), bounds).outcome());
		
		f2 = ta.totalOrder(oa, fa, la);
		f3 = fa.join(ta).no(); // F
		
		Set<Formula> reduction = reduce(f3.and(f0).and(f1).and(f2));
		assertEquals(1, reduction.size());
		assertTrue(reduction.contains(f3));
				 			   	
		f4 = ta.acyclic();
		f5 = ta.closure().intersection(Expression.IDEN).some(); // F
		
		reduction = reduce(f4.and(f1).and(f0).and(f5));
		assertEquals(1, reduction.size());
		assertTrue(reduction.contains(f5));
				
		bounds.boundExactly(rba, bounds.upperBound(rba));
		f6 = rba.function(ra, rb); // F
		
		reduction = reduce(f1.and(f2).and(f6));
		assertEquals(1, reduction.size());
		assertTrue(reduction.contains(f6));
		
	}
	
	public final void testProof() {
		Variable v0 = Variable.unary("v0"), v1 = Variable.unary("v1"),
		         v2 = Variable.unary("v2");
		Formula f0 = v0.join(rab).eq(v1.union(v2)).and(v1.eq(v2).not());
		Formula f1 = f0.forSome(v0.oneOf(ra).and(v1.oneOf(rb)).and(v2.oneOf(rb)));
		Formula f2 = rab.function(ra, rb); 
		Formula f3 = f1.and(f2).and(ta.totalOrder(oa, fa, la));
		
	    Solution sol = null;
	    
	
	    	solver.options().setLogTranslation(false);
			sol = solver.solve(f3, bounds);
			assertEquals(Solution.Outcome.UNSATISFIABLE, sol.outcome());
			assertNull(sol.proof());
			solver.options().setLogTranslation(true);
			sol = solver.solve(f3, bounds);
			assertNull(sol.proof());
			
			solver.options().setSolver(SATFactory.MiniSatProver);
			sol = solver.solve(f3, bounds);
			
			//System.out.println(f3 + ", " + bounds);

			sol.proof().minimize(new EmptyClauseConeStrategy());
			final Set<Formula> top = sol.proof().highLevelCore();
			assertEquals(2, top.size());
			assertTrue(top.contains(f1));
			assertTrue(top.contains(f2));
//			for(Iterator<TranslationLog.Record> itr = sol.proof().core(); itr.hasNext(); ) {
//				System.out.println(itr.next());
//			}
			
	}
	
	
 }
