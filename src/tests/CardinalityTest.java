package tests;

import java.util.ArrayList;
import java.util.List;

import junit.framework.TestCase;
import kodkod.ast.Formula;
import kodkod.ast.IntComparisonFormula;
import kodkod.ast.IntConstant;
import kodkod.ast.Relation;
import kodkod.engine.Options;
import kodkod.engine.Solution;
import kodkod.engine.Solver;
import kodkod.engine.TimeoutException;
import kodkod.instance.Bounds;
import kodkod.instance.TupleFactory;
import kodkod.instance.Universe;
import kodkod.util.ints.Ints;

import static kodkod.ast.IntComparisonFormula.Operator.*;
import static kodkod.engine.Options.IntEncoding.*;
/**
 * Tests translation of cardinality expressions/formulas.
 * 
 * @author Emina Torlak
 */
public class CardinalityTest extends TestCase {
	private static final int SIZE = 16;
	private final TupleFactory factory;
	private final Solver solver;
	private final Relation r1, r2, r3;
	private Bounds bounds;
	
	public CardinalityTest(String arg0) {
		super(arg0);
		this.solver = new Solver();
		List<String> atoms = new ArrayList<String>(SIZE);
		for (int i = 0; i < SIZE; i++) {
			atoms.add(String.valueOf(i));
		}
		final Universe universe = new Universe(atoms);
		this.factory = universe.factory();
		r1 = Relation.unary("r1");
		r2 = Relation.binary("r2");
		r3 = Relation.ternary("r3");
	}

	protected void setUp() throws Exception {
		super.setUp();
		bounds = new Bounds(factory.universe());
		
	}

	protected void tearDown() throws Exception {
		super.tearDown();
	}
	
	private Solution solve(Formula formula) {
		try {
			return solver.solve(formula, bounds);
		} catch (TimeoutException te) {
			fail("Timed out solving " + formula);
			return null;
		}
	}

	/**
	 * @requires op in { EQ, LTE, GTE }
	 */
	private final void testCardEqLteGte(Options.IntEncoding encoding, IntComparisonFormula.Operator op) {
		solver.options().setIntEncoding(encoding);
		bounds.bound(r1, factory.setOf("15"), factory.setOf("15"));
		Formula f = r1.count().compare(op, IntConstant.constant(1));
		Solution s = solve(f);
		assertNotNull(s.instance());
		assertEquals(Ints.singleton(15), s.instance().tuples(r1).indexView());
		
		f = r1.count().compare(op, IntConstant.constant(op==GTE ? 2 : 0));
		s = solve(f);
		assertNull(s.instance());
		
		bounds.bound(r1, factory.setOf("15"), factory.allOf(1));
		f = IntConstant.constant(2).compare(op, r1.count());
		s = solve(f);
		assertNotNull(s.instance());
		assertTrue(s.instance().tuples(r1).indexView().contains(15));
		assertTrue(op.apply(2, s.instance().tuples(r1).size()));
		
		bounds.bound(r1, factory.allOf(1));
		f = r1.count().compare(op, IntConstant.constant(3));
		s = solve(f);
		assertNotNull(s.instance());
		assertTrue(op.apply(s.instance().tuples(r1).size(), 3));
		
		bounds.bound(r2, factory.area(factory.tuple("0","8"), factory.tuple("8","15")));
		
		f = r2.count().compare(op, IntConstant.constant(SIZE));
		s = solve(f);
//		System.out.println(s.stats());
		assertNotNull(s.instance());   
		assertTrue(op.apply(s.instance().tuples(r2).size(), SIZE));
		
		f = r2.count().compare(op, r1.count());
		s = solve(f);
//		System.out.println(s.stats());
		assertNotNull(s.instance());
		assertTrue(op.apply(s.instance().tuples(r2).size(), s.instance().tuples(r1).size()));
		
		f = IntConstant.constant(100000).compare(op, IntConstant.constant(100000));
		s = solve(f);
		assertEquals(Solution.Outcome.TRIVIALLY_SATISFIABLE, s.outcome());
	}
	
	
	public final void testCardEQ() {
		testCardEqLteGte(UNARY, EQ);
		testCardEqLteGte(BINARY, EQ);
		testCardEqLteGte(TWOS_COMPLEMENT, EQ);	
	}
	
	public final void testCardLTE() {
		testCardEqLteGte(UNARY, LTE);
		testCardEqLteGte(BINARY, LTE);
		testCardEqLteGte(TWOS_COMPLEMENT, LTE);	
	}
	
	public final void testCardGTE() {
		testCardEqLteGte(UNARY, GTE);
		testCardEqLteGte(BINARY, GTE);
		testCardEqLteGte(TWOS_COMPLEMENT, GTE);	
	}
	
	/**
	 * @requires op in { LT, GT }
	 */
	private final void testCardLtGt(Options.IntEncoding encoding, IntComparisonFormula.Operator op) {
		solver.options().setIntEncoding(encoding);
		bounds.bound(r1, factory.setOf("14"), factory.setOf("13", "14", "15"));
		Formula f = r1.count().compare(op, IntConstant.constant(2));
		Solution s = solve(f);
		assertNotNull(s.instance());
		assertTrue(op.apply(s.instance().tuples(r1).size(), 2));
		
		f = IntConstant.constant(2).compare(op, r1.count());
		s = solve(f);
		assertNotNull(s.instance());
		assertTrue(op.apply(2, s.instance().tuples(r1).size()));
		
		f = r1.count().compare(op, IntConstant.constant(op==GT ? 3 : 1));
		s = solve(f);
		assertNull(s.instance());
		
		bounds.bound(r2, factory.area(factory.tuple("0","8"), factory.tuple("8","15")));
		bounds.bound(r3, factory.area(factory.tuple("0","8","14"), factory.tuple("1","15","15")));
		
		f = r2.count().compare(op, r3.count());
		s = solve(f);
//		System.out.println(s.stats());
		assertNotNull(s.instance());
		assertTrue(op.apply(s.instance().tuples(r2).size(), s.instance().tuples(r3).size()));
		
		f = IntConstant.constant(100000).compare(op, IntConstant.constant(100000 + (op==GT ? -1 : 1)));
		s = solve(f);
		assertEquals(Solution.Outcome.TRIVIALLY_SATISFIABLE, s.outcome());
	}
	
	public final void testCardLT() {
		testCardLtGt(UNARY, LT);
		testCardLtGt(BINARY, LT);
		testCardLtGt(TWOS_COMPLEMENT, LT);	
	}
	
	public final void testCardGT() {
		testCardLtGt(UNARY, GT);
		testCardLtGt(BINARY, GT);
		testCardLtGt(TWOS_COMPLEMENT, GT);	
	}
	
	private final void testCardPlus(Options.IntEncoding encoding) {
		solver.options().setIntEncoding(encoding);
		bounds.bound(r1, factory.setOf("15"), factory.setOf("15"));
		Formula f = r1.count().plus(IntConstant.constant(1)).eq(IntConstant.constant(2));
		Solution s = solve(f);
		assertNotNull(s.instance());
		assertEquals(Ints.singleton(15), s.instance().tuples(r1).indexView());
		
		f = r1.count().plus(IntConstant.constant(10)).lt(IntConstant.constant(11));
		s = solve(f);
		assertNull(s.instance());
		
		bounds.bound(r1, factory.setOf("15"), factory.allOf(1));
		f = IntConstant.constant(3).plus(IntConstant.constant(2)).eq(r1.count());
		s = solve(f);
		assertNotNull(s.instance());
		assertTrue(s.instance().tuples(r1).indexView().contains(15));
		assertEquals(5, s.instance().tuples(r1).size());

		bounds.bound(r2, factory.area(factory.tuple("0","8"), factory.tuple("8","15")));
	
		f = r2.count().plus(r1.count()).gte(IntConstant.constant(7));
		s = solve(f);
//		System.out.println(s.stats());
		assertNotNull(s.instance());
		assertTrue(s.instance().tuples(r2).size() + s.instance().tuples(r1).size() >= 7);
		
		f = IntConstant.constant(100000).plus(IntConstant.constant(9)).eq(IntConstant.constant(100009));
		s = solve(f);
		assertEquals(Solution.Outcome.TRIVIALLY_SATISFIABLE, s.outcome());
	}
	
	public final void testCardPlus() {
		testCardPlus(UNARY);
		testCardPlus(BINARY);
		testCardPlus(TWOS_COMPLEMENT);
	}
	
	private final void testCardMinus(Options.IntEncoding encoding) {
		solver.options().setIntEncoding(encoding);
		bounds.bound(r1, factory.setOf("15"), factory.setOf("15"));
		Formula f = r1.count().minus(IntConstant.constant(2)).eq(IntConstant.constant(-1));
		Solution s = solve(f);
		assertNotNull(s.instance());
		assertEquals(Ints.singleton(15), s.instance().tuples(r1).indexView());
		
		f = r1.count().plus(IntConstant.constant(1)).lt(IntConstant.constant(0));
		s = solve(f);
		assertNull(s.instance());
		
		bounds.bound(r1, factory.setOf("15"), factory.allOf(1));
		f = IntConstant.constant(23).minus(IntConstant.constant(20)).eq(r1.count());
		s = solve(f);
		assertNotNull(s.instance());
		assertTrue(s.instance().tuples(r1).indexView().contains(15));
		assertEquals(3, s.instance().tuples(r1).size());

		bounds.bound(r2, factory.area(factory.tuple("0","8"), factory.tuple("8","15")));
	
		f = r2.count().minus(r1.count()).gte(IntConstant.constant(7));
		s = solve(f);
//		System.out.println(s.stats());
		assertNotNull(s.instance());
		assertTrue(s.instance().tuples(r2).size() + s.instance().tuples(r1).size() >= 7);
		
		f = IntConstant.constant(100000).minus(IntConstant.constant(9)).eq(IntConstant.constant(99991));
		s = solve(f);
		assertEquals(Solution.Outcome.TRIVIALLY_SATISFIABLE, s.outcome());
	}
	
	public final void testCardMinus() {
		try {
			testCardMinus(UNARY);
			fail();
		} catch (UnsupportedOperationException unused) {	}
		try {
			testCardMinus(BINARY);
			fail();
		} catch (UnsupportedOperationException unused) { }
		testCardMinus(TWOS_COMPLEMENT);
	}
}
