package tests;

import static kodkod.ast.IntComparisonFormula.Operator.EQ;
import static kodkod.ast.IntComparisonFormula.Operator.GT;
import static kodkod.ast.IntComparisonFormula.Operator.GTE;
import static kodkod.ast.IntComparisonFormula.Operator.LT;
import static kodkod.ast.IntComparisonFormula.Operator.LTE;
import static kodkod.engine.Options.IntEncoding.BINARY;
import static kodkod.engine.Options.IntEncoding.UNARY;

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
import kodkod.instance.TupleSet;
import kodkod.instance.Universe;
import kodkod.util.ints.Ints;
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
//		System.out.println(bounds.upperBound(r2).size());
//		System.out.println(bounds.upperBound(r1).size());
		assertNotNull(s.instance());
		assertTrue(op.apply(s.instance().tuples(r2).size(), s.instance().tuples(r1).size()));
		
		f = IntConstant.constant(100000).compare(op, IntConstant.constant(100000));
		s = solve(f);
		assertEquals(Solution.Outcome.TRIVIALLY_SATISFIABLE, s.outcome());
	}
	
	
	public final void testCardEQ() {
		solver.options().setBitwidth(17);
		testCardEqLteGte(UNARY, EQ);
		solver.options().setBitwidth(6);
		testCardEqLteGte(BINARY, EQ);
	}
	
	public final void testCardLTE() {
		solver.options().setBitwidth(17);
		testCardEqLteGte(UNARY, LTE);
		solver.options().setBitwidth(8);
		testCardEqLteGte(BINARY, LTE);
	}
	
	public final void testCardGTE() {
		solver.options().setBitwidth(17);
		testCardEqLteGte(UNARY, GTE);
		solver.options().setBitwidth(8);
		testCardEqLteGte(BINARY, GTE);
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
		solver.options().setBitwidth(8);
		testCardLtGt(BINARY, LT);
	}
	
	public final void testCardGT() {
		testCardLtGt(UNARY, GT);
		solver.options().setBitwidth(8);
		testCardLtGt(BINARY, GT);
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
		solver.options().setBitwidth(17);
		testCardPlus(UNARY);
		solver.options().setBitwidth(8);
		testCardPlus(BINARY);
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
		testCardMinus(BINARY);
	}
	
	public final void testMultiply() {
		solver.options().setBitwidth(6);
		TupleSet r1b = factory.setOf("1", "5");
		bounds.bound(r1, r1b, r1b);
		
		Formula f = r1.count().multiply(IntConstant.constant(0)).eq(IntConstant.constant(0));
		Solution s = solve(f);
		assertNotNull(s.instance());
		
		f = r1.count().multiply(IntConstant.constant(1)).eq(IntConstant.constant(2));
		s = solve(f);
		assertNotNull(s.instance());
		
		f = r1.count().multiply(IntConstant.constant(4)).eq(IntConstant.constant(8));
		s = solve(f);
		assertNotNull(s.instance());
		
		f = r1.count().multiply(IntConstant.constant(-1)).eq(IntConstant.constant(-2));
		s = solve(f);
		assertNotNull(s.instance());
		
		f = r1.count().multiply(IntConstant.constant(-3)).eq(IntConstant.constant(-6));
		s = solve(f);
		assertNotNull(s.instance());
		
		r1b = factory.setOf("1", "5", "14");
		bounds.bound(r1, r1b);
		f = r1.count().multiply(IntConstant.constant(3)).lt(IntConstant.constant(8));
		s = solve(f);
		assertTrue(s.instance().tuples(r1).size() * 3 < 8);
		
		TupleSet r2b = factory.setOf(factory.tuple("1", "1"), factory.tuple("2","5"));
		bounds.bound(r2, r2b);
		f = r1.count().multiply(r2.count()).lt(r2.count());
		s = solve(f);
		assertNotNull(s.instance());
		assertTrue(s.instance().tuples(r1).isEmpty());
		assertFalse(s.instance().tuples(r2).isEmpty());
		
		s = solve(IntConstant.constant(1000).multiply(IntConstant.constant(456)).eq(IntConstant.constant(456000)));
		assertNotNull(s.instance());
		
	}
	
	public final void testDivide() {
		solver.options().setBitwidth(6);
		TupleSet r1b = factory.setOf("1", "5", "9");
		bounds.bound(r1, r1b, r1b);
		
		Formula f = r1.count().divide(IntConstant.constant(1)).eq(IntConstant.constant(3));
		Solution s = solve(f);
		assertNotNull(s.instance());
		
		f = r1.count().divide(IntConstant.constant(3)).eq(IntConstant.constant(1));
		s = solve(f);
		assertNotNull(s.instance());

		f = r1.count().divide(IntConstant.constant(2)).eq(IntConstant.constant(1));
		s = solve(f);
		assertNotNull(s.instance());
		
		f =  r1.count().divide(IntConstant.constant(8)).eq(IntConstant.constant(0));
		s = solve(f);
		assertNotNull(s.instance());
		
		f = r1.count().divide(IntConstant.constant(-3)).eq(IntConstant.constant(-1));
		s = solve(f);
		assertNotNull(s.instance());
		
		f = r1.count().divide(IntConstant.constant(-2)).eq(IntConstant.constant(-1));
		s = solve(f);
		assertNotNull(s.instance());
		
		f = r1.count().divide(IntConstant.constant(-1)).eq(IntConstant.constant(-3));
		s = solve(f);
		assertNotNull(s.instance());	
		
		f = r1.count().divide(IntConstant.constant(-4)).eq(IntConstant.constant(0));
		s = solve(f);
		assertNotNull(s.instance());
		
		f = IntConstant.constant(-3).divide(r1.count()).eq(IntConstant.constant(-1));
		s = solve(f);
		assertNotNull(s.instance());
		
		f = IntConstant.constant(-2).divide(r1.count()).eq(IntConstant.constant(0));
		s = solve(f);
		assertNotNull(s.instance());
		
		f = IntConstant.constant(-4).divide(r1.count()).eq(IntConstant.constant(-1));
		s = solve(f);
		assertNotNull(s.instance());	
		
		r1b = factory.setOf("1", "5", "9", "10", "11");
		bounds.bound(r1, r1b);
		
		TupleSet r2b = factory.setOf(factory.tuple("1", "1"), factory.tuple("2","5"));
		bounds.bound(r2, r2b);
		
		f = r1.count().divide(r2.count()).eq(IntConstant.constant(2));
		s = solve(f);
		assertSame(s.instance().tuples(r1).size() / s.instance().tuples(r2).size(), 2);
		
		f = r1.count().divide(r2.count()).eq(IntConstant.constant(-2));
		s = solve(f);
		assertNull(s.instance());
	}
	
}
