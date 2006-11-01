package tests;

import static kodkod.ast.IntComparisonFormula.Operator.EQ;
import static kodkod.ast.IntComparisonFormula.Operator.GT;
import static kodkod.ast.IntComparisonFormula.Operator.GTE;
import static kodkod.ast.IntComparisonFormula.Operator.LT;
import static kodkod.ast.IntComparisonFormula.Operator.LTE;
import static kodkod.engine.settings.Options.IntEncoding.BINARY;
import static kodkod.engine.settings.Options.IntEncoding.UNARY;

import java.util.ArrayList;
import java.util.List;

import junit.framework.TestCase;
import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.IntComparisonFormula;
import kodkod.ast.IntConstant;
import kodkod.ast.Relation;
import kodkod.ast.Variable;
import kodkod.engine.Solution;
import kodkod.engine.Solver;
import kodkod.engine.settings.Options;
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
public class IntTest extends TestCase {
	private static final int SIZE = 16;
	private final TupleFactory factory;
	private final Solver solver;
	private final Relation r1, r2, r3;
	private Bounds bounds;
	
	public IntTest(String arg0) {
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
		
			return solver.solve(formula, bounds);
		
	}

	/**
	 * @requires op in { EQ, LTE, GTE }
	 */
	private final void testEqLteGte(Options.IntEncoding encoding, IntComparisonFormula.Operator op) {
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
	
	
	public final void testEQ() {
		solver.options().setBitwidth(17);
		testEqLteGte(UNARY, EQ);
		solver.options().setBitwidth(6);
		testEqLteGte(BINARY, EQ);
	}
	
	public final void testLTE() {
		solver.options().setBitwidth(17);
		testEqLteGte(UNARY, LTE);
		solver.options().setBitwidth(8);
		testEqLteGte(BINARY, LTE);
	}
	
	public final void testGTE() {
		solver.options().setBitwidth(17);
		testEqLteGte(UNARY, GTE);
		solver.options().setBitwidth(8);
		testEqLteGte(BINARY, GTE);
	}
	
	/**
	 * @requires op in { LT, GT }
	 */
	private final void testLtGt(Options.IntEncoding encoding, IntComparisonFormula.Operator op) {
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
	
	public final void testLT() {
		testLtGt(UNARY, LT);
		solver.options().setBitwidth(8);
		testLtGt(BINARY, LT);
	}
	
	public final void testGT() {
		testLtGt(UNARY, GT);
		solver.options().setBitwidth(8);
		testLtGt(BINARY, GT);
	}
	
	private final void testPlus(Options.IntEncoding encoding) {
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
	
	public final void testPlus() {
		solver.options().setBitwidth(17);
		testPlus(UNARY);
		solver.options().setBitwidth(8);
		testPlus(BINARY);
	}
	
	private final void testMinus(Options.IntEncoding encoding) {
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
	
	public final void testMinus() {
		try {
			testMinus(UNARY);
			fail();
		} catch (UnsupportedOperationException unused) {	}
		testMinus(BINARY);
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
	
	public void testSum() {
		solver.options().setBitwidth(6);
		TupleSet r1b = factory.setOf("1", "5", "9");
		bounds.bound(r1, r1b, r1b);
		
		
		Formula f = r1.sum().eq(IntConstant.constant(0));
		Solution s = solve(f);
		assertNotNull(s.instance());
		
		bounds.boundExactly(5, factory.setOf(factory.tuple("5")));
		
		f = r1.sum().eq(IntConstant.constant(5));
		s = solve(f);
		assertNotNull(s.instance());
		
		bounds.boundExactly(2, factory.setOf(factory.tuple("2")));
		
		f = r1.sum().eq(IntConstant.constant(5));
		s = solve(f);
		assertNotNull(s.instance());
		
		bounds.boundExactly(1, factory.setOf(factory.tuple("9")));
		f = r1.sum().eq(IntConstant.constant(6));
		s = solve(f);
		assertNotNull(s.instance());
		
		bounds.boundExactly(-8, factory.setOf(factory.tuple("1")));
		f = r1.sum().eq(IntConstant.constant(-2));
		s = solve(f);
		assertNotNull(s.instance());
		
		bounds.bound(r1, r1b);
		f = r1.sum().eq(IntConstant.constant(-2));
		s = solve(f);
		assertNotNull(s.instance());
		assertEquals(s.instance().tuples(r1), r1b);
	}
	
	public void testIntCast() {
		solver.options().setBitwidth(6);
		TupleSet r1b = factory.setOf("1", "5", "9");
		bounds.bound(r1, r1b, r1b);
		
		Formula f = r1.sum().toExpression().eq(Expression.NONE);
		Solution s = solve(f);
		assertNotNull(s.instance());
		
		bounds.boundExactly(5, factory.setOf(factory.tuple("5")));
		f = r1.sum().toExpression().eq(IntConstant.constant(5).toExpression());
		s = solve(f);
		assertNotNull(s.instance());
		
		bounds.boundExactly(1, factory.setOf(factory.tuple("1")));
		bounds.boundExactly(6, factory.setOf(factory.tuple("6")));
		
		f = r1.sum().toExpression().eq(IntConstant.constant(6).toExpression());
		s = solve(f);
		assertNotNull(s.instance());
		
		bounds.bound(r1, r1b);
		f = r1.sum().toExpression().eq(IntConstant.constant(6).toExpression());
		s = solve(f);
		assertNotNull(s.instance());
		
		bounds.boundExactly(6, factory.setOf(factory.tuple("1")));
		f = r1.sum().toExpression().eq(IntConstant.constant(6).toExpression());
		s = solve(f);
		assertNull(s.instance());
		
	}
	
	private void testIfIntExpr(Options.IntEncoding encoding) {
		solver.options().setIntEncoding(encoding);
		bounds.bound(r1, factory.setOf("15"), factory.setOf("15"));
		Formula f = (r1.some().thenElse(r1.count(), IntConstant.constant(5))).eq(IntConstant.constant(1));
		Solution s = solve(f);
		assertNotNull(s.instance());
		assertEquals(Ints.singleton(15), s.instance().tuples(r1).indexView());
		
		f = (r1.some().thenElse(r1.sum(), IntConstant.constant(5))).eq(IntConstant.constant(1));
		s = solve(f);
		assertNull(s.instance());
		
		bounds.bound(r1, factory.setOf("3"), factory.allOf(1));
		bounds.boundExactly(3, factory.setOf("3"));
		bounds.boundExactly(1, factory.setOf("1"));
		f = ((r1.count().eq(IntConstant.constant(2))).thenElse(r1.sum(), IntConstant.constant(5))).eq(IntConstant.constant(4));
		s = solve(f);
		assertNotNull(s.instance());
		assertTrue(s.instance().tuples(r1).indexView().contains(1));
		assertTrue(s.instance().tuples(r1).indexView().contains(3));
		assertEquals(2, s.instance().tuples(r1).size());
		
		f = Formula.TRUE.thenElse(IntConstant.constant(2), IntConstant.constant(3)).eq(IntConstant.constant(4));
		s = solve(f);
		assertEquals(Solution.Outcome.TRIVIALLY_UNSATISFIABLE, s.outcome());
		
		f = Formula.FALSE.thenElse(IntConstant.constant(2), IntConstant.constant(3)).eq(IntConstant.constant(3));
		s = solve(f);
		assertEquals(Solution.Outcome.TRIVIALLY_SATISFIABLE, s.outcome());
	}
	
	public void testIfIntExpr() {
		solver.options().setBitwidth(17);
		testIfIntExpr(UNARY);
		solver.options().setBitwidth(8);
		testIfIntExpr(BINARY);
	}
	
	private void testIntSum(Options.IntEncoding encoding) {
		solver.options().setIntEncoding(encoding);
		final Variable x = Variable.unary("x");
		bounds.bound(r1, factory.setOf("13","14","15"), factory.setOf("13","14","15"));
		Formula f = IntConstant.constant(3).eq(IntConstant.constant(1).sum(x.oneOf(r1)));
		Solution s = solve(f);
		
		assertNotNull(s.instance());
		bounds.bound(r1, factory.noneOf(1), factory.setOf("1","3","5"));
		bounds.boundExactly(1, factory.setOf("1"));
		bounds.boundExactly(3, factory.setOf("3"));
		bounds.boundExactly(5, factory.setOf("5"));
		
		f = IntConstant.constant(9).eq(x.sum().sum(x.oneOf(r1)));
		s = solve(f);
		assertNotNull(s.instance());
		assertEquals(s.instance().tuples(r1), factory.setOf("1","3","5"));
	}
	
	public void testIntSum() {
		solver.options().setBitwidth(17);
		testIntSum(UNARY);
		solver.options().setBitwidth(8);
		testIntSum(BINARY);
	}
	
	public void testAndOrXor() {
		Formula f = IntConstant.constant(1).and(IntConstant.constant(2)).eq(IntConstant.constant(0));
		Solution s = solve(f);
		assertNotNull(s.instance());
		bounds.bound(r1, factory.setOf("13","14","15"));
		f = IntConstant.constant(1).xor(r1.count()).eq(IntConstant.constant(2));
		s = solve(f);
		assertEquals(s.instance().tuples(r1).size(), 3);
		bounds.bound(r1, factory.setOf("12","13","14","15"));
		bounds.bound(r2, factory.setOf(factory.tuple("1", "1"), factory.tuple("2","5")));
		f = r2.count().or(r1.count()).eq(IntConstant.constant(5));
		s = solve(f);
		assertEquals(s.instance().tuples(r1).size(), 4);
		assertEquals(s.instance().tuples(r2).size(), 1);
	}
	
	public void testShifts() {
		bounds.bound(r1, factory.setOf("9","10","11","12","13","14","15"));
		Formula f = IntConstant.constant(1).shl(r1.count()).eq(IntConstant.constant(4));
		Solution s = solve(f);
		assertEquals(s.instance().tuples(r1).size(), 2);
		f = IntConstant.constant(1).shl(r1.count()).eq(IntConstant.constant(0));
		s = solve(f);
		assertTrue(s.instance().tuples(r1).size() >= 5);
		
		f = IntConstant.constant(-12).shr(r1.count()).eq(IntConstant.constant(5));
		s = solve(f);
		assertTrue(s.instance().tuples(r1).size() == 2);
		
		f = IntConstant.constant(-16).sha(r1.count()).eq(IntConstant.constant(0));
		s = solve(f);
		assertNull(s.instance());
		
		f = IntConstant.constant(-16).sha(r1.count()).eq(IntConstant.constant(-1));
		s = solve(f);
		assertTrue(s.instance().tuples(r1).size() >= 4);
	
	}
	
}
