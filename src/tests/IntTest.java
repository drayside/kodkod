package tests;

import static kodkod.ast.BinaryIntExpression.Operator.AND;
import static kodkod.ast.BinaryIntExpression.Operator.DIVIDE;
import static kodkod.ast.BinaryIntExpression.Operator.MINUS;
import static kodkod.ast.BinaryIntExpression.Operator.MODULO;
import static kodkod.ast.BinaryIntExpression.Operator.MULTIPLY;
import static kodkod.ast.BinaryIntExpression.Operator.OR;
import static kodkod.ast.BinaryIntExpression.Operator.PLUS;
import static kodkod.ast.BinaryIntExpression.Operator.SHA;
import static kodkod.ast.BinaryIntExpression.Operator.SHL;
import static kodkod.ast.BinaryIntExpression.Operator.SHR;
import static kodkod.ast.BinaryIntExpression.Operator.XOR;
import static kodkod.ast.IntComparisonFormula.Operator.EQ;
import static kodkod.ast.IntComparisonFormula.Operator.GT;
import static kodkod.ast.IntComparisonFormula.Operator.GTE;
import static kodkod.ast.IntComparisonFormula.Operator.LT;
import static kodkod.ast.IntComparisonFormula.Operator.LTE;
import static kodkod.engine.config.Options.IntEncoding.BINARY;
import static kodkod.engine.config.Options.IntEncoding.UNARY;

import java.util.ArrayList;
import java.util.List;

import junit.framework.TestCase;
import kodkod.ast.BinaryIntExpression;
import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.IntComparisonFormula;
import kodkod.ast.IntConstant;
import kodkod.ast.IntExpression;
import kodkod.ast.Relation;
import kodkod.ast.UnaryIntExpression;
import kodkod.ast.Variable;
import kodkod.engine.Evaluator;
import kodkod.engine.Solution;
import kodkod.engine.Solver;
import kodkod.engine.config.Options;
import kodkod.instance.Bounds;
import kodkod.instance.TupleFactory;
import kodkod.instance.TupleSet;
import kodkod.instance.Universe;
import kodkod.util.ints.IntRange;
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
	private final Relation r1;
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
	}

	protected void setUp() throws Exception {
		super.setUp();
		bounds = new Bounds(factory.universe());
		
	}

	protected void tearDown() throws Exception {
		super.tearDown();
	}
	
	private Solution solve(Formula formula) {
//		  System.out.println(formula); 
//		  System.out.println(bounds);
			return solver.solve(formula, bounds);
		
	}

		
	private IntExpression[] constants() {
		final Options options = solver.options();
		final IntRange range = options.integers();
		final int min = range.min(), max = range.max();
		final IntExpression[] vals = new IntExpression[max-min+1];
		for(int i = min; i <= max; i++) {
			vals[i-min] = constant(i);
		}
		return vals;
	}
	
	private IntExpression[] nonConstants() {
		final Options options = solver.options();
			
		final IntRange range = options.integers();
		final int min = range.min(), max = range.max();
		final int size = range.size();
		
		final Relation[] r = new Relation[size];
				
		final TupleFactory f = bounds.universe().factory();
		for(int i = 0; i < size; i++) {
			int arity = i%3 + 1;
			r[i] = Relation.nary("r"+i, arity);
			
			TupleSet b = f.noneOf(arity);
			for(int j = (i/3)*((int)Math.pow(SIZE, arity-1)), jmax = j+size; j < jmax; j++ ) {
				b.add(f.tuple(arity, j%b.capacity()));
			}
			
			bounds.bound(r[i], b);
		}
		
		final IntExpression[] vals = new IntExpression[max-min+1];
		for(int i = 0; i < size; i++) {
			vals[i] = i+min < 0 ? r[i].count().negate() : r[i].count();
		}
		
		return vals;
	}
	
	private static IntExpression constant(int i) { return IntConstant.constant(i); }
	
	private final void testBinOp(BinaryIntExpression.Operator op, IntExpression ei, IntExpression ej, int i, int j, int result, int mask) {
		final IntExpression e = ei.compose(op, ej);
		final Formula f = ei.eq(constant(i)).and(ej.eq(constant(j))).and(e.eq(constant(result)));
		final Solution s = solve(f);
		assertNotNull(s.instance());
		final Evaluator eval = new Evaluator(s.instance(), solver.options());
		assertEquals(result & mask, eval.evaluate(e) & mask);
		
	}
	
	/**
	 * Tests all binary ops for this.solver.options and range of vals.
	 * @requires this.solver.options.intEncoding = binary 
	 * @requires vals contains int expressions that represent all 
	 * integers allowed by this.solver.options, in proper sequence
	 */
	private final void test2sComplementBinOps(IntExpression[] vals) {
		final Options options = solver.options();
		
		final IntRange range = options.integers();
		final int min = range.min(), max = range.max();
		final int mask = ~(-1 << options.bitwidth());
		final int shiftmask = ~(-1 << (32 - Integer.numberOfLeadingZeros(options.bitwidth()-1)));
		
		for(int i = min; i <= max; i++) {
				IntExpression vi = vals[i-min];
			
			for(int j = min; j <= max; j++) {
				
				IntExpression vj = vals[j-min];
				testBinOp(PLUS, vi, vj, i, j, i+j, mask);
				testBinOp(MINUS, vi, vj, i, j, i-j, mask);
				testBinOp(MULTIPLY, vi, vj, i, j, i*j, mask);
				
				if (j!=0) {
					testBinOp(DIVIDE, vi, vj, i, j, i/j, mask);
					testBinOp(MODULO, vi, vj, i, j, i%j, mask);
				}
				
				testBinOp(AND, vi, vj, i, j, i & j, mask);
				testBinOp(OR, vi, vj, i, j, i | j, mask);
				testBinOp(XOR, vi, vj, i, j, i ^ j, mask);
				
				testBinOp(SHL, vi, vj, i, j, i << (j & shiftmask), mask);
				testBinOp(SHR, vi, vj, i, j, (i & mask) >>> (j & shiftmask), mask);
				testBinOp(SHA, vi, vj, i, j, i >> (j & shiftmask), mask);
			}
		}
		
	}
	
	public final void testConstant2sComplementBinOps() {
		test2sComplementBinOps(constants());
	}
	
	public final void testNonConstant2sComplementBinOps() {
		solver.options().setBitwidth(3);
		test2sComplementBinOps(nonConstants());
	}
	
	private final void testUnOp(UnaryIntExpression.Operator op, IntExpression ei, int i, int result, int mask) {
		final IntExpression e = ei.apply(op);
		final Formula f = ei.eq(constant(i)).and(e.eq(constant(result)));
		final Solution s = solve(f);

		assertNotNull(s.instance());
		final Evaluator eval = new Evaluator(s.instance(), solver.options());
		assertEquals(result & mask, eval.evaluate(e) & mask);
		
	}
	
	private final void test2sComplementUnOps(IntExpression[] vals) {
		final Options options = solver.options();
		
		final IntRange range = options.integers();
		final int min = range.min(), max = range.max();
		final int mask = ~(-1 << options.bitwidth());
		
		for(int i = min; i <= max; i++) {
			IntExpression vi = vals[i-min];
			testUnOp(UnaryIntExpression.Operator.MINUS, vi, i, -i, mask);
			testUnOp(UnaryIntExpression.Operator.NOT, vi, i, ~i, mask);
			testUnOp(UnaryIntExpression.Operator.ABS, vi, i, Math.abs(i), mask);
			testUnOp(UnaryIntExpression.Operator.SGN, vi, i, i < 0 ? -1 : i > 0 ? 1 : 0, mask);
		}		
	}
	
	public final void testConstant2sComplementUnOps() {
		test2sComplementUnOps(constants());
	}
	
	public final void testNonConstant2sComplementUnOps() {
		solver.options().setBitwidth(3);
		test2sComplementUnOps(nonConstants());
	}
	
	private final void testCompOp(IntComparisonFormula.Operator op, IntExpression ei, IntExpression ej, int i, int j, boolean result) {
		final Formula e = ei.compare(op, ej);
		final Formula f = ei.eq(constant(i)).and(ej.eq(constant(j))).and(result ? e : e.not());
		final Solution s = solve(f);
		assertNotNull(s.instance());
		final Evaluator eval = new Evaluator(s.instance(), solver.options());
		assertFalse(result ^ eval.evaluate(e));
		
	}
	
	/**
	 * Tests all comparison ops for this.solver.options and range of vals.
	 * @requires this.solver.options.intEncoding = binary 
	 * @requires vals contains int expressions that represent all 
	 * integers allowed by this.solver.options, in proper sequence
	 */
	private final void testComparisonOps(IntExpression[] vals) {
		final Options options = solver.options();
		
		final IntRange range = options.integers();
		final int min = range.min(), max = range.max();
		
		for(int i = min; i <= max; i++) {
				IntExpression vi = vals[i-min];
			
				
			for(int j = min; j <= max; j++) {
				
				IntExpression vj = vals[j-min];
			
				testCompOp(EQ, vi, vj, i, j, i==j);
				testCompOp(LT, vi, vj, i, j, i<j);
				testCompOp(LTE, vi, vj, i, j, i<=j);
				testCompOp(GT, vi, vj, i, j, i>j);
				testCompOp(GTE, vi, vj, i, j, i>=j);
				
			}
		}
		
	}
	
	public final void testConstant2sComplementComparisonOps() {
		testComparisonOps(constants());
	}
	
	public final void testNonConstant2sComplementComparisonOps() {
		solver.options().setBitwidth(3);
		testComparisonOps(nonConstants());
	}
	
	
	
	
	
	private final void testUnaryBinOp(BinaryIntExpression.Operator op, IntExpression ei, IntExpression ej, int i, int j, int result, int max) {
		final IntExpression e = ei.compose(op, ej);
		final Formula f = ei.eq(constant(i)).and(ej.eq(constant(j))).and(e.eq(constant(result)));
		final Solution s = solve(f);
		assertNotNull(s.instance());
		final Evaluator eval = new Evaluator(s.instance(), solver.options());
		assertEquals(Math.min(result, max), eval.evaluate(e));
		
	}
		
	/**
	 * Tests all binary ops for this.solver.options and range of vals.
	 * @requires this.solver.options.intEncoding = binary 
	 * @requires vals contains int expressions that represent all 
	 * integers allowed by this.solver.options, in proper sequence
	 */
	private final void testUnaryBinOps(IntExpression[] vals) {
		final Options options = solver.options();
		
		
		final IntRange range = options.integers();
		final int min = range.min(), max = range.max();

		for(int i = min; i <= max; i++) {
			IntExpression vi = vals[i-min];
			
			for(int j = min; j <= max; j++) {
				
				IntExpression vj = vals[j-min];
				testUnaryBinOp(PLUS, vi, vj, i, j, i+j, max);
				testUnaryBinOp(AND, vi, vj, i, j, Integer.bitCount(~(-1<<i) & ~(-1<<j)), max);
				testUnaryBinOp(OR, vi, vj, i, j, Integer.bitCount(~(-1<<i) | ~(-1<<j)), max);
				
			}
		}
		
	}

	public final void testConstantUnaryBinOps() {
		solver.options().setIntEncoding(Options.IntEncoding.UNARY);
		testUnaryBinOps(constants());
	}
	
	public final void testNonConstantUnaryBinOps() {
		solver.options().setBitwidth(3);
		solver.options().setIntEncoding(Options.IntEncoding.UNARY);
		testUnaryBinOps(nonConstants());
	}
	
	private final void testUnaryUnOp(UnaryIntExpression.Operator op, IntExpression ei, int i, int result, int max) {
		final IntExpression e = ei.apply(op);
		final Formula f = ei.eq(constant(i)).and(e.eq(constant(result)));
		final Solution s = solve(f);

		assertNotNull(s.instance());
		final Evaluator eval = new Evaluator(s.instance(), solver.options());
		assertEquals(Math.min(result, max), eval.evaluate(e));
		
	}
	
	private final void testUnaryUnOps(IntExpression[] vals) {
		final Options options = solver.options();
		
		final IntRange range = options.integers();
		final int min = range.min(), max = range.max();
			
		for(int i = min; i <= max; i++) {
			IntExpression vi = vals[i-min];
			testUnaryUnOp(UnaryIntExpression.Operator.ABS, vi, i, Math.abs(i), max);
			testUnaryUnOp(UnaryIntExpression.Operator.SGN, vi, i, i < 0 ? -1 : i > 0 ? 1 : 0, max);
		}		
	}
	
	public final void testConstantUnaryUnOps() {
		solver.options().setIntEncoding(Options.IntEncoding.UNARY);
		testUnaryUnOps(constants());
	}
	
	public final void testNonConstantUnaryUnOps() {
		solver.options().setBitwidth(3);
		solver.options().setIntEncoding(Options.IntEncoding.UNARY);
		testUnaryUnOps(nonConstants());
	}
	
	public final void testConstantUnaryComparisonOps() {
		solver.options().setIntEncoding(Options.IntEncoding.UNARY);
		testComparisonOps(constants());
	}
	
	public final void testNonConstantUnaryComparisonOps() {
		solver.options().setBitwidth(3);
		solver.options().setIntEncoding(Options.IntEncoding.UNARY);
		testComparisonOps(nonConstants());
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
	
}
