/**
 * 
 */
package examples;
import static kodkod.ast.Expression.INTS;

import java.util.ArrayList;
import java.util.List;

import kodkod.ast.Decls;
import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.IntConstant;
import kodkod.ast.IntExpression;
import kodkod.ast.Relation;
import kodkod.ast.Variable;
import kodkod.engine.Evaluator;
import kodkod.engine.Solution;
import kodkod.engine.Solver;
import kodkod.engine.satlab.SATFactory;
import kodkod.instance.Bounds;
import kodkod.instance.TupleFactory;
import kodkod.instance.TupleSet;
import kodkod.instance.Universe;

/**
 * A KK encoding of the Kuncak hypothesis.
 * @author Emina Torlak
 */
public final class Viktor {
	private final int rows, cols;
	private final Relation[][] a;
	private final Relation[] x;
	private final IntExpression[] b;
	/**
	 * Constructs an instance of Viktor for
	 * the given n.
	 * @requires n > 0
	 */
	public Viktor(int n) {
		rows = n;
		cols = 1<<n;
		a = new Relation[rows][cols];
		x = new Relation[cols];
		b = new IntExpression[n];
		for(int i = 0; i < rows; i++) {
			for(int j = 0; j < cols; j++) {
				a[i][j] = Relation.unary("a"+String.valueOf(i)+String.valueOf(j));
			}
		}
		for(int j = 0; j < cols; j++) {
			x[j] = Relation.unary("x"+j);
		}
		for(int i = 0; i < rows; i++) {
			b[i] = conditionalSum(a[i], x, 0, cols-1);
		}
	}
	
	/**
	 * Returns the sum of the elements in x (conditional on the non-emptiness of a 
	 * given a[i]) located at indices [lo..hi]
	 * @return the sum of cardinalities of the elements in x (conditional on the non-emptiness of a 
	 * given a[i]) located at indices [lo..hi]
	 */
	private static IntExpression conditionalSum(Expression[] a, Expression[] x, int lo, int hi) {
		if (lo>hi)
			return IntConstant.constant(0);
		else if (lo==hi) 
			return a[lo].some().thenElse(x[lo].sum(), IntConstant.constant(0));
		else {
			final int mid = (lo + hi) / 2;
			final IntExpression lsum = conditionalSum(a, x, lo, mid);
			final IntExpression hsum = conditionalSum(a, x, mid+1, hi);
			return lsum.plus(hsum);
		}
	}
	
	/**
	 * Returns a formula constraining all x's to be singletons.
	 * @return a formula constraining all x's to be singletons
	 */
	public final Formula decls() {
		Formula ret = Formula.TRUE;
		for(Relation xj: x) {
			ret =ret.and(xj.one());
		}
		return ret;
	}
	
	/**
	 * Returns the equations to be satisfied.
	 * @return equations to be satisfied.
	 */
	public final Formula equations() {
		
		// each b <= cols-1
		Formula f0 = Formula.TRUE;
		final IntConstant colConst = IntConstant.constant(cols-1);
		for(IntExpression bi: b) {
			f0 = f0.and(bi.lte(colConst));
		}
		
		final Variable[] y = new Variable[rows];
		for(int i = 0; i < rows; i++) {
			y[i] = Variable.unary("y"+i);
		}
		
		Decls decls = y[0].oneOf(INTS);
		for(int i = 1; i < rows; i++)
			decls = decls.and(y[i].oneOf(INTS));
		
		Formula f1 = Formula.TRUE;
		for(int i = 0; i < rows; i++) {
			f1 = f1.and(conditionalSum(a[i], y, 0, rows-1).eq(b[i]));
		}
		f1 = f1.not().forAll(decls);
		
		return f0.and(f1); 
	}
	
	/**
	 * Returns the bounds for the problem.
	 * @return bounds
	 */
	public final Bounds bounds() {
		List<String> atoms = new ArrayList<String>(cols+1);
		for(int i = 0; i < cols; i++) {
			atoms.add(String.valueOf(i));
		}
		atoms.add("a");
		final Universe u = new Universe(atoms);
		final TupleFactory f = u.factory();
		final Bounds b = new Bounds(u);
		
		final TupleSet abound = f.setOf("a");
		for(int i = 0; i < rows; i++) {
			for(int j = 0; j < cols; j++) {
				b.bound(a[i][j], abound);
			}
		}
		final TupleSet xbound = f.range(f.tuple(String.valueOf(0)), f.tuple(String.valueOf(cols-1)));
		for(int j = 0; j < cols; j++) {
			b.bound(x[j], xbound);
			b.boundExactly(j, f.setOf(String.valueOf(j)));
		}
		
		return b;
	}
	
	private static void usage() {
		System.out.println("Usage: java tests.Viktor n");
		System.exit(1);
	}
	
	/**
	 * Usage: java tests.Viktor n
	 */
	public static void main(String[] args) {
		if (args.length < 1)
			usage();
		
		try {
			final int n = Integer.parseInt(args[0]);
			final Viktor model = new Viktor(n);
			
			final Solver solver = new Solver();
			solver.options().setSolver(SATFactory.MiniSat);

			final Formula f = model.decls().and(model.equations());
			final Bounds b = model.bounds();
			System.out.println(f);
			System.out.println(b);
			final Solution sol = solver.solve(f, b);
			final Evaluator eval = new Evaluator(sol.instance(), solver.options());
			
			System.out.println(sol);
			System.out.println(eval.evaluate(f));
		} catch (NumberFormatException nfe) {
			usage();
		}
	}
	
	
}
