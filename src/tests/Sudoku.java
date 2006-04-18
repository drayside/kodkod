package tests;

import static kodkod.ast.Expression.UNIV;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.Relation;
import kodkod.ast.Variable;
import kodkod.engine.Solution;
import kodkod.engine.Solver;
import kodkod.engine.TimeoutException;
import kodkod.engine.satlab.SATFactory;
import kodkod.instance.Bounds;
import kodkod.instance.Tuple;
import kodkod.instance.TupleFactory;
import kodkod.instance.TupleSet;
import kodkod.instance.Universe;
/**
 * Sudoku puzzle as a partial instance problem
 */
public final class Sudoku {

	private final Relation puzzle;
	private final Relation[] regions;
	
	
	/**
	 * Creates an nxn sudoku grid
	 * @requires n > 0
	 */
	Sudoku(int n) {
		assert n > 0;
		puzzle = Relation.ternary("puzzle");
		regions = new Relation[n];
		for(int i = 0; i < n; i++) {
			regions[i] = Relation.unary("region"+i);
		}
	}

	final Formula declarations() {
		final Variable v1 = Variable.unary("v1");
		final Variable v2 = Variable.unary("v2");
		return v2.join(v1.join(puzzle)).one().forAll(v1.oneOf(UNIV).and(v2.oneOf(UNIV)));
	}
	
	final Formula oneInEachRow() {
		final Variable r = Variable.unary("r");
		return UNIV.join(r.join(puzzle)).eq(UNIV).forAll(r.oneOf(UNIV));
	}
	
	final Formula oneInEachColumn() {
		final Variable c = Variable.unary("c");
		return c.join(UNIV.join(puzzle)).eq(UNIV).forAll(c.oneOf(UNIV));
	}
	
	final Formula oneInEachRegion() {
		Formula ret = Formula.TRUE;
		for(Relation x: regions) {
			for(Relation y: regions) {
				ret = ret.and(UNIV.eq(y.join(x.join(puzzle))));
			}
		}
		return ret;
	}
	
	final Formula alterRules() {
		Formula ret = oneInEachRegion();
		final Variable x = Variable.unary("x");
		final Variable y = Variable.unary("y");
		final Expression xy = y.join(x.join(puzzle));
		final Expression xdiffy = (UNIV.difference(y)).join(x.join(puzzle));
		final Expression ydiffx = y.join(UNIV.difference(x).join(puzzle));
		Formula f = xy.some().and(xy.intersection(xdiffy.union(ydiffx)).no());
		return ret.and(f.forAll(x.oneOf(UNIV).and(y.oneOf(UNIV))));
	}
	
	final Formula rules() {
		return declarations().and(oneInEachRow()).and(oneInEachColumn()).and(oneInEachRegion());
	}
	
	/**
	 * Returns bounds for the following puzzle:
	 * 
	 * <pre>
	 *        0 1 2   3 4 5   6 7 8
	 * 	   +------------------------+
	 * 	 0 |  1     | 2     | 3     |
	 * 	 1 |    2   |   3   |   4   |
	 * 	 2 |      3 |     4 |     5 |
	 * 	   | -------+-------+------ |
	 * 	 3 |  6     | 4     | 5     |
	 * 	 4 |    7   |   5   |   6   |
	 * 	 5 |      8 |     6 |     7 |
	 * 	   | -------+-------+------ |
	 * 	 6 |  8     | 0     | 7     |
	 * 	 7 |    0   |   1   |   8   |
	 * 	 8 |      1 |     2 |     4 |
	 * 	   +------------------------+
	 * </pre>
	 * 
	 * @return bounds for the puzzle
	 */
	final Bounds puzzle1() {
		final int n = regions.length, nsq = n*n;
		assert n==3;
		final List<String> nums = new ArrayList<String>(nsq);
		for(int i = 0; i < nsq; i++)
			nums.add(String.valueOf(i));
		final Universe u = new Universe(nums);
		final Bounds b = new Bounds(u);
		final TupleFactory f = u.factory();
		for(int i = 0; i < n; i++) {
			b.boundExactly(regions[i], f.range(f.tuple(String.valueOf(i*3)), f.tuple(String.valueOf(i*3+2))));
		}
		final TupleSet start = f.noneOf(3);
		start.add(f.tuple("0","0","1"));
		start.add(f.tuple("0","3","2"));
		start.add(f.tuple("0","6","3"));
		
		start.add(f.tuple("1","1","2"));
		start.add(f.tuple("1","4","3"));
		start.add(f.tuple("1","7","4"));
		
		start.add(f.tuple("2","2","3"));
		start.add(f.tuple("2","5","4"));
		start.add(f.tuple("2","8","5"));
		
		start.add(f.tuple("3","0","6"));
		start.add(f.tuple("3","3","4"));
		start.add(f.tuple("3","6","5"));
		
		start.add(f.tuple("4","1","7"));
		start.add(f.tuple("4","4","5"));
		start.add(f.tuple("4","7","6"));
		
		start.add(f.tuple("5","2","8"));
		start.add(f.tuple("5","5","6"));
		start.add(f.tuple("5","8","7"));
		
		start.add(f.tuple("6","0","8"));
		start.add(f.tuple("6","3","0"));
		start.add(f.tuple("6","6","7"));
		
		start.add(f.tuple("7","1","0"));
		start.add(f.tuple("7","4","1"));
		start.add(f.tuple("7","7","8"));
		
		start.add(f.tuple("8","2","1"));
		start.add(f.tuple("8","5","2"));
		start.add(f.tuple("8","8","4"));
		
		b.bound(puzzle, start, f.allOf(3));

		return b;
	}
	
	public static void main(String[] args) {
		final Sudoku sudoku = new Sudoku(3);
		final Solver solver = new Solver();
		solver.options().setSolver(SATFactory.ZChaff);
		try {
			final Formula rules = sudoku.alterRules();// sudoku.rules();
			final Bounds puzzle1 = sudoku.puzzle1();
			//System.out.println(rules);
			
			final Solution sol = solver.solve(rules, puzzle1);
			
			//System.out.println(sol);
			System.out.println(sol.stats());
			
			System.out.println("+-------+-------+-------+");
			final Iterator<Tuple> psol = sol.instance().tuples(sudoku.puzzle).iterator();
			for(int i = 1; i <= 9; i++) {
				System.out.print("| ");
				for(int j = 0; j < 3; j++) {
					for(int k = 0; k < 3; k++) {
						System.out.print(psol.next().atom(2));
						System.out.print(" ");
					}
					System.out.print("| ");
				}
				if (i%3==0)
					System.out.println("\n+-------+-------+-------+");
				else 
					System.out.println();
			}
			
		

		} catch (TimeoutException e) {
			System.out.println("timed out.");
			e.printStackTrace();
		} 
	}
	
	
	
}
