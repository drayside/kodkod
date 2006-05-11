package examples;

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
 * A kodkod encoding of sudoku.als:
 * <pre>
 * module internal/sudoku2
 * 
 * abstract sig Number {
 *  data: Number -> Number
 * } 
 * one sig Zero, One, Two, Three, Four, Five, Six, Seven, Eight extends Number {}
 * 
 * pred uniqueRegions() {
 *   // all numbers in each region
 *   let region0 = Zero+One+Two, region1 = Three+Four+Five, 
 *     region2=Six+Seven+Eight | {
 *     region0.(region0.data) = Number
 *     region0.(region1.data) = Number
 *     region0.(region2.data) = Number
 *     region1.(region0.data) = Number
 *     region1.(region1.data) = Number
 *     region1.(region2.data) = Number
 *     region2.(region0.data) = Number
 *     region2.(region1.data) = Number
 *     region2.(region2.data) = Number
 *   }
 * }
 * 
 * pred rules() {
 *   uniqueRegions() &&
 *   all x, y: Number | 
 *    some y.(x.data) && 
 *    no (y.(x.data) & 
 *    ((Number-y).(x.data) + y.((Number-x).data))) 
 * }
 * 
 * pred puzzle() {
 *   Zero->Zero->One + Zero->Three->Two + Zero->Six->Three + 
 *   One->One->Two + One->Four->Three + One->Seven->Four + 
 *   Two->Two->Three + Two->Five->Four + Two->Eight->Five +
 * 
 *   Three->Zero->Six + Three->Three->Four + Three->Six->Five + 
 *   Four->One->Seven + Four->Four->Five + Four->Seven->Six + 
 *   Five->Two->Eight + Five->Five->Six + Five->Eight->Seven +   
 * 
 *   Six->Zero->Eight + Six->Three->Zero + Six->Six->Seven + 
 *   Seven->One->Zero + Seven->Four->One + Seven->Seven->Eight + 
 *   Eight->Two->One + Eight->Five->Two + Eight->Eight->Four 
 *   
 *   in data
 * }
 * 
 * pred game() {
 *  rules() && puzzle()
 * }
 * </pre>
 */
public final class Sudoku {

	private final Relation puzzle;
	private final Relation[] regions;
	
	
	/**
	 * Creates an nxn sudoku grid
	 * @requires n > 0
	 */
	public Sudoku(int n) {
		assert n > 0;
		puzzle = Relation.ternary("puzzle");
		regions = new Relation[n];
		for(int i = 0; i < n; i++) {
			regions[i] = Relation.unary("region"+i);
		}
	}

	/**
	 * Returns the declaration constraints.
	 * @return declarations
	 */
	public final Formula declarations() {
		final Variable v1 = Variable.unary("v1");
		final Variable v2 = Variable.unary("v2");
		return v2.join(v1.join(puzzle)).one().forAll(v1.oneOf(UNIV).and(v2.oneOf(UNIV)));
	}
	
	/**
	 * Returns the uniqueRegions predicate.
	 * @return uniqueRegions
	 */
	public final Formula oneInEachRegion() {
		Formula ret = Formula.TRUE;
		for(Relation x: regions) {
			for(Relation y: regions) {
				ret = ret.and(UNIV.eq(y.join(x.join(puzzle))));
			}
		}
		return ret;
	}
	
	/**
	 * Returns the rules predicate.
	 * @return rules
	 */
	public final Formula rules() {
		Formula ret = oneInEachRegion();
		final Variable x = Variable.unary("x");
		final Variable y = Variable.unary("y");
		final Expression xy = y.join(x.join(puzzle));
		final Expression xdiffy = (UNIV.difference(y)).join(x.join(puzzle));
		final Expression ydiffx = y.join(UNIV.difference(x).join(puzzle));
		Formula f = xy.some().and(xy.intersection(xdiffy.union(ydiffx)).no());
		return ret.and(f.forAll(x.oneOf(UNIV).and(y.oneOf(UNIV))));
	}
	
	
	/**
	 * Returns the bounds for the following puzzle:
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
	public final Bounds puzzle1() {
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
	
	/**
	 * Usage: java examples.Suduku
	 */
	public static void main(String[] args) {
		final Sudoku sudoku = new Sudoku(3);
		final Solver solver = new Solver();
		solver.options().setSolver(SATFactory.ZChaff);
		try {
			final Formula rules = sudoku.rules();// sudoku.rules();
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
