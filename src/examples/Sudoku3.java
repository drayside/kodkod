/**
 * 
 */
package examples;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.Relation;
import kodkod.ast.Variable;
import kodkod.engine.Solution;
import kodkod.engine.Solver;
import kodkod.engine.satlab.SATFactory;
import kodkod.instance.Bounds;
import kodkod.instance.Tuple;
import kodkod.instance.TupleFactory;
import kodkod.instance.TupleSet;
import kodkod.instance.Universe;

/**
 * A KK encoding of the following sudoku puzzle formulation:
 * 
 * <pre>
 *  module sudoku
 * 
 *  abstract sig Number {
 *  data: Number -&gt; Number
 *  }
 * 
 *  abstract sig Region1, Region2, Region3 extends Number{}
 * 
 *  one sig One, Two, Three extends Region1{} 
 *  one sig Four, Five, Six extends Region2 {}
 *  one sig Seven, Eight, Nine extends Region3 {}
 * 
 *  pred complete(rows: set Number, cols: set Number) {
 *  Number in cols.(rows.data)
 *  } 
 *  
 *  pred rules() {
 *  all x, y: Number { lone y.(x.data) }
 * 
 *  all row: Number { complete(row, Number) }
 * 
 *  all col: Number { complete(Number, col) }
 * 
 *  complete(Region1, Region1)
 *  complete(Region1, Region2) 
 *  complete(Region1, Region3)
 *  complete(Region2, Region1)
 *  complete(Region2, Region2) 
 *  complete(Region2, Region3) 
 *  complete(Region3, Region1) 
 *  complete(Region3, Region2)
 *  complete(Region3, Region3)
 *  
 *  }
 *  
 *  pred puzzle() {
 *  Nine-&gt;Nine-&gt;One + Nine-&gt;Three-&gt;Two + Nine-&gt;Six-&gt;Three + 
 *  One-&gt;One-&gt;Two + One-&gt;Four-&gt;Three + One-&gt;Seven-&gt;Four + 
 *  Two-&gt;Two-&gt;Three + Two-&gt;Five-&gt;Four + Two-&gt;Eight-&gt;Five +
 *  
 *  Three-&gt;Nine-&gt;Six + Three-&gt;Three-&gt;Four + Three-&gt;Six-&gt;Five + 
 *  Four-&gt;One-&gt;Seven + Four-&gt;Four-&gt;Five + Four-&gt;Seven-&gt;Six + 
 *  Five-&gt;Two-&gt;Eight + Five-&gt;Five-&gt;Six + Five-&gt;Eight-&gt;Seven +   
 *  
 *  Six-&gt;Nine-&gt;Eight + Six-&gt;Three-&gt;Nine + Six-&gt;Six-&gt;Seven + 
 *  Seven-&gt;One-&gt;Nine + Seven-&gt;Four-&gt;One + Seven-&gt;Seven-&gt;Eight + 
 *  Eight-&gt;Two-&gt;One + Eight-&gt;Five-&gt;Two + Eight-&gt;Eight-&gt;Four 
 *  
 *  in data
 *  }
 *  
 *  pred game() {
 *  rules() &amp;&amp; puzzle()
 *  }
 * 
 *  run game for 9
 * </pre>
 * 
 * @author Emina Torlak
 */
public class Sudoku3 {
	private final Relation Number;
	private final Relation data;
	private final Relation[] nums;
	
	/**
	 * Creates a 9x9 sudoku grid
	 */
	public Sudoku3() {
		data = Relation.ternary("data");
		nums = new Relation[9];
		for(int i = 0; i < 9; i++) { 
			nums[i] = Relation.unary("N"+(i+1));
		}
		Number = Relation.unary("Number");
	}
	
	/**
	 * Returns the value of the complete predicate, given the specified rows and columns expressions.
	 * @requires rows.arity = cols.arity = 1
	 * @return complete(rows, cols)
	 */
	public final Formula complete(Expression/*set Number*/ rows, Expression/*set Number*/ cols) {
		return Number.in(cols.join(rows.join(data)));
	}
	
	/**
	 * Returns the rules predicate.
	 * @return rules
	 */
	public final Formula rules() {
		// (all x, y: Number | one y.(x.data))
		final Variable x = Variable.unary("x"), y = Variable.unary("y");
		final Formula total = y.join(x.join(data)).lone().forAll(x.oneOf(Number).and(y.oneOf(Number)));
		// all row: Number { complete(row, Number) }
		final Variable row = Variable.unary("row");
		final Formula rowsComplete = complete(row, Number).forAll(row.oneOf(Number));
		// all col: Number { complete(Number, col) }
		final Variable col = Variable.unary("col");
		final Formula colsComplete = complete(Number, col).forAll(col.oneOf(Number));
		Formula ret = total.and(rowsComplete).and(colsComplete);
		final Expression[] regions = new Expression[3];
		regions[0] = nums[0].union(nums[1]).union(nums[2]);
		regions[1] = nums[3].union(nums[4]).union(nums[5]);
		regions[2] = nums[6].union(nums[7]).union(nums[8]);
		for(Expression rx: regions) {
			for(Expression ry: regions) {
				ret = ret.and(complete(rx,ry));
			}
		}
//		for(int i = 0; i < 3; i++) {
//			Expression rx = nums[i*3].union(nums[i*3+1]).union(nums[i*3+2]);
//			for(int j = 0; j < 3; j++) {
//				Expression ry = nums[j*3].union(nums[j*3+1]).union(nums[j*3+2]);
//				ret = ret.and(complete(rx,ry));
//			}
//		}
		return ret;
	}
	
	/**
	 * Returns the bounds for the following data:
	 * 
	 * <pre>
	 *        1 2 3   4 5 6   7 8 9
	 * 	   +------------------------+
	 * 	 1 |  1     | 2     | 3     |
	 * 	 2 |    2   |   3   |   4   |
	 * 	 3 |      3 |     4 |     5 |
	 * 	   | -------+-------+------ |
	 * 	 4 |  6     | 4     | 5     |
	 * 	 5 |    7   |   5   |   6   |
	 * 	 6 |      8 |     6 |     7 |
	 * 	   | -------+-------+------ |
	 * 	 7 |  8     | 9     | 7     |
	 * 	 8 |    9   |   1   |   8   |
	 * 	 9 |      1 |     2 |     4 |
	 * 	   +------------------------+
	 * </pre>
	 * 
	 * @return bounds for the data
	 */
	public final Bounds puzzle() {
	
		final List<String> atoms = new ArrayList<String>(9);
		for(int i = 1; i <= 9; i++) { atoms.add(String.valueOf(i)); }
		
		final Universe u = new Universe(atoms);
		final Bounds b = new Bounds(u);
		final TupleFactory f = u.factory();
		
		b.boundExactly(Number, f.allOf(1));
		
		for(int i = 0; i < 9; i++) {
			b.boundExactly(nums[i], f.setOf(String.valueOf(i+1)));
		}
		
		final TupleSet init = f.noneOf(3);
		init.add(f.tuple("1","1","1"));
		init.add(f.tuple("1","4","2"));
		init.add(f.tuple("1","7","3"));
		
		init.add(f.tuple("2","2","2"));
		init.add(f.tuple("2","5","3"));
		init.add(f.tuple("2","8","4"));
		
		init.add(f.tuple("3","3","3"));
		init.add(f.tuple("3","6","4"));
		init.add(f.tuple("3","9","5"));
		
		init.add(f.tuple("4","1","6"));
		init.add(f.tuple("4","4","4"));
		init.add(f.tuple("4","7","5"));
		
		init.add(f.tuple("5","2","7"));
		init.add(f.tuple("5","5","5"));
		init.add(f.tuple("5","8","6"));
		
		init.add(f.tuple("6","3","8"));
		init.add(f.tuple("6","6","6"));
		init.add(f.tuple("6","9","7"));
		
		init.add(f.tuple("7","1","8"));
		init.add(f.tuple("7","4","9"));
		init.add(f.tuple("7","7","7"));
		
		init.add(f.tuple("8","2","9"));
		init.add(f.tuple("8","5","1"));
		init.add(f.tuple("8","8","8"));
		
		init.add(f.tuple("9","3","1"));
		init.add(f.tuple("9","6","2"));
		init.add(f.tuple("9","9","4"));
		
		b.bound(data, init, f.allOf(3));
		return b;
	}
	
	/**
	 * Usage: java examples.Suduku
	 */
	public static void main(String[] args) {
		final Sudoku3 sudoku = new Sudoku3();
		final Solver solver = new Solver();
		solver.options().setSolver(SATFactory.MiniSat);
	
		final Formula rules = sudoku.rules();
		final Bounds puzzle = sudoku.puzzle();
		System.out.println(rules);
//		System.out.println(puzzle);
		
		final Solution sol = solver.solve(rules, puzzle);
		
		//System.out.println(sol);
		System.out.println(sol.stats());
		
		System.out.println("+-------+-------+-------+");
		final Iterator<Tuple> psol = sol.instance().tuples(sudoku.data).iterator();
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
			
		

		
	}
}
