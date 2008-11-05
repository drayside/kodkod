/* 
 * Kodkod -- Copyright (c) 2005-2008, Emina Torlak
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package examples.sudoku;

import java.io.IOException;
import java.util.Iterator;
import java.util.Map;
import java.util.Random;

import kodkod.engine.Solution;
import kodkod.engine.Solver;
import kodkod.engine.satlab.SATFactory;
import kodkod.instance.Tuple;
import kodkod.instance.TupleSet;

/**
 * A driver for solving sudoku puzzles with various solvers.
 * 
 * @author Emina Torlak
 */
public abstract class SudokuSolver {
	
	/**
	 * Returns the amount of time taken to solve the problem.
	 * @return amount of time taken to solve the problem.
	 */
	abstract long solve(TupleSet puzzle, boolean printOutput) throws IOException ;
	
	/**
	 * Returns an array of longs, storing the times that this solver took to solve the variants
	 * of the given puzzle, constructed by adding more and more clues to it (using the given random seed)
	 * until the entire puzzle is specified.  A negative entry indicates an error.
	 * @return an array of longs, storing the times that this solver took to solve the variants
	 * of the given puzzle, constructed by adding more and more clues to it (using the given random seed)
	 * until the entire puzzle is specified
	 */
	final long[] solveAll(TupleSet clues, long seed) throws IOException {
		final Random rand = new Random(seed);
		final TupleSet rest = rest(clues);
		
		// warmup the  solver 
		solve(clues, false);
		
		final long[] times = new long[rest.size()+1];
		
		times[0] = solve(clues, false);
		
		for(int i = 1; i < times.length; i++) { 
			final Iterator<Tuple> tuples = rest.iterator();
			for(int j = 0, max = rand.nextInt(rest.size()); j < max; j++) { tuples.next(); }
			clues.add(tuples.next());
			tuples.remove();
			times[i] = solve(clues, false);
		}
		
		return times;
	}
	
	/**
	 * Releases any resources associated with the solver.
	 */
	abstract void releaseResources();
	
	/**
	 * Selects a solver to use based on the given options.
	 * @return a sudoku solver specified by the given options or a KodkodSolver is none is specified
	 */
	private static SudokuSolver solver(Map<String, String> opts) throws IOException { 
		if (opts.containsKey("-paradox")) { 
			final String path = opts.get("-paradox");
			return path==null ? new ParadoxSudoku(".") : new ParadoxSudoku(path);
		}
		
		if (opts.containsKey("-alloy3")) { 
			final String path = opts.get("-alloy3");
			return path==null ? new Alloy3Sudoku(".") : new Alloy3Sudoku(path);
		}
		
		if (opts.containsKey("-mace4")) { 
			final String path = opts.get("-mace4");
			return path==null ? new MACE4Sudoku(".") : new MACE4Sudoku(path);
		}
		
		if (opts.containsKey("-idp")) { 
			final String path = opts.get("-idp");
			return path==null ? new IDPSudoku(".") : new IDPSudoku(path);
		}
		
		if (opts.containsKey("-minisat")) { 
			final String path = opts.get("-minisat");
			return path==null ? new MiniSatSudoku(".") : new MiniSatSudoku(path);
		}
		
		return new KodkodSudoku();
	}
	
	private static TupleSet rest(TupleSet clues) { 
		final Solver solver = new Solver();
		final Sudoku sudoku = new Sudoku((int)Math.sqrt(clues.universe().size()));
		solver.options().setSolver(SATFactory.MiniSat);
		final Solution sol = solver.solve(sudoku.fastRules(), sudoku.bounds(clues));
		if (sol.instance()==null) throw new IllegalArgumentException("Unsolvable puzzle: " + SudokuParser.toString(clues));
		final TupleSet rest = sol.instance().tuples(sudoku.grid()).clone();
		rest.removeAll(clues);
		return rest;
	}
	
	
	
	private static void print(int base, long[] times) { 
		System.out.println("# of clues\ttime (ms)");
		for(int i = 0; i < times.length; i++) { 
			System.out.println((base+i)+"\t"+times[i]);
		}
	}
	
	private static void usage() { 
		System.out.println("java examples.SudokuDriver [-inc[=<random seed>]] [[-minisat | -paradox | -idp | -mace4 | -alloy3][=<solver command>]]  <puzzle>" );
		System.exit(1);
	}
	
	/**
	 * Usage: java examples.sudoku.SudokuSolver [-inc[=<random seed>]] [[-minisat | -paradox | -idp | -mace4 | -alloy3][=<solver command>]] <puzzle> 
	 */
	public static void main(String[] args) {
		if (args.length<1) { usage(); }
		
		final TupleSet clues = SudokuParser.parse(args[args.length-1]);
		final Map<String,String> opts = SudokuParser.options(args);
		
		try {
			final SudokuSolver solver = solver(opts);
			if (opts.containsKey("-inc")) { 
				final int base = clues.size();
				final String val = opts.get("-inc");
				if (val==null) {
					print(base, solver.solveAll(clues, System.currentTimeMillis()));
				} else {
					try {
						print(base, solver.solveAll(clues, Long.parseLong(val)));
					} catch (NumberFormatException nfe){
						System.out.println("Invalid random seed: " + val);
						usage();
					}
				}
				
			} else {
				
				final long time = solver.solve(clues, true);
				System.out.println("total ms to generate and solve: " + time);
				
			}
			
			solver.releaseResources();
		} catch (IOException ioe) { 
			ioe.printStackTrace();
			usage();
		}
	}
}
