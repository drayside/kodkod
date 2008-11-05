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
import java.lang.management.ManagementFactory;
import java.lang.management.ThreadMXBean;

import kodkod.engine.Solution;
import kodkod.engine.Solver;
import kodkod.engine.satlab.SATFactory;
import kodkod.instance.TupleSet;

/**
 * A sudoku solver based on Kodkod.
 * @author Emina Torlak
 */
final class KodkodSudoku extends SudokuSolver {
	private final ThreadMXBean bean;
	private final Solver solver;
	
	KodkodSudoku() {
		bean = ManagementFactory.getThreadMXBean();
		bean.setThreadCpuTimeEnabled(true);
		solver = new Solver();
		solver.options().setSolver(SATFactory.MiniSat);
	}

	/**
	 * {@inheritDoc}
	 * @see examples.sudoku.SudokuSolver#solve(kodkod.instance.TupleSet, boolean)
	 */
	@Override
	long solve(TupleSet puzzle, boolean printOutput)
			throws IOException {
		
		final long start = bean.getCurrentThreadUserTime();
		final Sudoku sudoku = new Sudoku((int)Math.sqrt(puzzle.universe().size()));
		final Solution sol = solver.solve(sudoku.rules(), sudoku.bounds(puzzle));
		final long end = bean.getCurrentThreadUserTime();
		
		if (printOutput) { 
			if (sol.instance()!=null) {
				System.out.println(sol.stats());
				System.out.println(SudokuParser.prettyPrint(sol.instance().tuples(sudoku.grid())));
			} else {
				System.out.println(sol);
			}
		}
		
		return (end-start)/1000000;
	}
	
	/**
	 * {@inheritDoc}
	 * @see examples.sudoku.SudokuSolver#releaseResources()
	 */
	void releaseResources() { }

}
