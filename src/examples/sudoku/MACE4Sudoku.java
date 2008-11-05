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

import java.io.File;
import java.io.IOException;
import java.io.PrintStream;

import kodkod.instance.Tuple;
import kodkod.instance.TupleSet;

/**
 * A sudoku solver that works
 * by calling MACE4 with a file containing
 * a sudoku encoding.
 * @author Emina Torlak
 */
final class MACE4Sudoku extends ExternalSolver {
	/**
	 * Constructs a new MACE4 solver, assuming MACE4 solver is in the specified directory.
	 */
	MACE4Sudoku(String path)  throws IOException { 
		super(path+ (path.endsWith(File.separator)?"":File.separator) + "mace4 -f", ".m4"); 
	}
	/**
	 * {@inheritDoc}
	 * @see examples.sudoku.ExternalSolver#printSudoku(kodkod.instance.TupleSet, java.io.PrintStream)
	 */
	@Override
	void printSudoku(TupleSet puzzle, PrintStream stream) {
		final int n = puzzle.universe().size(), r = (int)StrictMath.sqrt(n);
		stream.println("assign(domain_size, "+n+").");
		stream.println("set(print_models_portable).");
		stream.println("formulas(sudoku_rules).");

		stream.println("all x all y1 all y2 (f(x, y1) = f(x, y2) -> y1 = y2).");
		stream.println("all x1 all x2 all y (f(x1, y) = f(x2, y) -> x1 = x2).");
		stream.println("all x same_interval(x,x).");
		stream.println("all x all y (same_interval(x,y) -> same_interval(y,x)).");
		stream.println("all x all y all z (same_interval(x,y) & same_interval(y,z) -> same_interval(x,z)).");
		
		for(int i = 0, max = n-1; i<max; i++) { 
			if (i%r!=(r-1)) 
				stream.println("same_interval("+i+","+(i+1)+").");
		}
		for(int i = 0; i < n; i+= r) { 
			for(int j = i+r; j < n; j+=r) { 
				stream.println("-same_interval("+i+","+j+").");
			}
		}
		stream.println("all x1 all y1 all x2 all y2");
		stream.println("  (");
		stream.println("     same_interval(x1,x2) &");
		stream.println("     same_interval(y1,y2) &");
		stream.println("     f(x1, y1) = f(x2, y2)");
		stream.println("  ->");
		stream.println("     x1 = x2 &");
		stream.println("     y1 = y2"); 
		stream.println("  ).");
		
		stream.println("all x all z exists y (f(x,y) = z).");
		stream.println("all y all z exists x (f(x,y) = z).");
		stream.println("end_of_list.");
		
		stream.println("formulas(sample_puzzle).");
		for(Tuple t : puzzle) { 
			stream.println("f("+((Integer)t.atom(0)-1)+","+((Integer)t.atom(1)-1)+") = "+((Integer)t.atom(2)-1) +".");
		}
		stream.println("end_of_list.");
	}
}