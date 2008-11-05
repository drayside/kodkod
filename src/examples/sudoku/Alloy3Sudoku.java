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
import java.io.PrintStream;
import java.util.Iterator;

import kodkod.instance.Tuple;
import kodkod.instance.TupleSet;

/**
 * A sudoku solver that works
 * by calling Alloy3 with a file containing
 * a sudoku encoding.
 * @author Emina Torlak
 */
final class Alloy3Sudoku extends ExternalSolver {
	
	/**
	 * Constructs a new Alloy3 solver, assuming class files are in the specified directory.
	 */
	Alloy3Sudoku(String path) throws IOException { 
		super("java -cp "+path+" -Xmx2G alloy.cli.AlloyCLI -b MINISAT", ".als"); 
	}
		
	/**
	 * {@inheritDoc}
	 * @see examples.sudoku.ExternalSolver#printSudoku(kodkod.instance.TupleSet, java.io.PrintStream)
	 */
	void printSudoku(TupleSet puzzle, PrintStream stream) {
		final int n = puzzle.universe().size(), r = (int)StrictMath.sqrt(n);
		stream.println("module sudoku");

		stream.println("abstract sig Number { grid: Number -> Number }");
		for(int i = 1; i <= r; i++) { 
			stream.println("abstract sig R"+i+" extends Number {}");
		}
		for(int i = 0; i < r; i++) { 
			stream.print("one sig N"+(i*r+1));
			for(int j = 2; j <= r; j++) { 
				stream.print(", N"+(i*r+j));
			}
			stream.println(" extends R" + (i+1) + " {}");
		}
		
		stream.println("fact rules {");
		stream.println("	all x, y: Number | some grid[x][y]");
		stream.println("	all x, y: Number | no grid[x][y] & grid[x][Number-y]");
		stream.println("	all x, y: Number | no grid[x][y] & grid[Number-x][y]");
		
		for(int i = 1; i <= r; i++) { 
			for(int j = 1; j <= r; j++) { 
				stream.println("	all x: R"+i+", y: R"+j+" | no grid[x][y] & grid[R"+i+"-x][R"+j+"-y]");
			}
		}
		stream.println("}");
		
		stream.println("pred puzzle() {");
		final Iterator<Tuple> itr = puzzle.iterator();
		Tuple t = itr.next();
		stream.print("	N" + t.atom(0) + "->N" + t.atom(1) + "->N" + t.atom(2));
		int last = (Integer) t.atom(0);
		while(itr.hasNext()) { 
			t = itr.next();
			if ((Integer)t.atom(0)==last)
				stream.print(" + N" + t.atom(0) + "->N" + t.atom(1) + "->N" + t.atom(2));
			else {
				last = (Integer)t.atom(0);
				stream.print(" +\n	N" + t.atom(0) + "->N" + t.atom(1) + "->N" + t.atom(2));
			}
		}
		stream.println(" in grid }");
		
		stream.println("run puzzle");
	}
}