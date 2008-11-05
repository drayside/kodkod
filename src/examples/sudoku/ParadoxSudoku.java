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
import java.util.Iterator;

import kodkod.instance.Tuple;
import kodkod.instance.TupleSet;

/**
 * A sudoku solver that works
 * by calling Paradox with a file containing
 * a sudoku encoding.
 * @author Emina Torlak
 */
final class ParadoxSudoku extends ExternalSolver {
	/**
	 * Constructs a new Paradox solver, assuming paradox solver is in the specified directory.
	 */
	ParadoxSudoku(String path)  throws IOException { 
		super(path+(path.endsWith(File.separator)?"":File.separator) + "paradox.exe", ".tptp"); 
	}
	/**
	 * {@inheritDoc}
	 * @see examples.sudoku.ExternalSolver#printSudoku(kodkod.instance.TupleSet, java.io.PrintStream)
	 */
	void printSudoku(TupleSet puzzle, PrintStream stream) {
		final int n = puzzle.universe().size(), r = (int)StrictMath.sqrt(n);
		
		stream.println("fof(at_most_one_in_each_row, axiom,"); 
		stream.println("  (! [X, Y1, Y2] : ((grid(X, Y1) = grid(X, Y2)) => Y1 = Y2))).");
		stream.println("fof(at_most_one_in_each_column, axiom,"); 
		stream.println("  (! [X1,X2,Y] : ((grid(X1,Y) = grid(X2,Y)) => X1 = X2))).");
		stream.println("fof(region_reflexive, axiom,"); 
		stream.println("  (! [X] : same_region(X,X))).");
		stream.println("fof(region_symmetric, axiom,"); 
		stream.println("  (! [X,Y] : (same_region(X,Y) => same_region(Y,X)))).");
		stream.println("fof(region_transitive, axiom,"); 
		stream.println("  (! [X,Y,Z] : ((same_region(X,Y) & same_region(Y,Z)) => same_region(X,Z)))).");
		
		for(int i = 1; i<n; i++) { 
			if (i%r!=0)
				stream.println("fof(same_"+i+"_"+(i+1)+", axiom, (same_region("+i+","+(i+1)+"))).");
		}
		
		for(int i = 1; i< n; i+= r) { 
			for(int j = i+r; j < n; j+=r) { 
				stream.println("fof(not_same_"+i+"_"+j+", axiom, (~same_region("+i+","+j+"))).");
			}
		}
		
		stream.println("fof(all_different, axiom, (");
		stream.print("  1!=2");
		for(int i = 2; i<n; i++) { 
			if (i%r!=0) 
				stream.print(" & "+i+"!="+(i+1));
		}
		stream.println(")).");

		stream.println("fof(at_most_one_in_each_region, axiom,");
		stream.println("  (! [X1, Y1, X2, Y2] : ( (same_region(X1,X2) & same_region(Y1,Y2) & grid(X1,Y1) = grid(X2,Y2)) => (X1 = X2 & Y1 = Y2) ))).");

		stream.println("fof(puzzle, axiom, (");
		final Iterator<Tuple> itr = puzzle.iterator();
		Tuple t = itr.next();
		stream.print("  grid(" + t.atom(0) + "," + t.atom(1) + ")=" + t.atom(2));
		int last = (Integer) t.atom(0);
		while(itr.hasNext()) { 
			t = itr.next();
			if ((Integer)t.atom(0)==last)
				stream.print(" & grid(" + t.atom(0) + "," + t.atom(1) + ")=" + t.atom(2));
			else {
				last = (Integer)t.atom(0);
				stream.print(" &\n  grid(" + t.atom(0) + "," + t.atom(1) + ")=" + t.atom(2));
			}
		}
		stream.println(")).");
	}
}