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

final class IDPSudoku extends ExternalSolver {
	/**
	 * Constructs a new IDP solver, assuming IDP solver is in the specified directory.
	 */
	IDPSudoku(String path)  throws IOException { 
		super(path+ (path.endsWith(File.separator)?"":File.separator) + "IDP", ".idp"); 
	}
	@Override
	void printSudoku(TupleSet puzzle, PrintStream stream) {
		final int n = puzzle.universe().size(), r = (int)StrictMath.sqrt(n);
		stream.println("Given:");
		stream.println("type int Number");
		stream.println("Given(Number,Number,Number)");

		stream.println("Find:");
		stream.println("Grid(Number,Number) : Number");

		stream.println("Satisfying:");
		stream.println("! r c n : Given(r,c,n) => Grid(r,c) = n.");

	    stream.println("! r c1 c2: Number(r) & Number(c1) & Number(c2) & Grid(r,c1) = Grid(r,c2) => c1 = c2.");
	    stream.println("! c r1 r2: Number(r1) & Number(r2) & Number(c) & Grid(r1,c) = Grid(r2,c) => r1 = r2.");

		stream.println("declare { ");
		stream.println("Same(Number,Number) ");
		stream.println("SameRegion(Number,Number,Number,Number) }");

		stream.println("! r1 r2 c1 c2: (Grid(r1,c1) = Grid(r2,c2) & SameRegion(r1,r2,c1,c2)) => (r1 = r2 & c1 = c2).");

		stream.println("{ Same(n,n).");
		stream.println("  Same(n1,n2) <- Same(n2,n1).");
		for(int i = 1; i<n; i++) { 
			if (i%r!=0) 
				stream.println("  Same("+i+","+(i+1)+").");
		}
		stream.println("}");
		stream.println("{  SameRegion(r1,r2,c1,c2) <- Same(r1,r2) & Same(c1,c2). }");
		stream.println("Data:");
		stream.println("Number = {1.."+n+"}");
		stream.println("Given = {");
		int last = 1;
		for(Tuple t : puzzle) { 
			if (last == (Integer)t.atom(0))
				stream.print(t.atom(0)+","+t.atom(1)+","+t.atom(2)+"; ");
			else {
				last = (Integer)t.atom(0);
				stream.println(t.atom(0)+","+t.atom(1)+","+t.atom(2)+"; ");
			}
		}
		stream.println("}");
	}
}