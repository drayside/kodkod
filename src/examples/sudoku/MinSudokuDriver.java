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

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Map;

/**
 * Driver for min sudoku experiments.
 * @author Emina Torlak
 */
public final class MinSudokuDriver {
	private static int GIVENS = 17, N = 81;
		
	private final long seed;
	private final SudokuDatabase db;
	/**
	 * Constructs a new MinSudokuDriver.
	 * @requires trials > 0
	 * @requires the given file contains at least <tt>trials</tt> Sudoku puzzles with 17 clues
	 * @throws IOException 
	 */
	public MinSudokuDriver(String file, int trials, long seed) throws IOException { 
		if (trials<1) throw new IllegalArgumentException("Cannot have fewer than 1 trial: " + trials);
		this.seed = seed;
		this.db = SudokuDatabase.loadRandom(file, trials, seed);
	}
	
	
	/**
	 * Runs the experiments and outputs the results to the given directory.
	 */	
	public void run(String dir) throws  IOException { 
		db.write(dir+"puzzles_"+db.size()+"_"+seed+".txt");
		final String path = "/Users/emina/Desktop/tools/";
		final SudokuSolver[] solvers = { 
			new MiniSatSudoku(path+"MiniSat_v1.14/"),	
			new KodkodSudoku(),
			new ParadoxSudoku(path+"Folkung/Haskell/"),
			new IDPSudoku(path+"PowerPC-IDP-1.3/"),
			new Alloy3Sudoku(path+"Alloy.app/Contents/Resources/Java/")
		};
		for(SudokuSolver solver : solvers) { 
			run(solver, dir);
		}
	}
	
	/**
	 * Runs the experiments with the given solver and outputs the results to the given directory.
	 */	
	private void run(SudokuSolver solver, String dir) throws IOException { 
		final int trials = db.size();
		final long[][] results = new long[trials][N-GIVENS];
		for(int i = 0; i < trials; i++) { 
			results[i] = solver.solveAll(db.puzzle(i), seed);
		}
			
		final String solverName = solver.getClass().getSimpleName().toLowerCase();
		final String prefix = dir + solverName.substring(0,solverName.lastIndexOf("sudoku")); 

		final PrintWriter avgWriter = new PrintWriter(new BufferedWriter(new FileWriter(prefix+"_"+trials+"_"+seed+"_avg.txt")));
		final PrintWriter datWriter = new PrintWriter(new BufferedWriter(new FileWriter(prefix+"_"+trials+"_"+seed+".txt")));
		
		for(int i = 0, max = N-GIVENS; i<=max; i++) { 
			double sum = 0; 
			datWriter.print(i+17);
			avgWriter.print(i+17);
			for(int j = 0; j < trials; j++) { 
				sum+=results[j][i]; 
				datWriter.print("\t"+results[j][i]);
			}
			datWriter.println();
			avgWriter.println("\t"+(sum / trials));
		}
		
		avgWriter.close();
		datWriter.close();
	}

	
	private static void usage() { 
		System.out.println("Usage: java examples.sudoku.MinSudokuDriver [-o=directory] [-seed=<random seed>] <# of trials> <min sudoku puzzle file>");
		System.exit(1);
	}
	
	/**
	 * Usage: java examples.sudoku.MinSudokuDriver [-o=directory] [-seed=<random seed>] <# of trials> <min sudoku puzzle file>
	 */
	public static void main(String[] args) {  
		if (args.length<2) usage();
		
		try {
			final String db = args[args.length-1];
			final int trials = Integer.parseInt(args[args.length-2]);
			
			final Map<String,String> opts = SudokuParser.options(args);
			
			final String dir;
			if (opts.containsKey("-o")) { 
				final String val = opts.get("-o");
				if (val==null) usage();
				dir = val.endsWith("/") ? val : val+"/";
			} else {
				dir = "";
			}
			
			final long seed;
			if (opts.containsKey("-seed")) { 
				final String val = opts.get("-seed");
				if (val==null) usage();
				seed = Long.parseLong(val);
			} else {
				seed = System.currentTimeMillis();
			}
				
			(new MinSudokuDriver(db, trials, seed)).run(dir);
				
		
		} catch (NumberFormatException nfe) { 
			usage();
		} catch (IOException e) {
			e.printStackTrace();
			usage();
		} 

	}
	
	
}
