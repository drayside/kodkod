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

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import kodkod.ast.Formula;
import kodkod.engine.Solution;
import kodkod.engine.Solver;
import kodkod.engine.satlab.SATFactory;
import kodkod.instance.Bounds;
import kodkod.instance.Tuple;
import kodkod.instance.TupleFactory;
import kodkod.instance.TupleSet;
import tests.util.ProcessRunner;

/**
 * A driver for unsat core experiments based on sudoku.
 * @author Emina Torlak
 */
public final class CoreDriver {
	private final long timeout;
	private final Solver solver;
	private final Sudoku sudoku;
	private final SudokuDatabase db;
	private final long seed;
	private final Random rand;
	private final int n;
	private final int maxBugsPerTrial;
	/**
	 * Constructs a driver for the given database, using the given number of trials,  maximum
	 * number of bugs per trial,  random seed, and timeout.  
	 * @requires trials > 0
	 * @requires the given file contains at least <tt>trials</tt> puzzles
	 * @throws IOException 
	 */
	public CoreDriver(String file, int trials, int maxBugsPerTrial, long seed, long timeout) throws IOException { 
		if (trials<1) throw new IllegalArgumentException("Cannot have fewer than 1 trial: " + trials);
		if (maxBugsPerTrial<1) throw new IllegalArgumentException("Cannot have fewer than 1 bug per trial: " + maxBugsPerTrial);
		if (timeout<1) throw new IllegalArgumentException("Timeout must be positive: " + timeout);
		this.timeout = timeout;
		this.maxBugsPerTrial = maxBugsPerTrial;
		this.db = SudokuDatabase.loadRandom(file, trials, seed);
		this.seed = seed;
		this.rand = new Random(seed);
		this.solver = new Solver();
		solver.options().setSolver(SATFactory.MiniSatProver);
		solver.options().setLogTranslation(1);
		this.n = db.puzzle(0).universe().size();
		this.sudoku = new Sudoku((int)Math.sqrt(n));
	}
	
	/**
	 * Runs the experiments and outputs the results to the given directory.
	 */
	public void run(String dir) throws IOException { 
		final PrintWriter dataWriter = new PrintWriter(new BufferedWriter(new FileWriter(dir+"cores_"+db.size()+"_"+seed+".txt")),true);
		final PrintWriter puzzleWriter = new PrintWriter(new BufferedWriter(new FileWriter(dir+"puzzles_"+db.size()+"_"+seed+".txt")),true);
		
		dataWriter.println("oce core\trce core\tsce core\tnce core\toce ms\trce ms\tsce ms\tnce ms");

		final Formula rules = sudoku.rules();
		
		for(TupleSet clues : db) { 
			int bugsPerTrial = 0;
			final Iterator<Tuple> bugs = bugs(clues, solver.solve(rules, sudoku.bounds(clues)).instance().tuples(sudoku.grid()));
			while(bugs.hasNext() && bugsPerTrial < maxBugsPerTrial) { 
				final Tuple bug = bugs.next();
				clues.add(bug);
				final Bounds bounds = sudoku.bounds(clues);
				final Solution sol = solver.solve(rules, bounds);
				if (sol.outcome()==Solution.Outcome.UNSATISFIABLE) {

					final String puzzle = SudokuParser.toString(clues);
					
					final long[] oce = extract(puzzle,"oce");
					final long[] rce = extract(puzzle,"rce");
					if (rce[0] > 0) {
						final long[] sce = extract(puzzle,"sce");
						if (sce[0] > 0) {
							final long[] nce = extract(puzzle,"nce");	
							if (nce[0] > 0) {
								bugsPerTrial++;
								puzzleWriter.println(puzzle);
								dataWriter.println(oce[0]+"\t"+rce[0]+"\t"+sce[0]+"\t"+nce[0]+"\t"+oce[1]+"\t"+rce[1]+"\t"+sce[1]+"\t"+nce[1]);
							}
						}
					}
				} 
				clues.remove(bug);
			}
		}
		
		puzzleWriter.close();
		dataWriter.close();
	}
	
	/**
	 * Assuming that the solutions set is a valid solution for the given set of clues,
	 * returns an iterator over all buggy clues.  The clues are returned in random order
	 * by the iterator.
	 * @return an iterator over all buggy clues.  The clues are returned in random order
	 * by the iterator.
	 */
	private final Iterator<Tuple> bugs(TupleSet clues, TupleSet solution) { 
		final TupleSet rest = solution.clone();
		rest.removeAll(clues);
		final TupleFactory factory  = clues.universe().factory();
		final TupleSet bugs = factory.noneOf(3);
		for(Tuple tuple : rest) { 
			final int val = (Integer) tuple.atom(2);
			for(int i = 1; i <= n; i++) { 
				if (i!=val)
					bugs.add(factory.tuple(tuple.atom(0), tuple.atom(1), i));
			}
		}
		final List<Tuple> bugList = new ArrayList<Tuple>(bugs.size());
		bugList.addAll(bugs);
		Collections.shuffle(bugList, rand);
		return bugList.iterator();
	}
	
	/**
	 * Calls examples.sudoku.Sudoku with the given arguments and parses
	 * and returns the result.
	 * @requires puzzle is unsatisfiable
	 * @requires extractor is a valid strategy name
	 * @return an array of 2 longs, the first of which is the size of the extracted core
	 * and the second of which is the time to extraction.  Both numbers are negative if 
	 * the process timed out. 
	 */
	private  long[] extract(String puzzle, String extractor) { 
		final ProcessRunner runner = new ProcessRunner("java","-cp","bin","-Xmx2G","examples.sudoku.Sudoku","-core="+extractor,puzzle);
		runner.start();
		try {
			runner.join(timeout);
			if (runner.getState()!=Thread.State.TERMINATED) {
				runner.interrupt();
				runner.destroyProcess();
				return new long[]{-1,-timeout};
			}
			final BufferedReader out = new BufferedReader(new InputStreamReader(runner.processOutput(), "ISO-8859-1"));
			String line = null;
			final Pattern p = Pattern.compile("Core \\(strategy="+extractor+", size=(\\d+), ms=(\\d+)\\).*");
			final Matcher m = p.matcher("");
			while((line = out.readLine()) != null) {
				m.reset(line);
				if (m.matches()) { 
					out.close();
					return new long[]{ Long.parseLong(m.group(1)), Long.parseLong(m.group(2)) };
				}
			}
			throw new IllegalArgumentException("Satisfiable puzzle: " + puzzle);
		} catch (InterruptedException e) {
			return new long[]{-1,-timeout};
		} catch (UnsupportedEncodingException e) {
			e.printStackTrace();
			throw new RuntimeException(e);
		} catch (IOException e) {
			e.printStackTrace();
			throw new RuntimeException(e);
		}
	}
	
			
	private static void usage() { 
		System.out.println("Usage: java examples.sudoku.CoreDriver [-o=directory] [-seed=<random seed>] [-mbpt=<maximum number of bugs per trial>] [-timeout=<miliseconds>] <# of trials> <sudoku puzzle file>");
		System.exit(1);
	}
	
	/**
	 * Usage: java examples.sudoku.CoreDriver [-o=directory] [-seed=<random seed>] [-mbpt=<maximum number of bugs per trial>] [-timeout=<miliseconds>] <# of trials> <sudoku puzzle file>
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
			
			final int mbpt;
			if (opts.containsKey("-mbpt")) { 
				final String val = opts.get("-mbpt");
				if (val==null) usage();
				mbpt = Integer.parseInt(val);
			} else {
				mbpt = 1;
			}
			
			final long timeout;
			if (opts.containsKey("-timeout")) { 
				final String val = opts.get("-timeout");
				if (val==null) usage();
				timeout = Long.parseLong(val);
			} else {
				timeout = 300000;
			}
			
			if (mbpt<1 || trials<1) usage();
			
			(new CoreDriver(db, trials, mbpt, seed, timeout)).run(dir);
		
		} catch (NumberFormatException nfe) { 
			usage();
		} catch (IOException e) {
			e.printStackTrace();
			usage();
		}
	}
}

