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
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.lang.management.ManagementFactory;
import java.lang.management.ThreadMXBean;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import kodkod.instance.TupleSet;
import tests.util.ProcessRunner;

/**
 * A skeleton implementation of a sudoku solver that works
 * by calling an external program with a file containing
 * a sudoku encoding.
 * @author Emina Torlak
 */
abstract class ExternalSolver extends SudokuSolver {

	final String cmd;
	private final File tmp;
	private final ThreadMXBean bean;
	
	/**
	 * Constructs a new external solver with the given command
	 * and file extension.
	 */
	ExternalSolver(String cmd, String ext) throws IOException { 
		this.cmd = cmd; 
		this.tmp = File.createTempFile("sudoku", ext);
		bean = ManagementFactory.getThreadMXBean();
		bean.setThreadCpuTimeEnabled(true);
	}
	
	/**
	 * Prints the encoding of a sudoku puzzle with the given clues
	 * to the specified stream.
	 */
	abstract void printSudoku(TupleSet puzzle, PrintStream stream);

	/**
	 * Returns the amount of time taken to solve the problem.
	 * @return amount of time taken to solve the problem.
	 */
	final long solve(TupleSet puzzle, boolean printOutput) throws IOException { 
		final long start = bean.getCurrentThreadUserTime();
		final PrintStream stream = new PrintStream(new FileOutputStream(tmp));
		printSudoku(puzzle, stream);
		final long end = bean.getCurrentThreadUserTime();
		stream.flush();
		stream.close();
		
		final ProcessRunner runner = new ProcessRunner(("time " +cmd + " " +tmp.getAbsolutePath()).split("\\s"));
		runner.start();
		
		try {	
			
			runner.join();
			
			String line = null;
			BufferedReader out;
			if (printOutput) { 
				out = new BufferedReader(new InputStreamReader(runner.processOutput(), "ISO-8859-1"));
				while((line = out.readLine()) != null) { System.out.println(line); }
				out.close();
			}
			long time = -1;
			out = new BufferedReader(new InputStreamReader(runner.processError(), "ISO-8859-1"));
			final Pattern pattern = Pattern.compile("\\s*\\d+\\.\\d+\\s*real\\s+(\\d+\\.\\d+)\\s*user\\s+\\d+\\.\\d+\\s*sys");
			final Matcher matcher = pattern.matcher("");
			while((line = out.readLine()) != null) { 
				matcher.reset(line);
				if (matcher.matches()) { 
					time = (long)(Double.parseDouble(matcher.group(1))*1000);
				}
				if (printOutput) 
					System.out.println(line); 
			}
			out.close();
			
			runner.destroyProcess();
			return time+(end-start)/1000000;
			
			
		} catch (InterruptedException ie) { 
			runner.destroyProcess();
			return -1;
		}
	}
	
	/**
	 * {@inheritDoc}
	 * @see examples.sudoku.SudokuSolver#releaseResources()
	 */
	final void releaseResources() { 
		tmp.delete();
	}
}