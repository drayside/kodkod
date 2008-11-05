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
import java.net.MalformedURLException;
import java.net.URL;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Obtains random sudokus from www.menneske.no  
 * @author Emina Torlak
 */
public final class MenneskeSudoku {

	private static String puzzle(int size) { 
		try {
			final URL url = new URL("http","www.menneske.no","/sudoku/"+size+"/eng/");
			final BufferedReader reader = new BufferedReader(new InputStreamReader(url.openStream()));
			String line = null;
			final Pattern p = Pattern.compile("<div\\s*class\\s*=\\s*\"grid\"\\s*>\\s*<\\s*table\\s*>");
			final Matcher m = p.matcher("");
			
			while((line=reader.readLine())!=null) { 
				m.reset(line);
				if (m.matches()) { 
					break;
				}
			}
			final Pattern p1 = Pattern.compile("<.+?>");
			final Pattern p2 = Pattern.compile("\\s*(\\d+)\\s*");
			final Pattern p3 = Pattern.compile("\\s*&nbsp;\\s*");
			final Matcher m1 = p1.matcher("");
			final Matcher m2 = p2.matcher("");
			final Matcher m3 = p3.matcher("");
			final StringBuilder builder = new StringBuilder();
			while((line=reader.readLine())!=null) {
				m1.reset(line);
				if (m1.matches()) { 
					line = m1.replaceAll("");
					m2.reset(line);
					if (m2.matches()) { 
						builder.append(m2.group(1) +" ");
					} else {
						m3.reset(line);
						if (m3.matches()) { 
							builder.append("0 ");
						} 
					}
					
				}
			}
			return builder.substring(0, builder.length()-1).toString();
		} catch (MalformedURLException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return "";
	}
	
	private static void usage() {
		System.out.println("java examples.sudoku.MenneskeSudoku [-o=<file name>] [-sleep=<miliseconds>] <size> <number of puzzles>");
		System.exit(1);
	}
	
//	private static void checkSize(String file) throws IOException { 
//		final BufferedReader reader = new BufferedReader(new FileReader(file));
//		final Set<String> puzzles = new HashSet<String>();
//		for(String line = reader.readLine(); line != null; line = reader.readLine()) {
//			if (!puzzles.add(line)) { System.out.println(line); };
//		}
//		System.out.println(puzzles.size());
//	}
	
	/**
	 * Usage: java examples.sudoku.MenneskeSudoku [-o=<file name>] [-sleep=<miliseconds>] <size> <number of puzzles> 
	 */
	public static void main(String[] args) { 
		
		if (args.length<2) usage();
		try {
		
			final Map<String, String> opts = SudokuParser.options(args);
			final PrintWriter writer;
			
			if (opts.containsKey("-o")) { 
				final String val = opts.get("-o");
				if (val==null) usage();
				writer = new PrintWriter(new BufferedWriter(new FileWriter(val)));
			} else {
				writer = new PrintWriter(System.out);
			}
			
			final int sleep;
			if (opts.containsKey("-sleep")) { 
				final String val = opts.get("-o");
				if (val==null) usage();
				sleep = Integer.parseInt(val);
				if (sleep<0) usage();
			} else {
				sleep = 120000;
			}
			
			final int size = Integer.parseInt(args[args.length-2]);
			final int numPuzzles = Integer.parseInt(args[args.length-1]);
			final Set<String> puzzles = new LinkedHashSet<String>(numPuzzles*2);
			final Random rand = new Random();
			while(puzzles.size()<numPuzzles) { 
				final String puzzle = puzzle(size);
				puzzles.add(puzzle);
				Thread.sleep(rand.nextInt(sleep));
			}
			
			for(String puzzle : puzzles) { 
				writer.println(puzzle);
			}
			
			writer.close();
		} catch (NumberFormatException nfe) { 
			usage();
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} 
		catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
}
