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
package tests;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.lang.reflect.Method;
import java.util.LinkedHashSet;
import java.util.Set;

/**
 * Calls UCoreUnitTest on all unsatisfiable problems in examples.tptp.* and
 * selected unsatisfiable problems in examples.*
 * @author Emina Torlak
 */
public final class UCoreStatsDriver {
	
	
	private static final String[] RANGE_PROBLEMS = { 
		"examples.Lists" ,"examples.RingElection", "examples.Trees", "examples.Hotel", 
		"examples.tptp.ALG212", "examples.tptp.COM008",
		"examples.tptp.GEO091", "examples.tptp.GEO092", "examples.tptp.GEO115", "examples.tptp.GEO158",
		"examples.tptp.GEO159", "examples.tptp.LAT258", "examples.tptp.MED007", "examples.tptp.MED009", 
		"examples.tptp.TOP020", "examples.tptp.SET943", "examples.tptp.SET948", "examples.tptp.SET967",
		"examples.tptp.NUM374" };
	
	private static final class MaxSpec {
		final String problem, check;
		final int scope, depth;
		
		MaxSpec(String problem, String check, int scope, int depth) { 
			this.problem = problem;
			this.check = check;
			this.scope = scope;
			this.depth = depth;
		}
		
		MaxSpec(String problem, int scope, int depth) { 
			this(problem, UCoreStats.methods(UCoreStats.problem(problem)).iterator().next().getName(), scope, depth);
		}
	}
	
	
	private static final MaxSpec[] MAX_SPECS = {
		/** NCE, SCE, RCE */
		new MaxSpec("examples.Trees", 7, 2), 		new MaxSpec("examples.Hotel", 5, Integer.MAX_VALUE), 
		new MaxSpec("examples.RingElection", 8, 1),	new MaxSpec("examples.tptp.ALG212", 7, 2), 
		new MaxSpec("examples.tptp.COM008", 9, 4),	new MaxSpec("examples.tptp.NUM374", 3, Integer.MAX_VALUE),
		new MaxSpec("examples.tptp.SET943", 5, 3),	new MaxSpec("examples.tptp.LAT258", 7, Integer.MAX_VALUE),	
		new MaxSpec("examples.tptp.GEO092", 8, 4),	new MaxSpec("examples.tptp.GEO158", 8, Integer.MAX_VALUE),	
		new MaxSpec("examples.tptp.GEO159", 8, Integer.MAX_VALUE),
		
		/** SCE, RCE */
		new MaxSpec("examples.tptp.SET948", 14, 2),	new MaxSpec("examples.tptp.SET967", 4, Integer.MAX_VALUE),	
		new MaxSpec("examples.tptp.TOP020", 10, 2),	
		new MaxSpec("examples.Lists", "checkEmpties", 60, 4),	
		new MaxSpec("examples.Lists", "checkReflexive", 14, 4),
		new MaxSpec("examples.Lists", "checkSymmetric", 8, 4),	
		
		/** RCE */
		new MaxSpec("examples.tptp.GEO091", 10, Integer.MAX_VALUE),
		new MaxSpec("examples.tptp.GEO115", 9, Integer.MAX_VALUE),
		new MaxSpec("examples.tptp.MED007", 35, 6),
		new MaxSpec("examples.tptp.MED009", 35, 6),		
	};

	
	private static long FIVE_MIN = 300000, ONE_HOUR = 3600000;
	
	private static void usage() { 
		System.out.println("Usage: java tests.UCoreTestDriver strategy [<start scope> <end scope>]");
		System.exit(1);
	}
	
	private static void headers(String strategy) {
		System.out.print("problem\t");
		System.out.print("cmd\t");
		System.out.print("scope\t");
		System.out.print("status\t");
		System.out.print("vars\t");
		System.out.print("clauses\t");
		System.out.print("transl(ms)\t");
		System.out.print("solve(ms)\t");
		System.out.print("all core\t");
		System.out.print("init core\t");
		System.out.print("min core\t");
		System.out.print(strategy.substring(strategy.lastIndexOf(".")+1) + "(ms)\t");
		System.out.println();
	}
	
	private static void skip(String problem, String check, int scope, String status) { 
		System.out.print(problem.substring(problem.lastIndexOf(".")+1)+"\t");
		System.out.print(check+"\t");
		System.out.println(scope+"\t"+status);
	}
	
	/**
	 * Runs all problems with the given strategy for the predetermined maximum scopes.
	 * @requires strategy is the name of a reduction strategy supported by {@linkplain UCoreStats#main(String[])}
	 */
	private static void runToMax(String strategy) { 
		headers(strategy);
		
		final long timeout = strategy.toLowerCase().indexOf("rce") >= 0 ? FIVE_MIN : ONE_HOUR;
		for(MaxSpec spec : MAX_SPECS) { 
			final String opt =  " -m " + spec.check + " -s " +strategy + " -d " + spec.depth + " -o stats";
			final String cmd = "java -cp bin -Xmx2G tests.UCoreStats " + spec.problem + " " + spec.scope + " " + opt;
			
//			System.out.println(cmd);
			final ProcessRunner runner = new ProcessRunner(cmd.split("\\s"));
			runner.start();
			
			try {	
				runner.join(timeout);
				if (runner.getState()!=Thread.State.TERMINATED) {
					runner.interrupt();
					runner.destroyProcess();
					skip(spec.problem,spec.check,spec.scope,"G");
					continue;
				}
			
				final BufferedReader out = new BufferedReader(new InputStreamReader(runner.processOutput(), "ISO-8859-1"));
				String line = null;
	
				while((line = out.readLine()) != null) {
					
					final String[] parsed = line.split("\\s");
					
					System.out.print(spec.problem.substring(spec.problem.lastIndexOf(".")+1)+"\t");
					System.out.print(parsed[0]+"\t");
					System.out.print(spec.scope);
												
					for(int i = 1; i < parsed.length; i++) { 
						System.out.print("\t"+parsed[i]);
					}
					System.out.println();
				}
			} catch (InterruptedException e) {
				System.out.println("INTERRUPTED");
				runner.destroyProcess();
			} catch (UnsupportedEncodingException e) {
				e.printStackTrace();
				System.exit(1);
			} catch (IOException e) {
				e.printStackTrace();
				System.exit(1);
			}
			
		}
		
	}
	
	/**
	 * Runs all problems with the given strategy for the given scope range.
	 * @requires 1 < min <= max
	 * @requires strategy is the name of a reduction strategy supported by {@linkplain UCoreStats#main(String[])}
	 */
	private static void runAllInScopes(String strategy, int min, int max) { 
		headers(strategy);
		
		final Set<Method> timedOut = new LinkedHashSet<Method>();
		final Set<Method> sat = new LinkedHashSet<Method>();
		
		for(int scope = min; scope <= max; scope++) {
			for(String problem : RANGE_PROBLEMS) { 
				for(Method m : UCoreStats.methods(UCoreStats.problem(problem))) {
					if (timedOut.contains(m)) { 
						skip(problem,m.getName(),scope,"G");
						continue;
					}
					if (sat.contains(m)) { 
						skip(problem,m.getName(),scope,"S");
						continue;
					}
					
					final String cmd = "java -cp bin -Xmx2G tests.UCoreStats " + problem + " " + String.valueOf(scope) +  " -o stats -m " + m.getName() +" -s " +strategy;
					final ProcessRunner runner = new ProcessRunner(cmd.split("\\s"));
					runner.start();
					
					try {	
						runner.join(FIVE_MIN);
						if (runner.getState()!=Thread.State.TERMINATED) {
							runner.interrupt();
							runner.destroyProcess();
							skip(problem,m.getName(),scope,"G");
							timedOut.add(m);
							continue;
						}
					
						final BufferedReader out = new BufferedReader(new InputStreamReader(runner.processOutput(), "ISO-8859-1"));
						String line = null;
			
						while((line = out.readLine()) != null) {
							
							final String[] parsed = line.split("\\s");
							
							System.out.print(problem.substring(problem.lastIndexOf(".")+1)+"\t");
							System.out.print(parsed[0]+"\t");
							System.out.print(scope);
														
							for(int i = 1; i < parsed.length; i++) { 
								System.out.print("\t"+parsed[i]);
							}
							System.out.println();
							
							if ("S".equals(parsed[1]) || "T".equals(parsed[1])) { 
								sat.add(m);
							}
						}
					} catch (InterruptedException e) {
						System.out.println("INTERRUPTED");
						runner.destroyProcess();
					} catch (UnsupportedEncodingException e) {
						e.printStackTrace();
						System.exit(1);
					} catch (IOException e) {
						e.printStackTrace();
						System.exit(1);
					}
				}
			}
		}
	}
	
	/** Usage: java tests.UCoreStatsDriver strategy [<start scope> <end scope>]*/
	public static void main(String[] args) { 
		if (args.length<1) usage();
		
		switch(args.length) { 
		case 1 	: runToMax(args[0]); break;
		
		case 3 	: 
			try { 
				int min = Integer.parseInt(args[1]);
				int max = Integer.parseInt(args[2]); 
				if (min < 1 || min > max) usage();
				runAllInScopes(args[0], min, max);
			} catch (NumberFormatException e) { 
				usage();
			}
			break;
		
		default	: usage();
		}
	}
	
}
