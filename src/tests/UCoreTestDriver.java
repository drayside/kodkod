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
public final class UCoreTestDriver {
	
	
	private static final String[] UNSAT = { 
		"examples.Lists" ,"examples.RingElection", "examples.Trees", "examples.Hotel", 
		"examples.tptp.ALG212", "examples.tptp.COM008",
		"examples.tptp.GEO091", "examples.tptp.GEO092", "examples.tptp.GEO115", "examples.tptp.GEO158",
		"examples.tptp.GEO159", "examples.tptp.LAT258", "examples.tptp.MED007", "examples.tptp.MED009", 
		"examples.tptp.TOP020", "examples.tptp.SET943", "examples.tptp.SET948", "examples.tptp.SET967",
		"examples.tptp.NUM374", };
	
	
	private static long TIMEOUT = 300000;
	
	private static void usage() { 
		System.out.println("Usage: java tests.UCoreTestDriver <start scope> <end scope> [strategy]");
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
		System.out.print( (strategy==null ? "naive" : strategy.substring(strategy.lastIndexOf(".")+1)) + "(ms)\t");
		System.out.println();
	}
	
	private static void skip(String problem, Method m, int scope, String status) { 
		System.out.print(problem.substring(problem.lastIndexOf(".")+1)+"\t");
		System.out.print(m.getName()+"\t");
		System.out.println(scope+"\t"+status);
	}
	
	/** Usage: java tests.UCoreTestDriver <start scope> <end scope> [strategy]*/
	public static void main(String[] args) { 
		if (args.length<2) usage();
		
		int min = 0, max = 0;
		try { 
			min = Integer.parseInt(args[0]);
			max = Integer.parseInt(args[1]); 
		} catch (NumberFormatException e) { }
		
		if (min < 1 || min > max) usage();
		
		
		final String strategy = args.length<3 ? null : args[2];
		headers(strategy);
		
		
		final Set<Method> timedOut = new LinkedHashSet<Method>();
		final Set<Method> sat = new LinkedHashSet<Method>();
		
//		timedOut.addAll(UCoreUnitTest.methods(UCoreUnitTest.problem("examples.tptp.NUM374")));
//		timedOut.addAll(UCoreUnitTest.methods(UCoreUnitTest.problem("examples.tptp.SET943")));
//		timedOut.addAll(UCoreUnitTest.methods(UCoreUnitTest.problem("examples.tptp.SET967")));
//		timedOut.addAll(UCoreUnitTest.methods(UCoreUnitTest.problem("examples.tptp.LAT258")));
//		timedOut.addAll(UCoreUnitTest.methods(UCoreUnitTest.problem("examples.tptp.ALG212")));
//		timedOut.addAll(UCoreUnitTest.methods(UCoreUnitTest.problem("examples.tptp.GEO159")));
//		timedOut.addAll(UCoreUnitTest.methods(UCoreUnitTest.problem("examples.RingElection")));
//		timedOut.addAll(UCoreUnitTest.method(UCoreUnitTest.problem("examples.Lists"),"checkSymmetric"));
		
//		sat.addAll(UCoreUnitTest.methods(UCoreUnitTest.problem("examples.Hotel")));
//		sat.addAll(UCoreUnitTest.methods(UCoreUnitTest.problem("examples.Trees")));
		
		for(int scope = min; scope <= max; scope++) {
			for(String problem : UNSAT) { 
				for(Method m : UCoreUnitTest.methods(UCoreUnitTest.problem(problem))) {
					if (timedOut.contains(m)) { 
						skip(problem,m,scope,"G");
						continue;
					}
					if (sat.contains(m)) { 
						skip(problem,m,scope,"S");
						continue;
					}
					
					final String cmd = "java -cp bin -Xmx2G tests.UCoreUnitTest " + problem + " " + String.valueOf(scope) +  " -o stats -m " + m.getName();
					final ProcessRunner runner = new ProcessRunner(strategy==null?cmd.split("\\s"):(cmd+" -s " +strategy).split("\\s"));
					runner.start();
					
					try {	
						runner.join(TIMEOUT);
						if (runner.getState()!=Thread.State.TERMINATED) {
							runner.interrupt();
							runner.destroyProcess();
							skip(problem,m,scope,"G");
							timedOut.add(m);
							continue;
						}
					
						final BufferedReader out = new BufferedReader(new InputStreamReader(runner.process.getInputStream(), "ISO-8859-1"));
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
	
	private static final class ProcessRunner extends Thread {
		final String[] cmd;
		private Process process;
		
		ProcessRunner(String[] cmd) { 
			this.cmd = cmd; 
			process =  null;
		}
		
		void destroyProcess() { if (process!=null) { process.destroy(); process=null; } ; }
		
		public void run() { 
			try {
				process = Runtime.getRuntime().exec(cmd);
				process.waitFor();
			} catch (IOException e) {
				System.out.print("Could not run: ");
				for(String c: cmd) { 
					System.out.print(c + " ");
				}
				System.out.println();
				System.exit(1);
			} catch (InterruptedException e) {
				// ignore
			}
		}
	}
	
}
