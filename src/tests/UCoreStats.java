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

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import kodkod.ast.Formula;
import kodkod.engine.Solution;
import kodkod.engine.Solver;
import kodkod.engine.Statistics;
import kodkod.engine.fol2sat.TranslationLog;
import kodkod.engine.satlab.ReductionStrategy;
import kodkod.engine.satlab.SATFactory;
import kodkod.instance.Bounds;
import kodkod.util.nodes.Nodes;
import kodkod.util.nodes.PrettyPrinter;

/**
 * Test unsat core extraction for a single problem.
 * 
 * @author Emina Torlak
 */
public final class UCoreStats {

	/**
	 * Checks that the given proof of unsatisfiablity for the given problem is miminal.
	 * This method assumes that the given proof is correct.
	 */
	static void checkMinimal(Set<Formula> core, Bounds bounds) {
		System.out.print("checking minimality ... ");
		final long start = System.currentTimeMillis();
		final Set<Formula> minCore = new LinkedHashSet<Formula>(core);
		Solver solver = new Solver(); 
		solver.options().setSolver(SATFactory.MiniSat);
		for(Iterator<Formula> itr = minCore.iterator(); itr.hasNext();) {
			Formula f = itr.next();
			Formula noF = Formula.TRUE;
			for( Formula f1 : minCore ) {
				if (f!=f1)
					noF = noF.and(f1);
			}
			if (solver.solve(noF, bounds).instance()==null) {
				itr.remove();
			} 
		}
		final long end = System.currentTimeMillis();
		if (minCore.size()==core.size()) {
			System.out.println("minimal (" + (end-start) + " ms).");
		} else {
			System.out.println("not minimal (" + (end-start) + " ms). The minimal core has these " + minCore.size() + " formulas:");
			for(Formula f : minCore) {
				System.out.println(" " + f);
			}
		}
	}
	
	/**
	 * Checks that the given core is unsatsfiable with respect to the given bounds.
	 */
	static void checkCorrect(Set<Formula> core, Bounds bounds) { 
		System.out.print("checking correctness ... ");
		final long start = System.currentTimeMillis();
		Solver solver = new Solver(); 
		solver.options().setSolver(SATFactory.MiniSat);
		final Solution sol = solver.solve(Nodes.and(core), bounds);
		final long end = System.currentTimeMillis();
		if (sol.instance()==null) {
			System.out.println("correct (" + (end-start) + " ms).");
		} else {
			System.out.println("incorrect! (" + (end-start) + " ms). The core is satisfiable:");
			System.out.println(sol);
		}
	}
	
	
	/**
	 * @return class with the given name
	 */
	static Class<?> problem(String className) { 
		try {
			return Class.forName(className);
		} catch (ClassNotFoundException e) {
			throw new IllegalArgumentException("Class "+className + " can not be found.");
		}
	}
	
	/**
	 * Returns the formulas returned by the given methods, when invoked on 
	 * the specified instance.
	 * @return formulas corresponding to given methods.
	 */
	static Map<Method,Formula> checks(Object instance, Collection<Method> checks) { 
		final Map<Method, Formula> ret = new LinkedHashMap<Method, Formula>();
		for(Method check : checks) { 
			try {
				ret.put(check, (Formula)check.invoke(instance));
			} catch (IllegalArgumentException e) {
			} catch (IllegalAccessException e) {
			} catch (InvocationTargetException e) {
			}
		}
		return ret;
	}
	
	/**
	 * @return all public member methods declared by the given class that take no arguments,
	 * return a Formula, and whose name starts with the word "check".
	 */
	static Collection<Method> methods(Class<?> c) {
		final List<Method> methods = new ArrayList<Method>();
		for(Method m : c.getMethods()) { 
			if (m.getDeclaringClass().equals(c) && m.getName().startsWith("check") && noArgs(m) && returnsFormula(m)) {
				methods.add(m);
			}
		}
		return methods;
	}
	
	/**@return a public member method with the given name and no arguments that returns a formula. */
	static Collection<Method> method(Class<?> c, String name) { 
		try {
			Method m = c.getMethod(name, new Class[0]);
			if (returnsFormula(m))
				return Collections.singletonList(m);
			else {
				throw new IllegalArgumentException("Wrong signature for method " + name + ".");
			}
		} catch (SecurityException e) {
			throw new IllegalArgumentException("Cannot access method " + name + ".");
		} catch (NoSuchMethodException e) {
			throw new IllegalArgumentException("Method " + name + " does not exist.");
		}
		
	}
	
	/**
	 * @return strategy with the given name
	 */
	@SuppressWarnings("unchecked")
	private static Class<? extends ReductionStrategy> strategy(String className) { 
		try {
			final Class<?> c = Class.forName(className);
			if (ReductionStrategy.class.isAssignableFrom(c)) 
				return (Class<ReductionStrategy>)c;
			else {
				throw new IllegalArgumentException(className + " is not a known strategy.");
			}
		} catch (ClassNotFoundException e) {
			throw new IllegalArgumentException(className + " is not a known strategy.");
		}
	}
	
	/**
	 * @return scope specification corresponding to the given string
	 */
	private static int scope(String scope) { 
		int ret;
		try {
			ret = Integer.parseInt(scope);
		} catch (NumberFormatException n) { 
			ret = -1;
		}
		if (ret <= 0) { 
			throw new IllegalArgumentException(scope + " is not a valid scope.");
		}
		return ret;
	}
	
	/** @return true if m has no arguments **/
	private static boolean noArgs(Method m) { return m.getParameterTypes().length==0; }
	
	/** @return true if m returns a formula **/
	private static boolean returnsFormula(Method m) { return Formula.class.isAssignableFrom(m.getReturnType()); }
	
	
	
	/** 
	 * Returns a fresh instance of the given problem.
	 * @requires problem has a no-argument constructor
	 * @return a fresh instance of the given problem */
	static Object instance(Class<?> problem) { 
		try {
			return problem.newInstance();
		} catch (InstantiationException e) {
			throw new IllegalArgumentException(problem.getName() + " has no accessible nullary constructor.");
		} catch (IllegalAccessException e) {
			throw new IllegalArgumentException(problem.getName() + " has no accessible nullary constructor.");
		}
	}
	
	/** 
	 * Returns the bounds for the given instance.
	 * @requires instance must have a method called bounds that takes an integer argument and returns a Bounds object
	 * @return instance.bounds(scope) */
	static Bounds bounds(Object instance, int scope) { 
		try {
			final Method bounder = instance.getClass().getMethod("bounds", new Class[]{int.class});
			return (Bounds) bounder.invoke(instance, scope);
		} catch (SecurityException e) {
			throw new IllegalArgumentException(instance.getClass().getName() + " has no accessible Bounds bounds(int) method.");
		} catch (NoSuchMethodException e) {
			throw new IllegalArgumentException(instance.getClass().getName() + " has no Bounds bounds(int) method.");
		} catch (IllegalAccessException e) {
			throw new IllegalArgumentException("Could not invoke Bounds bounds(int) method of " + instance.getClass().getName() + ".");
		} catch (InvocationTargetException e) {
			throw new IllegalArgumentException("Could not invoke Bounds bounds(int) method of " + instance.getClass().getName() + ".");
		}
	}
	
	/** 
	 * Returns the bounds for the given instance.
	 * @requires instance must have a method called bounds that takes no arguments and returns a Bounds object
	 * @return instance.bounds() */
	static Bounds bounds(Object instance) { 
		try {
			final Method bounder = instance.getClass().getMethod("bounds", new Class[]{});
			return (Bounds) bounder.invoke(instance);
		} catch (SecurityException e) {
			throw new IllegalArgumentException(instance.getClass().getName() + " has no accessible Bounds bounds(int) method.");
		} catch (NoSuchMethodException e) {
			throw new IllegalArgumentException(instance.getClass().getName() + " has no Bounds bounds(int) method.");
		} catch (IllegalAccessException e) {
			throw new IllegalArgumentException("Could not invoke Bounds bounds(int) method of " + instance.getClass().getName() + ".");
		} catch (InvocationTargetException e) {
			throw new IllegalArgumentException("Could not invoke Bounds bounds(int) method of " + instance.getClass().getName() + ".");
		}
	}
	
	private static Map<String,String> processOptionalArgs(String[] args) { 
		final Map<String,String> ret = new LinkedHashMap<String, String>();
		for(int i = 2; i < args.length;) {
			if (!ret.containsKey(args[i]))
				if (i+1<args.length && !args[i+1].startsWith("-")) {
					ret.put(args[i], args[i+1]);
					i += 2;
				} else {
					ret.put(args[i], "");
					i++;
				}
			else {
				usage();
			}
		}
		return ret;
	}
	
	/**
	 * Returns an instance of the given reduction strategy.
	 * @requires strategy has a constructor that takes a translation log
	 * @return an instance of the given reduction strategy.
	 */
	static ReductionStrategy instance(Class<? extends ReductionStrategy> strategy, TranslationLog log) { 
		try {
			return strategy.getConstructor(TranslationLog.class).newInstance(log);
		} catch (IllegalArgumentException e) {
			throw e;
		} catch (SecurityException e) {
			throw new IllegalArgumentException(strategy.getName() + " has no accessible one-argument constructor.");
		} catch (InstantiationException e) {
			throw new IllegalArgumentException(strategy.getName() + " has no accessible one-argument constructor.");
		} catch (IllegalAccessException e) {
			throw new IllegalArgumentException(strategy.getName() + " has no accessible one-argument constructor.");
		} catch (InvocationTargetException e) {
			throw new IllegalArgumentException(strategy.getName() + " has no accessible one-argument constructor.");
		} catch (NoSuchMethodException e) {
			throw new IllegalArgumentException(strategy.getName() + " has no accessible one-argument constructor.");
		}
	}
		
	/**
	 * Returns the minimal core produced by the simple extraction algorithm.
	 */
	private static Set<Formula> nce(Formula formula, Bounds bounds) { 
		final Solver solver = new Solver();
		solver.options().setSolver(SATFactory.MiniSat);
		final Set<Formula> minCore = new LinkedHashSet<Formula>(Nodes.roots(formula));
		final Set<Formula> unknown = new LinkedHashSet<Formula>(minCore);
		
		while (!unknown.isEmpty()) { 
			Formula f = unknown.iterator().next();
			Set<Formula> tryCore = new LinkedHashSet<Formula>(minCore);
			tryCore.remove(f);
			
			if (solver.solve(Nodes.and(tryCore), bounds).instance()==null) { // unsat: f is irrelevant
				minCore.remove(f);
			}
			
			unknown.remove(f);
		}
		return minCore;
	}
	

	
	/**
	 * Usage: java tests.UCoreUnitTest <class> <scope> [-m method] [-s strategy] [-o (user | stats) ]
	 */
	private static void usage() {  
		System.out.println("Usage: java test.UCoreStats <class> <scope> [-m method] [-s strategy] [-o (user | stats) ]");
		System.exit(2);
	}
	
	/**
	 * Usage: java tests.UCoreStats <class> <scope> [-m method] [-s strategy] [-o (user | stats) ]
	 */
	public static void main(String[] args) { 
		if (args.length < 2)
			usage();
		
		try {
			final Class<?> problem = problem(args[0]);
			final Map<String,String> optional = processOptionalArgs(args);
			
			final Object instance = instance(problem);
			final Bounds bounds = bounds(instance, scope(args[1]));
			final Map<Method, Formula> checks = checks(instance, 
					optional.containsKey("-m") ? method(problem,optional.get("-m")) : methods(problem));
		
			if (checks.isEmpty()) { usage(); }
			
			final String extractor;
			final Class<? extends ReductionStrategy> strategy;
			if (optional.containsKey("-s")) {
				extractor = optional.get("-s");
				if (extractor.equals("rce")) { 
					strategy = strategy("kodkod.engine.ucore.RCEStrategy");
				} else if (extractor.equals("sce")) {
					strategy = strategy("kodkod.engine.ucore.SCEStrategy");
				} else if (!extractor.equals("nce") && !extractor.equals("oce")) { 
					strategy = strategy(optional.get("-s"));
				} else {
					strategy = null;
				}
			} else {
				extractor = "nce";
				strategy = null;
			}
				
			final ResultPrinter out = "stats".equals(optional.get("-o")) ? ResultPrinter.STATS : ResultPrinter.USER;
			
			final Solver solver = new Solver();
			solver.options().setLogTranslation(1);
			solver.options().setSolver(SATFactory.MiniSatProver);
			
			for(Map.Entry<Method, Formula> check : checks.entrySet()) { 
//				System.out.println(PrettyPrinter.print(check.getValue(), 1));
				Solution sol = solver.solve(check.getValue(), bounds);
								
				if (sol.outcome()==Solution.Outcome.UNSATISFIABLE) { 
					
					final Set<Formula> initialCore = sol.proof().highLevelCore();
					final Set<Formula> minCore;
					
					final long start = System.currentTimeMillis();
					
					if (strategy==null) { // no strategy -- naive or one-step
						if (extractor.equals("nce")) {
							minCore = nce(check.getValue(), bounds);
						} else { // oce
							minCore = initialCore;
						}
					} else {
						sol.proof().minimize(instance(strategy, sol.proof().log()));
						minCore = sol.proof().highLevelCore();
					}
					
					final long end = System.currentTimeMillis();
					
					out.printUnsat(check.getKey().getName(), check.getValue(), bounds, sol.stats(), 
							initialCore, minCore, end-start);
				
				} else if (sol.outcome()==Solution.Outcome.TRIVIALLY_UNSATISFIABLE) {	
					
					out.printFalse(check.getKey().getName(), check.getValue(), bounds, sol);
				
				} else {
					out.printSat(check.getKey().getName(), check.getValue(), bounds, sol);
				}
				
			}
		} catch (IllegalArgumentException e) { 
			System.out.println(e.getMessage());
			usage();
		}
	}
	
	/** @return short string representing a given outcome */
	static String outcome(Solution.Outcome outcome) { 
		switch(outcome) { 
		case SATISFIABLE: return "S";
		case UNSATISFIABLE: return "U";
		case TRIVIALLY_SATISFIABLE: return "T";
		case TRIVIALLY_UNSATISFIABLE: return "F";
		default : throw new AssertionError("unreachable");
		}
	}
	
	private static enum ResultPrinter {
		
		USER {
			void print(String check, Formula formula, Bounds bounds, Solution.Outcome outcome, Statistics stats) { 
				System.out.println(check + ": "+ outcome);
				System.out.println("p cnf " + stats.variables() + " " + stats.clauses());
				System.out.println("translation time: " + stats.translationTime() + " ms");
				System.out.println("solving time: " + stats.solvingTime() + " ms");
			}
			void printSat(String check, Formula formula, Bounds bounds, Solution sol) {
				print(check, formula, bounds, sol.outcome(), sol.stats());
				System.out.println("instance: " + sol.instance());
			}
			void printFalse(String check, Formula formula, Bounds bounds, Solution sol) {
				print(check, formula, bounds, sol.outcome(), sol.stats());
				System.out.println("trivial core:");
				for(Formula f : sol.proof().highLevelCore()) { 
					System.out.println(PrettyPrinter.print(f, 2, 100));
				}
			}
			void printUnsat(String check, Formula formula, Bounds bounds, Statistics stats, 
					Set<Formula> initialCore, Set<Formula> minCore, long minTime) {
				print(check, formula, bounds, Solution.Outcome.UNSATISFIABLE, stats);
				System.out.println("formulas: " + Nodes.roots(formula).size());
				System.out.println("initial core: " + initialCore.size());
				System.out.println("minimized core with " + minCore.size() + " formulas found in " + minTime + " ms:");
				for(Formula f : minCore) { 
					System.out.println(PrettyPrinter.print(f, 2, 100));
				}
			}
		},
		STATS {
			void print(String check, Formula formula, Bounds bounds, Solution.Outcome outcome, Statistics stats) { 
				System.out.print(check);
				System.out.print("\t");
				System.out.print(outcome(outcome));
				System.out.print("\t");
				System.out.print(stats.variables());
				System.out.print("\t");
				System.out.print(stats.clauses());
				System.out.print("\t");
				System.out.print(stats.translationTime());
				System.out.print("\t");
				System.out.print(stats.solvingTime());
			}
			void printSat(String check, Formula formula, Bounds bounds, Solution sol) {
				print(check, formula, bounds, sol.outcome(), sol.stats());
				System.out.println();
			}
			void printFalse(String check, Formula formula, Bounds bounds, Solution sol) {
				print(check, formula, bounds, sol.outcome(), sol.stats());
				System.out.print("\t");
				System.out.println(sol.proof().highLevelCore().size());
			}
			void printUnsat(String check, Formula formula, Bounds bounds, Statistics stats, 
					Set<Formula> initialCore, Set<Formula> minCore, long minTime) {
				print(check, formula, bounds, Solution.Outcome.UNSATISFIABLE, stats);
				System.out.print("\t");
				System.out.print(Nodes.roots(formula).size());
				System.out.print("\t");
				System.out.print(initialCore.size());
				System.out.print("\t");
				System.out.print(minCore.size());
				System.out.print("\t");
				System.out.print(minTime);
				System.out.println();
			}
		};
		
		abstract void printSat(String check, Formula formula, Bounds bounds, Solution sol);
		abstract void printUnsat(String check, Formula formula, Bounds bounds, Statistics stats, 
				Set<Formula> initialCore, Set<Formula> minCore, long minTime);
		abstract void printFalse(String check, Formula formula, Bounds bounds, Solution sol);
	}
	
}
