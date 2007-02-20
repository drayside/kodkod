/**
 * 
 */
package tests;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import kodkod.ast.Formula;
import kodkod.engine.Proof;
import kodkod.engine.Solution;
import kodkod.engine.Solver;
import kodkod.engine.fol2sat.TranslationLog;
import kodkod.engine.satlab.SATFactory;
import kodkod.instance.Bounds;
import examples.CeilingsAndFloors;
import examples.Dijkstra;
import examples.Pigeonhole;
import examples.tptp.GEO158;

/**
 * Tests the unsat core functionality.
 * @author Emina Torlak
 */
public final class UCoreTest {
	
	/**
	 * Returns a Solver configured for ucore extraction.
	 * @return a Solver configured for ucore extraction.
	 */
	private static Solver solver() {
		final Solver ret = new Solver();
		ret.options().setLogTranslation(true);
		ret.options().setSolver(SATFactory.MiniSatProver);
//		ret.options().setSymmetryBreaking(0);
		return ret;
	}
	
	/**
	 * Returns a Solution to the pigeonhole problem with the given parameters.
	 * @return a Solution to the pigeonhole problem with the given parameters.
	 */
	static Solution pigeonhole(List<String> params) {
		if (params.size() < 2)
			usage();
		final Pigeonhole model = new Pigeonhole();
		final Solver solver = solver();
		try {
			final int p = Integer.parseInt(params.get(0));
			final int h = Integer.parseInt(params.get(1));
			solver.options().setSymmetryBreaking(p);
			final Formula show = model.declarations().and(model.pigeonPerHole());
			return solver.solve(show, model.bounds(p,h));
		} catch (NumberFormatException nfe) {
			usage();
		}
		return null;
	}
	
	/**
	 * Returns a Solution to the ceilingsAndFloors problem with the given parameters.
	 * @return a Solution to the ceilingsAndFloors problem with the given parameters.
	 */
	static Solution ceilingsAndFloors(List<String> params) {
		if (params.size() < 2) usage();
		
		final CeilingsAndFloors model = new CeilingsAndFloors();
		final Solver solver = solver();
		
		try {
			final int m = Integer.parseInt(params.get(0));
			final int p = Integer.parseInt(params.get(1));
			final Formula show = model.belowTooDoublePrime();
			return solver.solve(show, model.bounds(m,p));
			
			
		} catch (NumberFormatException nfe) {
			usage();
		}
		
		return null;
	}
	
	/**
	 * Returns a Solution to the dijkstra problem with the given parameters.
	 * @return a Solution to the dijkstra problem with the given parameters.
	 */
	static Solution dijkstra(List<String> params) {
		if (params.size() < 3)
			usage();
		
		final Dijkstra model = new Dijkstra();
		final Solver solver = solver();

		try {
			final Formula noDeadlocks = model.dijkstraPreventsDeadlocksAssertion();
			final int states = Integer.parseInt(params.get(0));
			final int processes = Integer.parseInt(params.get(1));
			final int mutexes = Integer.parseInt(params.get(2));
			final Bounds bounds = model.bounds(states, processes, mutexes);
//			System.out.println(noDeadlocks);
//			System.out.println(bounds);
			return solver.solve(noDeadlocks, bounds);
//			System.out.println(sol1);
//			System.out.println(solver.solve(model.grabOrRelease().and(model.declarations()).
//					and(model.waits.some()).and(model.deadlock()), bounds));
			
		} catch (NumberFormatException nfe) {
			usage();
		}
		return null;
	}
	
	/**
	 * Returns a Solution to the geo158 problem with the given parameters.
	 * @return a Solution to the geo158 problem with the given parameters.
	 */
	static Solution geo158(List<String> params) {
		if (params.size() < 1)
			usage();
		
		try {
			final int n = Integer.parseInt(params.get(0));
				
			final Solver solver = solver();
	
			final GEO158 model = new GEO158();
			final Formula f = model.axioms().and(model.someCurve());
			
			final Bounds b = model.bounds(n,n);
			return solver.solve(f,b);
		} catch (NumberFormatException nfe) {
			usage();
		}
		return null;
	}
	
	private static void usage() {
		System.out.println("Usage: java tests.UCoreTest <command>");
		System.out.println("Available commands:" );
		System.out.println(" pigeonhole #pigeons #holes");
		System.out.println(" ceilingsAndFloors #man #platform");
		System.out.println(" dijkstra #states #processes #mutexes");
		System.out.println(" geo158 #atoms");
		System.exit(1);
	}
	
	/**
	 * Usage:  java tests.UCoreTest <name of test> <scope parameters>
	 */
	public static void main(String[] args) {
		if (args.length < 1) usage();
		
		try {
			final Method method = UCoreTest.class.getDeclaredMethod(args[0], List.class);
			//System.out.println(method);
			final Solution sol = (Solution) method.invoke(null, Arrays.asList(args).subList(1, args.length));
			System.out.println(sol);
			if (sol.outcome()==Solution.Outcome.UNSATISFIABLE) {
				final Proof proof = sol.proof();
				System.out.println("hardness: "+proof.relativeHardness());
				
				proof.refine();
				System.out.println("hardness after refinement: "+proof.relativeHardness());
				
				for(Iterator<TranslationLog.Record> itr = proof.core(); itr.hasNext();) {
					System.out.println(itr.next());
				}
			}
		} catch (SecurityException e) {
			e.printStackTrace();
		} catch (NoSuchMethodException e) {
			usage();
		} catch (IllegalArgumentException e) {
			e.printStackTrace();
		} catch (IllegalAccessException e) {
			e.printStackTrace();
		} catch (InvocationTargetException e) {
			e.printStackTrace();
		}
		
	}
	
}
