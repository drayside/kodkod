/**
 * 
 */
package tests;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import kodkod.ast.Formula;
import kodkod.engine.Proof;
import kodkod.engine.Solution;
import kodkod.engine.Solver;
import kodkod.engine.fol2sat.HigherOrderDeclException;
import kodkod.engine.fol2sat.UnboundLeafException;
import kodkod.engine.satlab.SATFactory;
import kodkod.engine.ucore.MinTopStrategy;
import kodkod.engine.ucore.StrategyUtils;
import kodkod.instance.Bounds;
import kodkod.util.nodes.Nodes;
import kodkod.util.nodes.PrettyPrinter;
import examples.AbstractWorldDefinitions;
import examples.CeilingsAndFloors;
import examples.Dijkstra;
import examples.Handshake;
import examples.Pigeonhole;
import examples.tptp.ALG195;
import examples.tptp.ALG212;
import examples.tptp.COM008;
import examples.tptp.GEO091;
import examples.tptp.GEO158;
import examples.tptp.LAT258;
import examples.tptp.MED007;
import examples.tptp.MED009;
import examples.tptp.NUM374;
import examples.tptp.SET943;
import examples.tptp.SET948;
import examples.tptp.TOP020;

/**
 * Tests the unsat core functionality.
 * @author Emina Torlak
 */
public final class UCoreTest {
	
	
	/**
	 * Returns a pigeonhole problem with the given parameters.
	 * @return a pigeonhole problem with the given parameters.
	 */
	static Problem pigeonhole(List<String> params) {
		if (params.size() < 2)
			usage();
		final Pigeonhole model = new Pigeonhole();
		try {
			final int p = Integer.parseInt(params.get(0));
			final int h = Integer.parseInt(params.get(1));
			final Formula show = model.declarations().and(model.pigeonPerHole());
			final Problem problem = new Problem(show, model.bounds(p,h));
			problem.solver.options().setSymmetryBreaking(p);
			return problem;
		} catch (NumberFormatException nfe) {
			usage();
		}
		return null;
	}
	
	/**
	 * Returns a ceilingsAndFloors problem with the given parameters.
	 * @return a ceilingsAndFloors problem with the given parameters.
	 */
	static Problem ceilingsAndFloors(List<String> params) {
		if (params.size() < 2) usage();
		
		final CeilingsAndFloors model = new CeilingsAndFloors();
				
		try {
			final int m = Integer.parseInt(params.get(0));
			final int p = Integer.parseInt(params.get(1));
			final Formula show = model.belowTooDoublePrime();
			return new Problem(show, model.bounds(m,p));
			
			
		} catch (NumberFormatException nfe) {
			usage();
		}
		
		return null;
	}
	
	/**
	 * Returns a dijkstra problem with the given parameters.
	 * @return a dijkstra problem with the given parameters.
	 */
	static Problem dijkstra(List<String> params) {
		if (params.size() < 3)
			usage();
		
		final Dijkstra model = new Dijkstra();
		
		try {
			final Formula noDeadlocks = model.dijkstraPreventsDeadlocksAssertion();
			final int states = Integer.parseInt(params.get(0));
			final int processes = Integer.parseInt(params.get(1));
			final int mutexes = Integer.parseInt(params.get(2));
			final Bounds bounds = model.bounds(states, processes, mutexes);
			return new Problem(noDeadlocks, bounds);
			
		} catch (NumberFormatException nfe) {
			usage();
		}
		return null;
	}
	
	/**
	 * Returns a geo158 problem with the given parameters.
	 * @return a geo158 problem with the given parameters.
	 */
	static Problem geo158(List<String> params) {
		if (params.size() < 1)
			usage();
		
		try {
			final int n = Integer.parseInt(params.get(0));
	
			final GEO158 model = new GEO158();
			final Formula f = model.axioms().and(model.someCurve());
			final Bounds b = model.bounds(n);
			return new Problem(f,b);
		} catch (NumberFormatException nfe) {
			usage();
		}
		return null;
	}
	
	/**
	 * Returns a top020 problem with the given parameters.
	 * @return a top020 problem with the given parameters.
	 */
	static Problem top020(List<String> params) { 
		if (params.size() < 1)
			usage();
		
		try {
			final int n = Integer.parseInt(params.get(0));
			if (n < 1)
				usage();
			final TOP020 model = new TOP020();
			final Solver solver = new Solver();
			solver.options().setSolver(SATFactory.MiniSat);
			final Formula f = model.axioms().and(model.challenge_AMR_1_4_4().not());
			final Bounds b = model.bounds(n);
//			System.out.println(f);
//			System.out.println(b);
			return new Problem(f,b);
		} catch (NumberFormatException nfe) {
			usage();
		}
		return null;
	}
	
	/**
	 * Returns a geo091 problem with the given parameters.
	 * @return a geo091 problem with the given parameters.
	 */
	static Problem geo091(List<String> params) { 
		if (params.size() < 1)
			usage();
		
		try {
			final int n = Integer.parseInt(params.get(0));
	
			final Solver solver = new Solver();
			solver.options().setSolver(SATFactory.MiniSat);
			final GEO091 model = new GEO091();
			final Formula f = model.axioms().and(model.theorem_2_13().not());
			
			final Bounds b = model.bounds(n);
			return new Problem(f,b);
		} catch (NumberFormatException nfe) {
			usage();
		}
		return null;
	}
	
	/**
	 * Returns a med007 problem with the given parameters.
	 * @return a med007 problem with the given parameters.
	 */
	static Problem med007(List<String> params) { 
		if (params.size() < 1)
			usage();
		
		try {
			final int n = Integer.parseInt(params.get(0));
			if (n < 1)
				usage();
			final MED007 model = new MED007();
			final Solver solver = new Solver();
			solver.options().setSolver(SATFactory.MiniSat);
//			solver.options().setSymmetryBreaking(1000);
//			solver.options().setFlatten(false);
			final Formula f = model.axioms().and(model.transsls2_qilt27().not());
			final Bounds b = model.bounds(n);
			return new Problem(f,b);
		} catch (NumberFormatException nfe) {
			usage();
		}
		return null;
	}
	
	/**
	 * Returns a med009 problem with the given parameters.
	 * @return a med009 problem with the given parameters.
	 */
	static Problem med009(List<String> params) { 
		if (params.size() < 1)
			usage();
		
		try {
			final int n = Integer.parseInt(params.get(0));
			if (n < 1)
				usage();
			final MED009 model = new MED009();
			final Solver solver = new Solver();
			solver.options().setSolver(SATFactory.MiniSat);
//			solver.options().setSymmetryBreaking(1000);
//			solver.options().setFlatten(false);
			final Formula f = model.axioms().and(model.transsls2_qige27().not());
			final Bounds b = model.bounds(n);
			return new Problem(f,b);
		} catch (NumberFormatException nfe) {
			usage();
		}
		return null;
	}
	/**
	 * Returns an alg212 problem with the given parameters.
	 * @return an alg212 problem with the given parameters.
	 */
	static Problem alg212(List<String> params) { 
		if (params.size() < 1)
			usage();
		
		try {
			final int n = Integer.parseInt(params.get(0));
			if (n < 1)
				usage();
			final ALG212 model = new ALG212();
			final Solver solver = new Solver();
			solver.options().setSolver(SATFactory.MiniSat);
//			solver.options().setSymmetryBreaking(n*n);
//			solver.options().setFlatten(false);
			final Formula f = model.axioms().and(model.distLong().not());
			final Bounds b = model.bounds(n);
			return new Problem(f,b);
		} catch (NumberFormatException nfe) {
			usage();
		}
		return null;
	}
	
	/**
	 * Returns an alg195 problem with the given parameters.
	 * @return an alg195 problem with the given parameters.
	 */
	static Problem alg195(List<String> params) { 
		try {
			
			final ALG195 model = new ALG195();
			final Solver solver = new Solver();
			solver.options().setSolver(SATFactory.MiniSat);
			final Formula f = model.axioms().and(model.co1().not());
			final Bounds b = model.bounds();
			return new Problem(f, b);
		} catch (NumberFormatException nfe) {
			usage();
		}
		return null;
	}
	
	/**
	 * Returns an num374 problem with the given parameters.
	 * @return an num374 problem with the given parameters.
	 */
	static Problem num374(List<String> params) { 
		if (params.size() < 1)
			usage();
		
		try {
			final int n = Integer.parseInt(params.get(0));
			if (n < 1)
				usage();
			final NUM374 model = new NUM374();
			final Formula f = model.axioms().and(model.wilkie().not());
			final Bounds b = model.bounds(n);
			final Problem p = new Problem(f,b);
//			p.solver.options().setSymmetryBreaking(n*n*n*n);
			return p;
		} catch (NumberFormatException nfe) {
			usage();
		}
		return null;
	}
	
	/**
	 * Returns an set943 problem with the given parameters.
	 * @return an set943 problem with the given parameters.
	 */
	static Problem set943(List<String> params) { 
		if (params.size() < 1)
			usage();
		
		try {
			final int n = Integer.parseInt(params.get(0));
			if (n < 1)
				usage();
			final SET943 model = new SET943();
			final Solver solver = new Solver();
			solver.options().setSolver(SATFactory.MiniSat);
//			solver.options().setSymmetryBreaking(1000);
//			solver.options().setFlatten(false);
			final Formula f = model.axioms().and(model.t96_zfmisc_1().not());
			final Bounds b = model.bounds(n);
			return new Problem(f,b);
		} catch (NumberFormatException nfe) {
			usage();
		}
		return null;
	}
	
	/**
	 * Returns an set948 problem with the given parameters.
	 * @return an set948 problem with the given parameters.
	 */
	static Problem set948(List<String> params) { 
		if (params.size() < 1)
			usage();
		
		try {
			final int n = Integer.parseInt(params.get(0));
			if (n < 1)
				usage();
			final SET948 model = new SET948();
			final Solver solver = new Solver();
			solver.options().setSolver(SATFactory.MiniSat);
//			solver.options().setSymmetryBreaking(n*n);
//			solver.options().setFlatten(false);
			final Formula f = model.axioms().and(model.t101_zfmisc_1().not());
			final Bounds b = model.bounds(n);
			return new Problem(f,b);
		} catch (NumberFormatException nfe) {
			usage();
		}
		return null;
	}
	
	/**
	 * Returns an com008 problem with the given parameters.
	 * @return an com008 problem with the given parameters.
	 */
	static Problem com008(List<String> params) { 
		if (params.size() < 1)
			usage();
		
		try {
			final int n = Integer.parseInt(params.get(0));
			if (n < 1)
				usage();
			final COM008 model = new COM008();
			final Formula f = model.axioms().and(model.goalToBeProved());
			final Bounds b = model.bounds(n);
			return new Problem(f,b);
		} catch (NumberFormatException nfe) {
			usage();
		}
		return null;
	}
	/**
	 * Returns an a241 problem with the given parameters.
	 * @return an a241 problem with the given parameters.
	 */
	static Problem a241(List<String> params) { 
		if (params.size() < 1)
			usage();
		try {
			final int n = Integer.parseInt(params.get(0));
			if (n < 1)
				usage();
			final AbstractWorldDefinitions model = new AbstractWorldDefinitions();
			final Solver solver = new Solver();
			solver.options().setSolver(SATFactory.MiniSat);
//			solver.options().setSymmetryBreaking(n*n);
//			solver.options().setFlatten(false);
			final Formula f = model.decls().and(model.AbOp_total().not());
			final Bounds b = model.bounds(n);
			return new Problem(f,b);
		} catch (NumberFormatException nfe) {
			usage();
		}
		return null;
	}
	
	/**
	 * Returns an lat258 problem with the given parameters.
	 * @return an lat258 problem with the given parameters.
	 */
	static Problem lat258(List<String> params) { 
		if (params.size() < 1)
			usage();
		try {

			final int n = Integer.parseInt(params.get(0));
			final LAT258 model = new LAT258();
			
			final Bounds b = model.bounds(n);
						
			final Formula f = model.axioms().and(model.goalToBeProved().not());
			return new Problem(f,b);
	
		} catch (NumberFormatException nfe) {
			usage();
		} catch (HigherOrderDeclException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (UnboundLeafException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} 
		return null;
	}
	
	/**
	 * Returns an handshake problem with the given parameters.
	 * @return an handshake problem with the given parameters.
	 */
	static Problem handshake(List<String> params) { 
		if (params.size() < 1)
			usage();
		
		try {
			final Handshake model =  new Handshake();
			final int persons = Integer.parseInt(params.get(0));
			if (persons<2) usage();
		
			final Bounds b = model.bounds(persons);
			final Formula f = model.runPuzzle();//.and(model.Person.count().eq(IntConstant.constant(persons)));
			final Problem p = new Problem(f,b);
			p.solver.options().setBitwidth(6);
			p.solver.options().setSymmetryBreaking(0);
			return p;
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
		System.out.println(" top020 #atoms");
		System.out.println(" geo081 #atoms");
		System.out.println(" med007 #atoms");
		System.out.println(" med009 #atoms");
		System.out.println(" alg212 #atoms");
		System.out.println(" alg195");
		System.out.println(" num374 #atoms");
		System.out.println(" set943 #atoms");
		System.out.println(" set948 #atoms");
		System.out.println(" A241 #atoms");
		System.out.println(" com008 #atoms");
		System.out.println(" lat258 #atoms");
		System.out.println(" handshake #atoms");
		System.exit(1);
	}
	
	/**
	 * Checks that the the given proof of unsatisfiablity for the given problem is
	 * correct (i.e. the conjunction of its core formulas is unsat).
	 */
	private static void checkCorrect(Problem problem, Set<Formula> core) {
		System.out.print("checking correctness ... ");
		final long start = System.currentTimeMillis();
		Formula coreFormula = Formula.TRUE;
		for(Formula f : core) {
			coreFormula = coreFormula.and(f);
		}
		
		final Solver solver = new Solver();
		solver.options().setLogTranslation(false);
		solver.options().setSolver(SATFactory.MiniSat);
//		solver.options().setReporter(new ConsoleReporter());
		final Solution sol = solver.solve(coreFormula, problem.bounds);
		final long end = System.currentTimeMillis();
		if (sol.instance()==null ) {
			System.out.println("correct (" + (end-start) + " ms).");
		} else {
			System.out.println("incorrect. The found core is satisfiable! (" + (end-start)+" ms).");
		}
	}
	
	/**
	 * Checks that the given proof of unsatisfiablity for the given problem is miminal.
	 * This method assumes that the given proof is correct.
	 */
	private static void checkMinimal(Problem problem, Set<Formula> core) {
		System.out.print("checking minimality ... ");
		final long start = System.currentTimeMillis();
		final Set<Formula> minCore = new LinkedHashSet<Formula>(core);
		for(Iterator<Formula> itr = minCore.iterator(); itr.hasNext();) {
			Formula f = itr.next();
			Formula noF = Formula.TRUE;
			for( Formula f1 : minCore ) {
				if (f!=f1)
					noF = noF.and(f1);
			}
			if (problem.solver.solve(noF, problem.bounds).instance()==null) {
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
	 * Times the naive core extraction algorithm.
	 */
	private static Set<Formula> timeNaive(Problem problem) { 
		
		Solution sol = problem.solve();
		if (sol.outcome()==Solution.Outcome.UNSATISFIABLE) { 
			
			
			System.out.print("\nfinding minimal core with naive algorithm ... ");
			final long start = System.currentTimeMillis();
			
			final Set<Formula> core = new LinkedHashSet<Formula>(sol.proof().highLevelCore());
			final Set<Formula> unknown = new LinkedHashSet<Formula>(core);
			
			while(!unknown.isEmpty()) {
				
				Formula f = unknown.iterator().next();
//				System.out.println("excluding:  " + f);
				
				Set<Formula> tryCore = new LinkedHashSet<Formula>(core);
				tryCore.remove(f);
				
				sol = problem.solver.solve(Nodes.and(tryCore), problem.bounds);
				
				if (sol.instance()==null) { // unsat: f is irrelevant
					core.retainAll(sol.proof().highLevelCore());
					unknown.retainAll(core);
				} else {// sat:  f is relevant
					unknown.remove(f);
				}
			}
			
			final long end = System.currentTimeMillis();
			
			System.out.println((end-start) + " ms");
			System.out.println("top-level formulas after min: " + core.size());
//			System.out.println("core:");
//			for(Formula f : core) {
//				System.out.println(PrettyPrinter.print(f,2));
//			}
			return core;
		}
		return Collections.emptySet();
		
	}
	
//	private static void countMaximallyConnectedComponents(Problem problem) { 
//		final UndirectedGraph<Node, DefaultEdge> g = new SimpleGraph<Node, DefaultEdge>(DefaultEdge.class);
//		final AbstractCollector<Relation> collector = 
//			new AbstractCollector<Relation>((new AnnotatedNode<Formula>(problem.formula)).sharedNodes()) {
//			@Override
//			protected Set<Relation> newSet() { return new LinkedHashSet<Relation>(); }
//			public Set<Relation> visit(Relation r) { return Collections.singleton(r); }
//		};
//		for(Formula f : StrategyUtils.topFormulas(problem.formula)) { 
//			g.addVertex(f);
//			for(Relation r : f.accept(collector)) { 
//				g.addVertex(r);
//				g.addEdge(f, r);
//			}
//		}
//		
//		System.out.println("connected components: "+(new ConnectivityInspector<Node, DefaultEdge>(g)).connectedSets().size());
//		
//		System.out.println("cut points: ");
//		for(Node n : new BiconnectivityInspector<Node, DefaultEdge>(g).getCutpoints()) { 
//			System.out.println(n);
//		}
//		
//	}
	
	/**
	 * Usage:  java tests.UCoreTest <name of test> <scope parameters>
	 */
	public static void main(String[] args) {
		if (args.length < 1) usage();
		
		try {
			final Method method = UCoreTest.class.getDeclaredMethod(args[0], List.class);
			final Problem problem = (Problem) method.invoke(null, Arrays.asList(args).subList(1, args.length));
			final Solution sol = problem.solve();
			
			System.out.println(sol);
			if (sol.outcome()==Solution.Outcome.UNSATISFIABLE) {
				final Proof proof = sol.proof();
				System.out.println("top-level formulas: " + StrategyUtils.topFormulas(problem.formula).size());
				System.out.println("top-level formulas in initial core: " + proof.highLevelCore().size());
//				for(Formula f : proof.highLevelCore()) {
//					System.out.println(PrettyPrinter.print(f,2));
//				}

				long start = System.currentTimeMillis();
				proof.minimize(new MinTopStrategy(proof.log()));
//				proof.minimize(new HybridStrategy(proof.log()));
				
				long end = System.currentTimeMillis();
				final Set<Formula> topCore = proof.highLevelCore();
				System.out.println("\ntop-level formulas in MinTop core: " + topCore.size());
				System.out.println("time: " + (end-start) + " ms");

				checkCorrect(problem, proof.highLevelCore());
				checkMinimal(problem, proof.highLevelCore());
				
				final Set<Formula> naiveCore = timeNaive(problem);
				System.out.println("\nnaive core = prev core: " + topCore.equals(naiveCore));
				System.out.println("core: ");
				for(Formula f : topCore) {
					System.out.println(PrettyPrinter.print(f,2));
				}
//				countMaximallyConnectedComponents(problem);
				
			} else if (sol.outcome()==Solution.Outcome.TRIVIALLY_UNSATISFIABLE) { 
				System.out.println("core: ");
				for(Formula f : sol.proof().highLevelCore()) {
					System.out.println(PrettyPrinter.print(f,2,100));
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
	
	private static final class Problem { 
		final Formula formula;
		final Bounds bounds;
		final Solver solver;
		
		Problem(Formula formula, Bounds bounds) {
			this.formula = formula; 
			this.bounds = bounds;
			this.solver = new Solver();
			solver.options().setLogTranslation(true);
			solver.options().setSolver(SATFactory.MiniSatProver);
//			solver.options().setSymmetryBreaking(0);
		}
		
		/**
		 * Solves this problem with translation logging on,
		 * using MiniSatProver.
		 * @return a solution to this problem generated with 
		 * with translation logging on, using MiniSatProver.
		 */
		Solution solve() {
			return solver.solve(formula, bounds);
		}
	}
	
}
