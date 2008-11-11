/**
 * 
 */
package tests.benchmarks;

import java.lang.reflect.Method;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import junit.framework.TestCase;
import kodkod.ast.Formula;
import kodkod.engine.Proof;
import kodkod.engine.Solution;
import kodkod.engine.Solver;
import kodkod.engine.satlab.ReductionStrategy;
import kodkod.engine.satlab.SATFactory;
import kodkod.engine.ucore.RCEStrategy;
import kodkod.engine.ucore.SCEStrategy;
import kodkod.instance.Bounds;
import examples.alloy.Hotel;
import examples.alloy.Lists;
import examples.alloy.RingElection;
import examples.alloy.Toughnut;
import examples.alloy.Trees;
import examples.tptp.ALG212;
import examples.tptp.COM008;
import examples.tptp.GEO091;
import examples.tptp.GEO092;
import examples.tptp.GEO115;
import examples.tptp.GEO158;
import examples.tptp.GEO159;
import examples.tptp.LAT258;
import examples.tptp.MED007;
import examples.tptp.MED009;
import examples.tptp.NUM374;
import examples.tptp.NUM378;
import examples.tptp.SET943;
import examples.tptp.SET948;
import examples.tptp.SET967;
import examples.tptp.TOP020;

/**
 * Tests the unsat core functionality.
 * @author Emina Torlak
 */
public final class UCoreTest extends TestCase {
	
	
	private static final Class<?>[] FIXED_SCOPE = {
		NUM378.class 
	};
	
	// scopes 1-10
	private static final Class<?>[] EASY = {
		Toughnut.class, MED007.class, MED009.class,
	};
	
	// scopes 1-5
	private static final Class<?>[] MEDIUM = {
		Lists.class, Trees.class, Hotel.class, 
		RingElection.class, 
		COM008.class, TOP020.class, 
		GEO091.class, GEO092.class, GEO115.class, 
		GEO158.class, GEO159.class, LAT258.class, 
	};
	
	// scopes 1-3
	private static final Class<?>[] HARD = { 
		ALG212.class, NUM374.class, 
		SET943.class, SET948.class, SET967.class,};
	
	private static final int EASY_MAX = 10, MED_MAX = 5, HARD_MAX = 3;
	
	private final Solver solver;

	public UCoreTest() {
		solver = new Solver();
		solver.options().setLogTranslation(2);
		solver.options().setSolver(SATFactory.MiniSatProver);		
	}
	
		
	private final void minimizeAndVerify(Bounds bounds, Proof proof, ReductionStrategy strategy) { 
		proof.minimize(strategy);
		final Set<Formula> core = proof.highLevelCore();
	
//		System.out.println(PrettyPrinter.print(Formula.and(proof.log().roots()), 2));
//		System.out.println("-------");
//		System.out.println(PrettyPrinter.print(Formula.and(core), 2));
//		System.out.println("-----");
		
		// check that the conjunction of the high-level core formulas is false
		assertNull(solver.solve(Formula.and(core), bounds).instance());
		
		// check that the core is minimal
		for(Iterator<Formula> itr = core.iterator(); itr.hasNext();) {
			Formula f = itr.next();
			Formula noF = Formula.TRUE;
			for( Formula f1 : core ) {
				if (f!=f1)
					noF = noF.and(f1);
			}
			assertNotNull(solver.solve(noF, bounds).instance());
		}
	}
	
	private final void testTrivialProofExtractor(Class<?>[] probs, int maxScope) { 
		for(Class<?> prob : probs) { 
			Object instance = UCoreStats.instance(prob);
			Map<Method, Formula> checks = UCoreStats.checks(instance, UCoreStats.methods(prob));
			for(Formula check : checks.values()) { 
				for(int scope = 1; scope <= maxScope; scope++ ) { 
					Bounds bounds = UCoreStats.bounds(instance, scope);
					Solution sol = solver.solve(check, bounds);
					if (sol.outcome()==Solution.Outcome.TRIVIALLY_UNSATISFIABLE) { 
						minimizeAndVerify(bounds, sol.proof(), null);
					} else {
						break;
					}
				}
			}
			
		}
	}
	
	private final void testTrivialProofExtractor(Class<?>[] probs) { 
		for(Class<?> prob : probs) { 
			Object instance = UCoreStats.instance(prob);
			Map<Method, Formula> checks = UCoreStats.checks(instance, UCoreStats.methods(prob));
			for(Formula check : checks.values()) { 
				Bounds bounds = UCoreStats.bounds(instance);
				Solution sol = solver.solve(check, bounds);
				if (sol.outcome()==Solution.Outcome.TRIVIALLY_UNSATISFIABLE) {
					minimizeAndVerify(bounds, sol.proof(), null);
				} 
			}
			
		}
	}
	
	public final void testTrivialProofExtractor() { 
		testTrivialProofExtractor(FIXED_SCOPE);
		testTrivialProofExtractor(EASY, EASY_MAX);
		testTrivialProofExtractor(MEDIUM, MED_MAX);
		testTrivialProofExtractor(HARD, HARD_MAX);
	}
	
	private final void testProofExtractor(Class<?>[] probs, Class<? extends ReductionStrategy> strategy, int maxScope) { 
		for(Class<?> prob : probs) { 
			Object instance = UCoreStats.instance(prob);
			Map<Method, Formula> checks = UCoreStats.checks(instance, UCoreStats.methods(prob));
			for(Formula check : checks.values()) { 
				for(int scope = 1; scope <= maxScope; scope++ ) { 
					Bounds bounds = UCoreStats.bounds(instance, scope);
					Solution sol = solver.solve(check, bounds);
					if (sol.outcome()==Solution.Outcome.UNSATISFIABLE) {
						minimizeAndVerify(bounds, sol.proof(), UCoreStats.instance(strategy, sol.proof().log()));
					} 
				}
			}
			
		}
	}
	
	private final void testProofExtractor(Class<?>[] probs, Class<? extends ReductionStrategy> strategy) { 
		for(Class<?> prob : probs) { 
			Object instance = UCoreStats.instance(prob);
			Map<Method, Formula> checks = UCoreStats.checks(instance, UCoreStats.methods(prob));
			for(Formula check : checks.values()) { 
				Bounds bounds = UCoreStats.bounds(instance);
				Solution sol = solver.solve(check, bounds);
				if (sol.outcome()==Solution.Outcome.UNSATISFIABLE) {
					minimizeAndVerify(bounds, sol.proof(), UCoreStats.instance(strategy, sol.proof().log()));
				} 
			}
			
		}
	}
	
	public final void testSCE() {
		testProofExtractor(FIXED_SCOPE, SCEStrategy.class);
		testProofExtractor(EASY, SCEStrategy.class, EASY_MAX);
		testProofExtractor(MEDIUM, SCEStrategy.class, MED_MAX);
		testProofExtractor(HARD, SCEStrategy.class, HARD_MAX);
	}
	
	public final void testRCE() {
		testProofExtractor(FIXED_SCOPE, RCEStrategy.class);
		testProofExtractor(EASY, RCEStrategy.class, EASY_MAX);
		testProofExtractor(MEDIUM, RCEStrategy.class, MED_MAX);
		testProofExtractor(HARD, RCEStrategy.class, HARD_MAX);
	}
	
}
