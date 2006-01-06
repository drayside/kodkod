package tests;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import junit.framework.TestCase;
import relcalc.ast.Formula;
import relcalc.ast.Relation;
import relcalc.engine.Evaluator;
import relcalc.engine.Solver;
import relcalc.engine.TimeoutException;
import relcalc.instance.Bounds;
import relcalc.instance.Instance;
import relcalc.instance.TupleFactory;
import relcalc.instance.Universe;
import relcalc.util.IndexedEntry;
import relcalc.util.TreeSequence;

/**
 * Test cases that record reported crashes. 
 * 
 * @author Emina Torlak
 */
public class CrashesTest extends TestCase {
	private final Solver solver = new Solver(Solver.SATSolverName.Mini3SAT);
	
	public final void testGreg_11232005() {
		final List<String> atoms = new ArrayList<String>(3);
		atoms.add("-1"); atoms.add("0"); atoms.add("1");
		final Universe u = new Universe(atoms);
		final TupleFactory t = u.factory();
		
		final Relation inc = Relation.binary("inc"), add = Relation.ternary("add"), 
		               one = Relation.unary("1"), param0 = Relation.unary("param0"), 
		               ints = Relation.unary("int");
		
		// (one param0 && ((1 . (param0 . add)) in (param0 . ^inc)))
		final Formula f = param0.one().and((one.join(param0.join(add))).in(param0.join(inc.closure())));
		
		final Bounds b = new Bounds(u);
		
		b.bound(param0, t.allOf(1));
		b.boundExactly(one, t.setOf(t.tuple("1")));
		b.boundExactly(ints, t.allOf(1));
		b.boundExactly(inc, t.setOf(t.tuple("-1","0"), t.tuple("0","1")));
		// [1, 1, -1], [1, -1, 0], [1, 0, 1], [-1, 1, 0], [-1, -1, 1],
		// [-1, 0, -1], [0, 1, 1], [0, -1, -1], [0, 0, 0]]
		b.boundExactly(add, t.setOf(t.tuple("1","1","-1"), t.tuple("1","-1","0"), t.tuple("1","0","1"), 
				                  t.tuple("-1","1","0"), t.tuple("-1","-1","1"), t.tuple("-1","0","-1"), 
				                  t.tuple("0","1","1"), t.tuple("0","-1","-1"), t.tuple("0","0","0")));
		
//		System.out.println(f);
//		System.out.println(b);
		
		try {
			final Instance instance = solver.solve(f, b);
			assertTrue((new Evaluator(instance)).evaluate(f));
//			System.out.println(instance);
//			System.out.println((new Evaluator(instance)).evaluate(f  ));
		} catch (TimeoutException te) {
			fail("Timed out solving " + f);
		}
	}
	
	public final void testGreg_01052006() {
		
		final TreeSequence<Integer> t = new TreeSequence<Integer>();
		final int[] elts = {31, 86, 72, 6, 23, 119, 131};
		for(int i = 0; i < elts.length - 1; i++) {
			t.put(elts[i], 0);
		}
		Arrays.sort(elts);
		int count = 0;
		for(IndexedEntry<Integer> e : t) {
			assertEquals(elts[count++], e.index());
		}
		t.remove(72);
		t.put(72,0);
		count = 0;
		for(IndexedEntry<Integer> e : t) {
			assertEquals(elts[count++], e.index());
		}
	
	}
	
	
}
