/*
 * TranslatorTest.java
 * Created on Jul 6, 2005
 */
package tests;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import junit.framework.TestCase;
import relcalc.ast.Formula;
import relcalc.ast.MultiplicityFormula;
import relcalc.ast.QuantifiedFormula;
import relcalc.ast.Relation;
import relcalc.ast.Variable;
import relcalc.engine.Solver;
import relcalc.engine.TimeoutException;
import relcalc.instance.Bounds;
import relcalc.instance.Instance;
import relcalc.instance.Tuple;
import relcalc.instance.TupleFactory;
import relcalc.instance.Universe;


/** 
 * Tests {@link relcalc.core.fol2sat.Fol2SatTranslator FOL to SAT translation} and
 * {@link relcalc.core.sat2cnf.Sat2CnfTranslator SAT to CNF translation}.
 *
 * @author Emina Torlak 
 */
public class TranslatorTest extends TestCase {
	
	private final TupleFactory factory;
	private final Solver solver;
	private final Relation[] r1, r2, r3;
	private final Tuple t112, t212, t312;
	private final Bounds bounds;
	
	public TranslatorTest(String arg0) {
		super(arg0);
		this.solver = new Solver(Solver.SATSolverName.MiniSAT);
		List<Integer> atoms = new ArrayList<Integer>(10);
		for (int i = 0; i < 10; i++) {
			atoms.add(i);
		}
		final Universe universe = new Universe(atoms);
		this.factory = universe.factory();
		this.t112 = factory.tuple(1, 4);
		this.t212 = factory.tuple(2, 57);
		this.t312 = factory.tuple(3, 345);
		this.r1 = new Relation[4];
		this.r2 = new Relation[4];
		this.r3 = new Relation[4];
		for (int i = 0; i < 4; i++) {
			
			r1[i] = Relation.unary("r1" + i);
			r2[i] = Relation.binary("r2" + i);
			r3[i] = Relation.ternary("r3" + i);
			
			
		}
		
		bounds = new Bounds(universe);
		
		bounds.bound(r1[0],	factory.allOf(1));
		bounds.bound(r1[1],	factory.range(factory.tuple(2), t112));
		bounds.bound(r1[2],	factory.range(t112, t112));
		bounds.bound(r1[3],	factory.range(factory.tuple(5),factory.tuple(8)));
		
		bounds.bound(r2[0],	factory.allOf(2));
		bounds.bound(r2[1],	factory.area(factory.tuple(2,35),t212));
		bounds.bound(r2[2],	factory.setOf(t212));
		bounds.bound(r2[3],	factory.area(factory.tuple(2,60), factory.tuple(2,86)));
		
		bounds.bound(r3[0],	factory.allOf(3));
		bounds.bound(r3[1],	factory.area(factory.tuple(3,123), t312));
		bounds.bound(r3[2],	factory.setOf(t312));
		bounds.bound(r3[3],	factory.area(factory.tuple(3,700),factory.tuple(3,897)));
		
//		System.out.println(bounds.upperBound(r3[0]).size());
//		System.out.println(bounds.upperBound(r3[1]).size());
//		System.out.println(bounds.upperBound(r3[2]).size());
//		System.out.println(bounds.upperBound(r3[3]).size());
		
		
	}
	
	private Instance solve(Formula formula) {
		try {
			return solver.solve(formula, bounds);
		} catch (TimeoutException te) {
			fail("Timed out solving " + formula);
			return null;
		}
	}
	
	private boolean isSatisfiable(Formula formula) {
		return (solve(formula)==null ? false : true);
	}
	
	
	
	
	private final void testIntersectionMultiplicity(MultiplicityFormula.Multiplicity mult, Relation p, Relation q, Tuple intersection) {
		final Instance m = solve(p.intersection(q).apply(mult));
		assertNotNull(m);
		final Set<Tuple> ps = m.tuples(p), qs = m.tuples(q); 
		assertFalse(ps.isEmpty());
		assertFalse(qs.isEmpty());
		assertTrue(ps.contains(intersection));
		assertTrue(qs.contains(intersection));
	}
	
	private final void testTranslateNonEmptyMultiplicity(MultiplicityFormula.Multiplicity mult) {
		
		testTranslateMultiplicity(mult, true, false);
		testIntersectionMultiplicity(mult, r1[1], r1[2], t112);
		testIntersectionMultiplicity(mult, r2[1], r2[2], t212);
		testIntersectionMultiplicity(mult, r3[1], r3[2], t312);
	}
	
	private final void testTranslateMultiplicity(MultiplicityFormula.Multiplicity mult, boolean trueTest, boolean falseTest) {
		for (int i = 0; i < 4; i++) {
			assertTrue(isSatisfiable(r1[i].apply(mult)));
			assertTrue(isSatisfiable(r2[i].apply(mult)));
//			assertTrue(solve(r3[i].apply(mult)));
		}
		
		// mult rx1 & rx3
		assertEquals(falseTest, isSatisfiable(r1[1].intersection(r1[3]).apply(mult)));
		assertEquals(falseTest, isSatisfiable(r2[1].intersection(r2[3]).apply(mult)));
		assertEquals(falseTest, isSatisfiable(r3[1].intersection(r3[3]).apply(mult)));
		
		// mult rx3 - rx3
		assertEquals(falseTest, isSatisfiable(r1[3].difference(r1[3]).apply(mult)));
		assertEquals(falseTest, isSatisfiable(r2[3].difference(r2[3]).apply(mult)));
		assertEquals(falseTest, isSatisfiable(r3[3].difference(r3[3]).apply(mult)));
		
		// mult r11->r13 & r21
		assertEquals(trueTest, isSatisfiable(r1[1].product(r1[3]).intersection(r2[1]).apply(mult)));
		// mult r11->r21 & r31
		assertEquals(trueTest, isSatisfiable(r1[1].product(r2[1]).intersection(r3[1]).apply(mult)));
		
		// mult rx1 + rx3
		assertEquals(trueTest, isSatisfiable(r1[1].union(r1[3]).apply(mult)));
		assertEquals(trueTest, isSatisfiable(r2[1].union(r2[3]).apply(mult)));
		assertEquals(trueTest, isSatisfiable(r3[1].union(r3[3]).apply(mult)));
		
		// mult r21.r13
		assertEquals(trueTest, isSatisfiable(r2[1].join(r1[3]).apply(mult)));
		// mult r31.r21
		assertEquals(trueTest, isSatisfiable(r3[1].join(r2[1]).apply(mult)));
		
		// mult ^r21
		assertEquals(trueTest, isSatisfiable(r2[1].closure().apply(mult)));
		// mult ~r23
		assertEquals(trueTest, isSatisfiable(r2[3].transpose().apply(mult)));
	}
	
	public final void testTranslateMultiplicityFormula_NO() {
		testTranslateMultiplicity(MultiplicityFormula.Multiplicity.NO, true, true);
	}
	
	public  final void testTranslateMultiplicityFormula_LONE() {
//		testTranslateMultiplicity(MultiplicityFormula.Multiplicity.LONE, true, true);
		assertEquals(true, isSatisfiable(r3[1].union(r3[3]).apply(MultiplicityFormula.Multiplicity.LONE)));
//		assertEquals(true, isSatisfiable(r3[3].difference(r3[1]).apply(MultiplicityFormula.Multiplicity.LONE)));
	}
	
	
	public  final void testTranslateMultiplicityFormula_ONE() {
//		testTranslateNonEmptyMultiplicity(MultiplicityFormula.Multiplicity.ONE);
	}
	
	public  final void testTranslateMultiplicityFormula_SOME() {
		testTranslateNonEmptyMultiplicity(MultiplicityFormula.Multiplicity.SOME);
	}	
	
	public final void testTranslateComparisonFormula() {
		for (int i = 0; i < 4; i++) {
			assertTrue(isSatisfiable(r1[i].in(r1[i])));
			assertTrue(isSatisfiable(r2[i].in(r2[i])));
			assertTrue(isSatisfiable(r3[i].in(r3[i])));
		}
		
		// some rx2 && (rx1 & rx2 in rx2)
		assertTrue(isSatisfiable(r1[2].some().and(r1[1].intersection(r1[2]).in(r1[2]))));
		assertTrue(isSatisfiable(r2[2].some().and(r2[1].intersection(r2[2]).in(r2[2]))));
		assertTrue(isSatisfiable(r3[2].some().and(r3[1].intersection(r3[2]).in(r3[2]))));
		
		// one rx2 && (rx1 & rx2 = rx2)
		assertTrue(isSatisfiable(r1[2].one().and(r1[1].intersection(r1[2]).eq(r1[2]))));
		assertTrue(isSatisfiable(r2[2].one().and(r2[1].intersection(r2[2]).eq(r2[2]))));
		assertTrue(isSatisfiable(r3[2].one().and(r3[1].intersection(r3[2]).eq(r3[2]))));
		
		// !(rx3 in rx0)
		assertTrue(isSatisfiable(r1[3].in(r1[0]).not()));
		assertTrue(isSatisfiable(r2[3].in(r2[0]).not()));
		assertTrue(isSatisfiable(r3[3].in(r3[0]).not()));
		
		// some rx0 && (rx1 + rx2 + rx3 in rx0)
		assertTrue(isSatisfiable(r1[0].some().and(r1[1].union(r1[2]).union(r1[3]).in(r1[0]))));
		assertTrue(isSatisfiable(r2[0].some().and(r2[1].union(r2[2]).union(r2[3]).in(r2[0]))));
//		assertTrue(isSatisfiable(r3[0].some().and(r3[1].union(r3[2]).union(r3[3]).in(r3[0]))));
		
		// some r11 && some r13 && r11->r13 = r21
		assertTrue(isSatisfiable(r1[1].some().and(r1[3].some()).and(r1[1].product(r1[3]).eq(r2[1]))));
		
		// r21.r13 in r11
		assertTrue(isSatisfiable(r2[1].join(r1[3]).in(r1[1])));
		
	}
	
	private final void testTranslateQuantifiedFormula(QuantifiedFormula.Quantifier quant) {
		// quant v1: r1[i] | v1 in rx0
		final Variable v1 = Variable.unary("v1"), v2 =Variable.unary("v2");
		for (int i = 1; i < 4; i++) {		
			assertTrue(isSatisfiable(v1.in(r1[0]).quantify(quant, v1.oneOf(r1[i]))));
		}
		
		// quant v1 : r1[2], v2: r1[3] | v1->v2 in r2[1]
		assertTrue(isSatisfiable(v1.product(v2).in(r2[1]).quantify(quant, v1.oneOf(r1[2]).and(v2.oneOf(r1[3])))));
		
		// quant v1 : r1[3] | some r3[3].v1 & r2[3]
		assertTrue(isSatisfiable(r3[3].join(v1).intersection(r2[3]).some().quantify(quant, v1.oneOf(r1[3]))));
		
		// quant v1 : r1[3] | some v1.(~r2[1])
		assertTrue(isSatisfiable(v1.join(r2[1].transpose()).some().quantify(quant, v1.oneOf(r1[3]))));
		// quant v1: r1[3] | some v1.(^r2[3])
		assertTrue(isSatisfiable(v1.join(r2[3].closure()).some().quantify(quant, v1.oneOf(r1[3]))));
		
	}
	
	public final void testTranslateQuantifiedFormula_ALL() {
		testTranslateQuantifiedFormula(QuantifiedFormula.Quantifier.ALL);	
	}
	
	public final void testTranslateQuantifiedFormula_SOME() {
		testTranslateQuantifiedFormula(QuantifiedFormula.Quantifier.SOME);
	}
	
	public final void testTranslateComprehension() {
		final Variable v1 = Variable.unary("v1"), v2 = Variable.unary("v2");
		
		// some { v1: r11 + r12 | some v1.r21 }
		assertTrue(isSatisfiable((v1.join(r2[1]).some()).comprehension(v1.oneOf(r1[1].union(r1[2]))).some()));
		
		// one { v1: r13, v2: r12 | v1->v2 in r23 }
		assertTrue(isSatisfiable((v1.product(v2).in(r2[3]).comprehension(v1.oneOf(r1[3]).and(v2.oneOf(r1[2])))).one()));
		
		// one { v1: r13, v2: r12 | v2->v1 in ~r23 }
		assertTrue(isSatisfiable((v2.product(v1).in(r2[3].transpose()).comprehension(v1.oneOf(r1[3]).and(v2.oneOf(r1[2])))).one()));
		
	}
	
	
}
