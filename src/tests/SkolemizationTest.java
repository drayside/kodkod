package tests;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import junit.framework.TestCase;
import kodkod.ast.Decl;
import kodkod.ast.Decls;
import kodkod.ast.Formula;
import kodkod.ast.Relation;
import kodkod.ast.Variable;
import kodkod.engine.Solver;
import kodkod.engine.TimeoutException;
import kodkod.instance.Bounds;
import kodkod.instance.Instance;
import kodkod.instance.TupleFactory;
import kodkod.instance.Universe;

/**
 * Tests skolemization. 
 */
public class SkolemizationTest extends TestCase {
	private final int USIZE = 10;
	private final TupleFactory factory;
	private final Solver solver;
	private final Relation r1a, r1b, r2a, r2b;
	private final Bounds bounds;
	
	public SkolemizationTest(String arg0) {
		super(arg0);
		this.solver = new Solver(Solver.SATSolverName.Mini3SAT);
		List<String> atoms = new ArrayList<String>(USIZE);
		for (int i = 0; i < USIZE; i++) {
			atoms.add(""+i);
		}
		final Universe universe = new Universe(atoms);
		this.factory = universe.factory();
		this.r1a = Relation.unary("r1a");
		this.r1b = Relation.unary("r1b");
		this.r2a = Relation.binary("r2a");
		this.r2b = Relation.binary("r2b");
		this.bounds = new Bounds(universe);
	}

	protected void setUp() throws Exception {
		super.setUp();
		bounds.bound(r1a, factory.setOf("0","1","2","3","4"));
		bounds.bound(r1b, factory.setOf("5","6","7","8","9"));
		bounds.bound(r2a, bounds.upperBound(r1a).product(bounds.upperBound(r1b)));
		bounds.bound(r2b, bounds.upperBound(r1b).product(bounds.upperBound(r1a)));
	}
	
	private Instance solve(Formula formula) {
		try {
			return solver.solve(formula, bounds);
		} catch (TimeoutException te) {
			fail("Timed out solving " + formula);
			return null;
		}
	}

	private final void testNoSkolems(Decls d, Formula f) {
		Instance inst = solve(f.not());
		assertEquals(bounds.relations(), inst.relations());
		
		inst = solve(f.and(f.thenElse(r1a, r1b).some()));
		assertEquals(bounds.relations(), inst.relations());
		
		inst = solve(f.and(f.comprehension(d).some()));
		assertEquals(bounds.relations(), inst.relations());
		
		inst = solve(f.and(f.comprehension(d).in(r1a)));
		assertEquals(bounds.relations(), inst.relations());
		
		inst = solve(f.or(r2a.in(r2b)));
		assertEquals(bounds.relations(), inst.relations());
		
		inst = solve(f.implies(r2a.in(r2b)));
		assertEquals(bounds.relations(), inst.relations());
		
		inst = solve(r2a.in(r2b).implies(f));
		assertEquals(bounds.relations(), inst.relations());
		
	}
	
	public final void testNoSkolems() {
		final Variable v = Variable.unary("v");
		final Decl d = v.oneOf(r1a);
		
		testNoSkolems(d, v.join(r2a).some().forAll(d).not());
		testNoSkolems(d, v.join(r2a).some().forSome(d));
	}
	
	private final void assertSkolems(Bounds bounds, Instance inst, Set<String> skolems) {
		assertEquals(skolems.size(), inst.relations().size() - bounds.relations().size());
		for(Relation r: inst.relations()) {
			assertFalse(skolems.contains(r.name()) && bounds.contains(r));
		}
	}
	
	public final void testSkolems() {
		final Variable va = Variable.unary("va");
		final Variable vb = Variable.unary("vb");
		final Set<String> skolems = new HashSet<String>(4);
		
		Decl da = va.oneOf(r1a);
		Decl db = vb.oneOf(r1b);
		
		skolems.add(va.name());
		
		Instance inst = solve(va.in(r1b.join(r2b)).forAll(da).not());
		assertSkolems(bounds, inst, skolems);
		
		inst = solve((r2b.some().implies(va.in(r1b.join(r2b)).forAll(da))).not());
		assertSkolems(bounds, inst, skolems);
		
		inst = solve(va.in(r1b.join(r2b)).forSome(da));
		assertSkolems(bounds, inst, skolems);
		
		inst = solve(va.in(r1b.join(r2b)).forSome(da).and(va.in(r1b).not().forAll(da)));
		assertSkolems(bounds, inst, skolems);
		
		inst = solve((va.in(vb.join(r2b)).forAll(vb.oneOf(va.join(r2a))).forAll(da)).not());
		assertSkolems(bounds, inst, skolems);
		
		inst = solve(va.in(vb.join(r2b)).forSome(vb.oneOf(va.join(r2a))).forSome(da));
		assertSkolems(bounds, inst, skolems);
		
		skolems.add(vb.name());
		
		inst = solve(va.in(vb.join(r2b)).forAll(da.and(db)).not());
		assertSkolems(bounds, inst, skolems);
		
		inst = solve(va.in(vb.join(r2b)).forSome(da.and(db)));
		assertSkolems(bounds, inst, skolems);
		
		inst = solve(va.in(r1b.join(r2b)).forSome(da).and(r1b.in(vb).forAll(db).not()));
		assertSkolems(bounds, inst, skolems);
		
	}
}
