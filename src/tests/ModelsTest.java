package tests;

import java.util.LinkedList;
import java.util.List;

import junit.framework.TestCase;
import kodkod.ast.Formula;
import kodkod.ast.Relation;
import kodkod.ast.Variable;
import kodkod.engine.Solver;
import kodkod.engine.TimeoutException;
import kodkod.instance.Bounds;
import kodkod.instance.Instance;
import kodkod.instance.TupleFactory;
import kodkod.instance.Universe;

public class ModelsTest extends TestCase {

	private final Solver solver;
	
	public ModelsTest(String arg0) {
		super(arg0);
		this.solver = new Solver();
//		this.solver.options().setSolver(SATFactory.ZChaff);
		
	}

	protected void setUp() throws Exception {
		super.setUp();
	}

	protected void tearDown() throws Exception {
		super.tearDown();
	}
	
	private Instance solve(Formula formula, Bounds bounds) {
		try {
			return solver.solve(formula, bounds).instance();
		} catch (TimeoutException te) {
			fail("Timed out solving " + formula);
			return null;
		}
	}
	
	private Bounds ceilingsAndFloorsInstance(Relation platform, int platformScope, 
			                                   Relation man, int manScope, Relation ceiling, Relation floor) {
		final List<String> atoms = new LinkedList<String>();
		for (int i = 0; i < manScope; i++) {
			atoms.add("Man" + i);
		}
		for (int i = 0; i < platformScope; i++) {
			atoms.add("Platform" + i);
		}
		final Universe universe = new Universe(atoms);
		final TupleFactory factory = universe.factory();
		final Bounds bounds = new Bounds(universe);
		final String manMax = "Man" + (manScope - 1), platformMax = "Platform" + (platformScope - 1);
		
		bounds.bound(platform, factory.range(factory.tuple("Platform0"), factory.tuple(platformMax)));
		bounds.bound(man, factory.range(factory.tuple("Man0"), factory.tuple(manMax)));
		bounds.bound(ceiling, factory.area(factory.tuple("Man0", "Platform0"), factory.tuple(manMax, platformMax)));
		bounds.bound(floor, factory.area(factory.tuple("Man0", "Platform0"), factory.tuple(manMax, platformMax)));
		
		
		return bounds;
	}
	
	public void testCeilingsAndFloors() {
		
		// sigs
		final Relation platform = Relation.unary("Platform");
		final Relation man = Relation.unary("Man");
		
		// fields
		final Relation ceiling = Relation.binary("ceiling");
		final Relation floor = Relation.binary("floor");
		
		final Variable m = Variable.unary("m"), n = Variable.unary("n");
		
		// ceiling and floor are functions from Man to Platform
		final Formula fieldConstraints = ceiling.function(man,platform).and(floor.function(man,platform));
		
		// all m: Man | some n: Man | n.floor = m.ceiling
		final Formula paulSimon = ((n.join(floor).eq(m.join(ceiling))).forSome(n.oneOf(man)).forAll(m.oneOf(man)));
		// all m: Man | some n: Man | m.floor = n.ceiling
		final Formula belowToo = ((m.join(floor).eq(n.join(ceiling))).forSome(n.oneOf(man)).forAll(m.oneOf(man)));
		
		final Formula model = fieldConstraints.and(paulSimon);
		
		Bounds bounds = ceilingsAndFloorsInstance(platform,2, man, 2,ceiling,floor);
		
		//System.out.println(solve(model.and(belowToo.not()), bounds));
		assertNotNull(solve(model.and(belowToo.not()), bounds));
		
		// all m : Man | m.ceiling != m.floor
		final Formula geometry = (m.join(ceiling).eq(m.join(floor)).not()).forAll(m.oneOf(man));
		
		bounds = ceilingsAndFloorsInstance(platform, 3, man, 3, ceiling, floor);
		
//		System.out.println(solve(((model.and(geometry)).implies(belowToo)).not(), bounds));
		assertNotNull(solve(((model.and(geometry)).implies(belowToo)).not(), bounds));
		
		// all m, n: Man | !(m = n) => !(m.floor = n.floor || m.ceiling = n.ceiling) 
		final Formula body = (m.join(floor).eq(n.join(floor))).or(m.join(ceiling).eq(n.join(ceiling)));
		final Formula noSharing = (m.eq(n).not().implies(body.not())).forAll(m.oneOf(man).and(n.oneOf(man)));
		
		bounds = ceilingsAndFloorsInstance(platform, 4, man, 4, ceiling, floor);
		assertNull(solve(((model.and(noSharing)).implies(belowToo)).not(), bounds));
//		System.out.println(solve(((model.and(noSharing)).implies(belowToo)).not(), bounds));
//		System.out.println(solve(model.and(noSharing).and(belowToo.not()), bounds));
//		System.out.println((solver.numberOfPrimaryVariables() +  solver.numberOfIntermediateVariables()) + " " + solver.numberOfClauses());
	}
}
