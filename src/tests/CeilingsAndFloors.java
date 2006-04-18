package tests;

import java.util.LinkedList;
import java.util.List;

import kodkod.ast.Formula;
import kodkod.ast.Relation;
import kodkod.ast.Variable;
import kodkod.engine.Solution;
import kodkod.engine.Solver;
import kodkod.engine.TimeoutException;
import kodkod.engine.satlab.SATFactory;
import kodkod.instance.Bounds;
import kodkod.instance.TupleFactory;
import kodkod.instance.Universe;

public final class CeilingsAndFloors {
	private final Relation Platform, Man, ceiling, floor;
	
	CeilingsAndFloors() {
		Platform = Relation.unary("Platform");
		Man = Relation.unary("Man");
		ceiling = Relation.binary("ceiling");
		floor = Relation.binary("floor");
	}
	private Formula declarations() {
		// ceiling and floor are functions from Man to Platform
		return ceiling.function(Man,Platform).and(floor.function(Man,Platform));
	}
	
	private Formula belowToo() {
		final Variable m = Variable.unary("m"), n = Variable.unary("n");
//		 all m: Man | some n: Man | m.floor = n.ceiling
		return ((m.join(floor).eq(n.join(ceiling))).forSome(n.oneOf(Man)).forAll(m.oneOf(Man)));

	}
	
	private Formula noSharing() {
		final Variable m = Variable.unary("m"), n = Variable.unary("n");
//		 all m, n: Man | !(m = n) => !(m.floor = n.floor || m.ceiling = n.ceiling) 
		final Formula body = (m.join(floor).eq(n.join(floor))).or(m.join(ceiling).eq(n.join(ceiling)));
		return (m.eq(n).not().implies(body.not())).forAll(m.oneOf(Man).and(n.oneOf(Man)));
	}
	
	private Formula paulSimon() {
		final Variable m = Variable.unary("m"), n = Variable.unary("n");
//		 all m: Man | some n: Man | n.floor = m.ceiling
		return ((n.join(floor).eq(m.join(ceiling))).forSome(n.oneOf(Man)).forAll(m.oneOf(Man)));

	}
	
	private Formula assertion() {
		return declarations().and(paulSimon()).and(noSharing()).and(belowToo().not());
	}
	
	private Bounds bounds(int platformScope,int manScope) {
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
		
		bounds.bound(Platform, factory.range(factory.tuple("Platform0"), factory.tuple(platformMax)));
		bounds.bound(Man, factory.range(factory.tuple("Man0"), factory.tuple(manMax)));
		bounds.bound(ceiling, factory.area(factory.tuple("Man0", "Platform0"), factory.tuple(manMax, platformMax)));
		bounds.bound(floor, factory.area(factory.tuple("Man0", "Platform0"), factory.tuple(manMax, platformMax)));
		
		
		return bounds;
	}
	
	public static void main(String[] args) {
		final CeilingsAndFloors model = new CeilingsAndFloors();
		final Solver solver = new Solver();
		final int m = Integer.parseInt(args[0]);
		final int p = Integer.parseInt(args[1]);
		solver.options().setSolver(SATFactory.ZChaff);
		//solver.options().setSymmetryBreaking(p);
		solver.options().setFlatten(true);
		try {
			final Formula show = model.declarations().and(model.assertion());
			final Solution sol = solver.solve(show, model.bounds(m,p));
			//System.out.println(show);
			System.out.println(sol);
			
		} catch (TimeoutException e) {
			System.out.println("timed out.");
			e.printStackTrace();
		} catch (NumberFormatException nfe) {
			System.out.println("Usage: java tests.CeilingsAndFloors [# man] [# platform]");
		}
	}
	
}
