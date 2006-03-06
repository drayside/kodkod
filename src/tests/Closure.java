package tests;

import java.util.ArrayList;
import java.util.List;

import kodkod.ast.Formula;
import kodkod.ast.Relation;
import kodkod.engine.Solution;
import kodkod.engine.Solver;
import kodkod.engine.TimeoutException;
import kodkod.engine.satlab.SATFactory;
import kodkod.instance.Bounds;
import kodkod.instance.TupleFactory;
import kodkod.instance.TupleSet;
import kodkod.instance.Universe;

/**
 * kodkod encoding of closuretest.als:
 * 
 * <pre>
 *  sig A {
 *  a: set A
 *  }
 * 
 *  pred show() {
 *  some ^a
 *  } 
 * </pre>
 * 
 * @author Emina Torlak
 */
public class Closure {
	private final Relation A, a;
	
	private Closure() {
		super();
		this.A = Relation.unary("A");
		this.a = Relation.binary("a");
	}
	
	/**
	 * Returns the delaration constraints.
	 * @return  sig A {
	 *  a: set A
	 *  }
	 */
	Formula declarations() {
		return a.in(A.product(A));
	}
	
	/**
	 * Returns the show predicate.
	 * @return pred show() {
	 *  some ^a
	 *  } 
	 */
	Formula show() {
		return declarations().and(a.closure().some());
	}
	
	/**
	 * Returns a bounds object that constructs the 'scope' for analyzing
	 * the command, using the given value for the number of As. 
	 * @requires all arguments are positive
	 * @return a bounds for the model
	 */
	Bounds bounds(int anum) {
		assert anum > 0;
		final List<String> atoms = new ArrayList<String>(anum);
		for(int i = 0; i < anum; i++) {
			atoms.add("A"+i);
		}
		final Universe u = new Universe(atoms);
		final TupleFactory f = u.factory();
		
		final Bounds b = new Bounds(u);
		final TupleSet as = f.range(f.tuple("A0"), f.tuple("A" + (anum-1)));
		b.bound(A, as);
		b.bound(a, as.product(as));
		return b;
	}
	
	
	public static void main(String[] args) {
		final Closure model = new Closure();
		final Solver solver = new Solver();
		solver.options().setSolver(SATFactory.ZChaff);
		try {
			final Formula show = model.show();
			final Solution sol = solver.solve(show, model.bounds(Integer.parseInt(args[0])));
			System.out.println(show);
			System.out.println(sol);
			
		} catch (TimeoutException e) {
			System.out.println("timed out.");
			e.printStackTrace();
		} catch (NumberFormatException nfe) {
			System.out.println("Usage: java tests.Closure [# A]");
		}
		
	}

}
