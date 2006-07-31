package examples.tptp;

import java.util.ArrayList;
import java.util.List;

import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.Variable;
import kodkod.engine.Solution;
import kodkod.engine.Solver;
import kodkod.engine.TimeoutException;
import kodkod.engine.satlab.SATFactory;
import kodkod.instance.Bounds;
import kodkod.instance.Universe;

/**
 * A KK encoding of ALG197+1.p from http://www.cs.miami.edu/~tptp/
 * 
 * @author Emina Torlak
 */
public final class SET010_3 {

	public SET010_3() {
		super();
		// TODO Auto-generated constructor stub
	}
	
	/**
	 * Returns the conjecture:  all b: set univ, c: set univ, d: set univ | b - c&d = (b-c) + (b-d)
	 * @return the conjecture
	 */
	public final Formula conjecture() {
		final Variable B = Variable.unary("B");
		final Variable C = Variable.unary("C");
		final Variable D = Variable.unary("D");
		return B.difference(C.intersection(D)).eq((B.difference(C)).union(B.difference(D))).forAll(B.setOf(Expression.UNIV).and(C.setOf(Expression.UNIV).and(D.setOf(Expression.UNIV))));
	}

	/**
	 * Returns a bounds for a universe of the given size.
	 * @return bounds for a universe of the given size.
	 */
	public final Bounds bounds(int size) {
		assert size > 0;
		final List<String> atoms = new ArrayList<String>(size);
		for(int i = 0; i < size; i++)
			atoms.add("a"+i);
		return new Bounds(new Universe(atoms));
	}
	
	private static void usage() {
		System.out.println("java examples.tptp.SET010_3 [univ size]");
		System.exit(1);
	}
	
	/**
	 * Usage: java examples.tptp.SET010_3 [univ size]
	 */
	public static void main(String[] args) {
		if (args.length < 1)
			usage();
		
		try {
			final int n = Integer.parseInt(args[0]);
			if (n < 1)
				usage();
			final SET010_3 model = new SET010_3();
			final Solver solver = new Solver();
			solver.options().setSolver(SATFactory.MiniSat);
			solver.options().setSymmetryBreaking(n*n*n);
			final Formula f = model.conjecture().not();
			final Bounds b = model.bounds(n);
			System.out.println(f);
			final Solution sol = solver.solve(f, b);
			System.out.println(sol);
		} catch (TimeoutException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (NumberFormatException nfe) {
			usage();
		}
	}
	
}
