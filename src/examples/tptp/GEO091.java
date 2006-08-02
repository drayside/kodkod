package examples.tptp;

import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.Variable;
import kodkod.engine.Solution;
import kodkod.engine.Solver;
import kodkod.engine.TimeoutException;
import kodkod.engine.satlab.SATFactory;
import kodkod.instance.Bounds;

/**
 * The  GEO091+1 problem from http://www.cs.miami.edu/~tptp/
 * 
 * @author Emina Torlak
 */
public class GEO091 extends GEO158 {

	/**
	 * Constructs a new instance of GEO091.
	 */
	public GEO091() {
		super();
		// TODO Auto-generated constructor stub
	}
	
	/**
	 * Returns the conjecture theorem_2_13.
	 * @return theorem_2_13
	 */
	public final Formula theorem213() {
		// all c1, c2: curve | 
		//  some c1.partOf & c2.partOf & open && !(lone endPoint.c1 & endPoint.c2) => c1 = c2
		final Variable c1 = Variable.unary("C1");
		final Variable c2 = Variable.unary("C2");
		final Expression e0 = c1.join(partOf).intersection(c2.join(partOf)).intersection(open);
		final Expression e1 = endPoint.join(c1).intersection(endPoint.join(c2));
		final Formula f0 = e0.some();
		final Formula f1 = e1.lone().not();
		return f0.and(f1).implies(c1.eq(c2)).forAll(c1.oneOf(curve).and(c2.oneOf(curve)));
	}

	private static void usage() {
		System.out.println("java examples.tptp.GEO191 [# curves] [# points]");
		System.exit(1);
	}
	
	/**
	 * Usage: ava examples.tptp.GEO191 [# curves] [# points]
	 */
	public static void main(String[] args) {
		if (args.length < 2)
			usage();
		
		try {
			final int c = Integer.parseInt(args[0]);
			final int p = Integer.parseInt(args[1]);
	
			final Solver solver = new Solver();
			solver.options().setSolver(SATFactory.MiniSat);
	
			final GEO091 model = new GEO091();
			final Formula f = model.axioms().and(model.theorem213().not());
			
			System.out.println(model.theorem213());
			
			final Bounds b = model.bounds(c,p);
			final Solution sol = solver.solve(f,b);
			
			System.out.println(sol);
			//System.out.println((new Evaluator(sol.instance())).evaluate(model.axioms().and(model.theorem213().not())));
		} catch (TimeoutException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (NumberFormatException nfe) {
			usage();
		}
	}
}
