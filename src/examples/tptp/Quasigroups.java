/**
 * 
 */
package examples.tptp;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.Relation;
import kodkod.ast.Variable;
import kodkod.engine.Solution;
import kodkod.engine.Solver;
import kodkod.engine.TimeoutException;
import kodkod.engine.satlab.SATFactory;
import kodkod.instance.Bounds;
import kodkod.instance.Instance;
import kodkod.instance.Tuple;
import kodkod.instance.TupleFactory;
import kodkod.instance.TupleSet;
import kodkod.instance.Universe;

/**
 * A KK encoding of ALG195+1.p from http://www.cs.miami.edu/~tptp/
 * @author Emina Torlak
 */
public final class Quasigroups {
	private final Relation[] e1, e2, h;
	private final Relation op1, op2, s1, s2;
	/**
	 * Constructs a new instance of Quasigroups.
	 */
	public Quasigroups() {
		op1 = Relation.ternary("op1");
		op2 = Relation.ternary("op2");
		s1 = Relation.unary("s1");
		s2 = Relation.unary("s2");
		e1 = new Relation[7];
		e2 = new Relation[7];
		h = new Relation[7];
		for(int i = 0; i < 7; i++) {
			e1[i] = Relation.unary("e1"+i);
			e2[i] = Relation.unary("e2"+i);
			h[i] = Relation.binary("h"+(i+1));
		}
	}
	
	private static Formula totalFunction(Relation s, Relation op) {
		final Variable x = Variable.unary("x"), y = Variable.unary("y");
		return y.join(x.join(op)).one().forAll(x.oneOf(s).and(y.oneOf(s)));
	}
	
	/**
	 * Returns the relation constraints.
	 * @returns the relation constraints.
	 */
	public final Formula decls() {
		return totalFunction(s1, op1).and(totalFunction(s2, op2));
	}
	
	/**
	 * States that op is a latin square over s.
	 * @requires s is unary, op is ternary
	 */
	private static Formula opCoversRange(Relation s, Relation op) {
		final Variable e = Variable.unary("e");
		final Formula row = s.in(s.join(e.join(op)));
		final Formula col = s.in(e.join(s.join(op)));
		return (row.and(col)).forAll(e.oneOf(s));
	}
	
	/**
	 * Returns axioms 2 and 7.
	 * @return ax2 and ax7
	 */
	public final Formula ax2ax7() {
		return opCoversRange(s1, op1);
	}
	
	/**
	 * Parametrization of axioms 3 and 6.
	 * @requires s is unary, op is ternary
	 */
	private static Formula ax3and6(Relation s, Relation op) {
		final Variable x = Variable.unary("x"), y = Variable.unary("y");
		final Expression expr0 = x.join(y.join(op)); // op(y,x)
		final Expression expr1 = y.join(expr0.join(op)); // op(op(y,x),y)
		final Expression expr2 = y.join(expr1.join(op)); // op(op(op(y,x),y),y)
		return expr2.in(x).forAll(x.oneOf(s).and(y.oneOf(s)));
	}
	
	/**
	 * Returns axiom 3.
	 * @return ax3
	 */
	public final Formula ax3() {
		return ax3and6(s1,op1);
	}
	
	/**
	 * Returns axioms 5 and 8.
	 * @return ax5 and ax8
	 */
	public final Formula ax5ax8() {
		return opCoversRange(s2, op2);
	}
	
	/**
	 * Returns axiom 6.
	 * @return ax6
	 */
	public final Formula ax6() {
		return ax3and6(s2,op2);
	}
	
	/**
	 * Parametrization of axioms 14 and 15.
	 * @requires e's are unary, op is ternary
	 */
	private static Formula ax14and15(Relation[] e, Relation op) {
		final Expression expr0 = e[5].join(op); // op(e5,...)
		final Expression expr1 = e[5].join(expr0); // op(e5,e5)
		final Expression expr2 = expr1.join(expr0); // op(e5,op(e5,e5))
		final Expression expr3 = expr2.join(expr2.join(op)); // op(op(e5,op(e5,e5)),op(e5,op(e5,e5)))
		final Expression expr3a = expr3.join(op); // op(op(op(e5,op(e5,e5)),op(e5,op(e5,e5))),...)
		final Expression expr4 = e[5].join(expr3a); // op(op(op(e5,op(e5,e5)),op(e5,op(e5,e5))),e5)
		// e0 = op(op(op(e5,op(e5,e5)),op(e5,op(e5,e5))),op(e5,op(e5,e5)))
		final Formula f0 = e[0].in(expr2.join(expr3a));
		// e2 = op(op(e5,op(e5,e5)),op(e5,op(e5,e5)))
		final Formula f2 = e[2].in(expr3);
		// e3 = op(op(op(e5,op(e5,e5)),op(e5,op(e5,e5))),e5)
		final Formula f3 = e[3].in(expr4);
		// e4 = op(e5,op(e5,e5))
		final Formula f4 = e[4].in(expr2);
		// e6 = op(op(op(op(e5,op(e5,e5)),op(e5,op(e5,e5))),e5),op(e5,op(e5,e5)))
		final Formula f6 = e[6].in(expr2.join(expr4.join(op)));
		return f0.and(f2).and(f3).and(f4).and(f6);
	}
	
	/**
	 * Returns lines 1 and 3-6 of axiom 14.
	 * @return ax14
	 */
	public final Formula ax14() {
		return ax14and15(e1, op1);
	}
	
	/**
	 * Returns lines 1 and 3-6 of axiom 15.
	 * @return ax15
	 */
	public final Formula ax15() {
		return ax14and15(e2, op2);
	}
	
	/**
	 * Parametrization of axioms 16-22.
	 * @requires e is unary, h is binary
	 */
	private Formula ax16_22(Relation e, Relation h) {
		final Expression expr0 = e.join(op2); // op2(e,...)
		final Expression expr1 = e.join(expr0); // op2(e,e)
		final Expression expr2 = expr1.join(expr0); // op2(e,op2(e,e))
		final Expression expr3 = expr2.join(expr2.join(op2)); // op2(op2(e,op2(e,e)),op2(e,op2(e,e)))
		final Expression expr3a = expr3.join(op2); // op2(op2(op2(e,op2(e,e)),op2(e,op2(e,e))),...)
		final Expression expr4 = e.join(expr3a); // op2(op2(op2(e,op2(e,e)),op2(e,op2(e,e))),e)
		// h(e10) = op2(op2(op2(e,op2(e,e)),op2(e,op2(e,e))),op2(e,op2(e,e)))
		final Formula f0 = e1[0].join(h).eq(expr2.join(expr3a));
		// h(e11) = op2(e,e)
		final Formula f1 = e1[1].join(h).eq(expr1);
		// h(e12) = op2(op2(e,op2(e,e)),op2(e,op2(e,e)))
		final Formula f2 = e1[2].join(h).eq(expr3);
		// h(e13) = op2(op2(op2(e,op2(e,e)),op2(e,op2(e,e))),e)
		final Formula f3 = e1[3].join(h).eq(expr4);
		// h(e14) = op2(e,op2(e,e))
		final Formula f4 = e1[4].join(h).eq(expr2);
		// h(e16) = op2(op2(op2(op2(e,op2(e,e)),op2(e,op2(e,e))),e),op2(e,op2(e,e)))
		final Formula f6 = e1[6].join(h).eq(expr2.join(expr4.join(op2)));
		
		return f0.and(f1).and(f2).and(f3).and(f4).and(f6);
	}
	
	/**
	 * Returns lines 2-7 of axioms 16-22.
	 * @return lines 2-7 of axioms 16-22.
	 */
	public final Formula ax16_22() {
		Formula f = Formula.TRUE;
		for(int i = 0; i < 7; i++) {
			f = f.and(ax16_22(e2[i], h[i]));
		}
		return f;
	}
	
	/**
	 * Returns the part of the conjecture 1 that applies to the given h.
	 * @return  the part of the conjecture 1 that applies to the given h.
	 */
	private final Formula co1h(Relation h) {
		final Variable x = Variable.unary("x"), y = Variable.unary("y");
		final Expression expr0 = (y.join(x.join(op1))).join(h); // h(op1(x,y))
		final Expression expr1 = (y.join(h)).join((x.join(h)).join(op2)); // op2(h(x),h(y))
		final Formula f0 = expr0.eq(expr1).forAll(x.oneOf(s1).and(y.oneOf(s1)));
		final Formula f1 = s2.in(s1.join(h));
		// (all x,y: s1 | h(op1(x,y)) = op2(h(x),h(y))) && (s1 in h(s1))
		return f0.and(f1);
	}
	
	/**
	 * Returns conjecture 1.
	 * @return co1
	 */
	public final Formula co1() {
		
		Formula f = Formula.FALSE;
		for(int i = 0; i < 7; i++) {	
			f = f.or(co1h(h[i]));
		}
		
		return f;
	}
	
	/**
	 * Returns the conjunction of all axioms and implicit constraints (decls()).
	 * @return the conjunction of all axioms and implicit constraints
	 */
	public final Formula axioms() {
		return decls().and(ax2ax7()).and(ax3()).and(ax5ax8()).and(ax6()).and(ax14()).and(ax15()).and(ax16_22());
	}
	
	/**
	 * Returns the bounds the problem (axioms 1, 4, 9-13, second formula of 14-15, and first formula of 16-22).
	 * @return the bounds for the problem
	 */
	public final Bounds bounds() {
		final List<String> atoms = new ArrayList<String>(14);
		for(int i = 0; i < 7; i++)
			atoms.add("e1"+i);
		for(int i = 0; i < 7; i++)
			atoms.add("e2"+i);
		
		final Universe u = new Universe(atoms);
		final Bounds b = new Bounds(u);
		final TupleFactory f = u.factory();
		
		b.boundExactly(s1, f.range(f.tuple("e10"), f.tuple("e16")));
		b.boundExactly(s2, f.range(f.tuple("e20"), f.tuple("e26")));
		
		// axioms 9, 10, 11
		for(int i = 0; i < 7; i++) {
			b.boundExactly(e1[i], f.setOf("e1"+i));
			b.boundExactly(e2[i], f.setOf("e2"+i));
		}
		
		// axom 1
		final TupleSet op1h = f.area(f.tuple("e10", "e10", "e10"), f.tuple("e16", "e16", "e16"));
		// axiom 4
		final TupleSet op2h = f.area(f.tuple("e20", "e20", "e20"), f.tuple("e26", "e26", "e26"));
		
		for(int i = 0; i < 7; i++) {
			op1h.remove(f.tuple("e1"+i, "e1"+i, "e1"+i)); // axiom 12
			op2h.remove(f.tuple("e2"+i, "e2"+i, "e2"+i)); // axiom 13
		}
		
		final TupleSet op1l = f.setOf(f.tuple("e15", "e15", "e11")); // axiom 14, line 2
		final TupleSet op2l = f.setOf(f.tuple("e25", "e25", "e21")); // axiom 15, line 2
		
		op1h.removeAll(f.area(f.tuple("e15", "e15", "e10"), f.tuple("e15", "e15", "e16")));
		op1h.addAll(op1l);
		
		op2h.removeAll(f.area(f.tuple("e25", "e25", "e20"), f.tuple("e25", "e25", "e26")));
		op2h.addAll(op2l);
		
		b.bound(op1, op1l, op1h);
		b.bound(op2, op2l, op2h);
		
		final TupleSet high = f.area(f.tuple("e10", "e20"), f.tuple("e14", "e26"));
		high.addAll(f.area(f.tuple("e16", "e20"), f.tuple("e16", "e26")));
		
		// first line of axioms 16-22
		for(int i = 0; i < 7; i++) {
			Tuple t = f.tuple("e15", "e2"+i);
			high.add(t);
			b.bound(h[i], f.setOf(t), high);
			high.remove(t);
		}
		
		return b;
	}

	private static void displayOp(Instance instance, Relation op) {
		System.out.println("\n"+op+":");
		final Iterator<Tuple> iter = instance.tuples(op).iterator();
		for(int i = 0; i < 7; i++) {
			for(int j = 0; j < 7; j++) {
				System.out.print(iter.next().atom(2));
				System.out.print("\t");
			}
			System.out.println();
		}
	}
	
	private static void displayh(Instance instance, Relation[] h) {
		for(int i = 0; i < 7; i++) {
			System.out.println("\n"+h[i]+":");
			System.out.println(instance.tuples(h[i]));
		}
	}
	
	private static void usage() {
		System.out.println("java examples.Quasigroups");
		System.exit(1);
	}
	
	/**
	 * Usage: java examples.Quasigroups
	 */
	public static void main(String[] args) {
	
		try {
	
			final Quasigroups model = new Quasigroups();
			final Solver solver = new Solver();
			solver.options().setSolver(SATFactory.MiniSat);
			final Formula f = model.axioms().and(model.co1().not());
			final Bounds b = model.bounds();
//			System.out.println(f);
			final Solution sol = solver.solve(f, b);
			if (sol.instance()==null) {
				System.out.println(sol);
			} else {
				System.out.println(sol.stats());
				displayOp(sol.instance(), model.op1);
				displayOp(sol.instance(), model.op2);
				displayh(sol.instance(), model.h);
			}
		} catch (TimeoutException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (NumberFormatException nfe) {
			usage();
		}
	}
}
