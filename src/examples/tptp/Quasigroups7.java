package examples.tptp;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.Relation;
import kodkod.ast.Variable;
import kodkod.instance.Bounds;
import kodkod.instance.Instance;
import kodkod.instance.Tuple;
import kodkod.instance.TupleFactory;
import kodkod.instance.TupleSet;
import kodkod.instance.Universe;

/**
 * Contains the relations/axioms common to the problems ALG195+1.p and ALG197+1.p from
 * from http://www.cs.miami.edu/~tptp/
 * 
 * @author Emina Torlak
 */
public abstract class Quasigroups7 {
	final Relation[] e1, e2, h;
	final Relation op1, op2, s1, s2;
	
	/**
	 * Constructs a new instance of Quasigroups7.
	 */
	Quasigroups7() {
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
	 * Parametrization of axioms 12 and 13.
	 * @requires e's are unary, op is ternary
	 */
	abstract Formula ax12and13(Relation[] e, Relation op);
	
	/**
	 * Returns axiom 12.
	 * @return ax12
	 */
	public final Formula ax12() {
		return ax12and13(e1, op1);
	}

	/**
	 * Returns axiom 13.
	 * @return ax13
	 */
	public final Formula ax13() { 
		return ax12and13(e2, op2);
	}
	
	/**
	 * Parametrization of axioms 14 and 15.
	 * @requires e's are unary, op is ternary
	 */
	abstract Formula ax14and15(Relation[] e, Relation op);
	
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
	abstract Formula ax16_22(Relation e, Relation h);
	
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
	 * Returns the conjunction of all axioms and implicit constraints (decls()).
	 * @return the conjunction of all axioms and implicit constraints
	 */
	public final Formula axioms() {
		return decls().and(ax2ax7()).and(ax3()).and(ax5ax8()).and(ax6()).and(ax12()).and(ax13()).and(ax14()).and(ax15()).and(ax16_22());
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
	 * Returns the partial bounds the problem (axioms 1, 4, 9-11).
	 * @return the partial bounds for the problem
	 */
	public Bounds bounds() {
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
		
		b.bound(op1, op1h);
		b.bound(op2, op2h);
		
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
	
	/**
	 * Prints the values of the op1, op2, and h1-h7 relations
	 * to standard out.
	 */
	void display(Instance instance) {
		displayOp(instance, op1);
		displayOp(instance, op2);
		for(int i = 0; i < 7; i++) {
			System.out.println("\n"+h[i]+":");
			System.out.println(instance.tuples(h[i]));
		}
	}

}
