package examples.tptp;

import java.util.ArrayList;
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
import kodkod.instance.TupleFactory;
import kodkod.instance.TupleSet;
import kodkod.instance.Universe;

/**
 * The  GEO158+1 problem from http://www.cs.miami.edu/~tptp/
 * 
 * @author Emina Torlak
 */
public class GEO158 {
	final Relation partOf, incident, sum, endPoint, innerPoint, meet, closed, open;
	final Relation curve, point;
	/*
	 * part_of		:	C -> C
	 * incident_c	: 	P -> C
	 * sum			:	C -> C -> one C     	 
	 * end_point		:	P -> C
	 * inner_point	:	P -> C
	 * meet			: 	P -> C -> C
	 * closed		:	C
	 * open			:	C
	 */
	
	/**
	 * Constructs a new instance of GEO0040.
	 */
	GEO158() {
		super();
		partOf = Relation.binary("part_of");
		incident = Relation.binary("incident_c");
		sum  = Relation.ternary("sum");
		endPoint = Relation.binary("end_point");
		closed = Relation.unary("closed");
		open = Relation.unary("open");
		curve = Relation.unary("curve");
		point = Relation.unary("point");
		meet = Relation.ternary("meet");
		innerPoint  = Relation.binary("inner_point");
	}
	
	/**
	 * Returns all the 'type' declarations.
	 * @return the type declarations
	 */
	public Formula decls() {
		final Expression cc = curve.product(curve);
		final Expression pc = point.product(curve);
		final Formula f0 = partOf.in(cc);
		final Formula f1 = closed.in(curve).and(open.in(curve));
		final Formula f2 = meet.in(point.product(cc)).and(sum.in(curve.product(cc)));
		final Formula f3 = incident.in(pc).and(endPoint.in(pc)).and(innerPoint.in(pc));
		return f0.and(f1).and(f2).and(f3);
	}
	
	/**
	 * Returns the part_of_defn axiom.
	 * @return part_of_defn 
	 */
	public final Formula partOfDefn() {
		// all c : Curve | incident.(partOf.c) in incident.c
		final Variable c = Variable.unary("C");
		final Formula f = incident.join(partOf.join(c)).in(incident.join(c));
		return f.forAll(c.oneOf(curve));
	}
	
	/**
	 * Returns the sum_defn axiom.
	 * @return sum_defn 
	 */
	public final Formula sumDefn() {
		// all c1, c2: Curve | some c2.(c1.Sum) <=> 
		//	incident.(c2.(c1.Sum)) = incident.(c1 + c2)
		final Variable c1 = Variable.unary("C1");
		final Variable c2 = Variable.unary("C2");
		final Expression e = c2.join(c1.join(sum));
		final Formula f = incident.join(e).eq(incident.join(c1.union(c2)));
		return e.some().iff(f).forAll(c1.oneOf(curve).and(c2.oneOf(curve)));
	}
	
	/**
	 * Returns the end_point_defn axiom.
	 * @return end_point_defn 
	 */
	public final Formula endPointDefn() {
		// endPoint in incident &&
		//  all c: Curve, p: Point | 
		//    p in endPoint.c <=> let e = p.incident & partOf.c | e->e in partOf+~partOf
		final Variable c = Variable.unary("C");
		final Variable p = Variable.unary("P");
		
		final Formula f0 = endPoint.in(incident);
		final Expression e = p.join(incident).intersection(partOf.join(c));
		final Formula f1 = p.in(endPoint.join(c)).iff(e.product(e).in(partOf.union(partOf.transpose()))).
			forAll(p.oneOf(endPoint.join(c))).forAll(c.oneOf(curve));
		
		return f0.and(f1);
	}
	
	/**
	 * Returns the inner_point_defn axiom.
	 * @return inner_point_defn
	 */
	public final Formula innerPointDefn() {
		//  innerPoint = incident - endPoint
		return innerPoint.eq(incident.difference(endPoint));
	}
	
	/**
	 * Returns the meet_defn axiom.
	 * @return meet_defn 
	 */
	public final Formula meetDefn() { 
		// all c, c1: Curve, p: Point | 
		//    p->c->c1 in meet <=> 
		//     p in incident.c & incident.c1 &&
		//     incident.c & incident.c1 in endPoint.c &&
		//     incident.c & incident.c1 in endPoint.c1
		final Variable c = Variable.unary("C");
		final Variable c1 = Variable.unary("C1");
		final Variable p = Variable.unary("P");
		final Formula f0 = p.product(c).product(c1).in(meet);
		final Expression e = incident.join(c).intersection(incident.join(c1));
		final Formula f1 = p.in(e);
		final Formula f2 = e.in(endPoint.join(c));
		final Formula f3 = e.in(endPoint.join(c1));
		return f0.iff(f1.and(f2).and(f3)).forAll(c.oneOf(curve).and(c1.oneOf(curve)).and(p.oneOf(point)));
	}
	
	/**
	 * Returns the closed_defn axiom.
	 * @return closed_defn 
	 */
	public final Formula closedDefn() {
		// closed = curve - open
		return closed.eq(curve.difference(open));
	}
	
	/**
	 * Returns the open_defn axiom.
	 * @return open_defn 
	 */
	public final Formula openDefn() {
		// open = point.endPoint
		return open.eq(point.join(endPoint));
	}
	
	/**
	 * Returns the c1 axiom.
	 * @return c1 
	 */
	public final Formula c1() {
		// (partOf - iden).curve in open
		return (partOf.difference(Expression.IDEN)).join(curve).in(open);
	}
	
	/**
	 * Returns the c2 axiom.
	 * @return c2 
	 */
	public final Formula c2() {
		// all c1, c2, c3: curve | 
		//  some c1.partOf & c2.partOf & c3.partOf &&
		//  some endPoint.c1 & endPoint.c2 & endPoint.c2 =>
		//   c2->c3 in partOf || c3->c2 in partOf || c1->c2 in partOf ||
		//   c2->c1 in partOf || c1->c3 in partOf || c3->c1 in partOf
		final Variable c1 = Variable.unary("C1");
		final Variable c2 = Variable.unary("C2");
		final Variable c3 = Variable.unary("C3");
		
		final Formula f0 = c1.join(partOf).intersection(c2.join(partOf)).intersection(c3.join(partOf)).some();
		final Formula f1 = endPoint.join(c1).intersection(endPoint.join(c2)).intersection(endPoint.join(c3)).some();
		final Formula f2 = c2.product(c3).in(partOf).or(c3.product(c2).in(partOf));
		final Formula f3 = c1.product(c2).in(partOf).or(c2.product(c1).in(partOf));
		final Formula f4 = c1.product(c3).in(partOf).or(c3.product(c1).in(partOf));
		
		return f0.and(f1).implies(f2.or(f3).or(f4)).
			forAll(c1.oneOf(curve).and(c2.oneOf(curve)).and(c3.oneOf(curve)));
	}
	
	/**
	 * Returns the c3 axiom.
	 * @return c3 
	 */
	public final Formula c3() {
		// all c: curve | some innerPoint.c
		final Variable c = Variable.unary("C");
		return  innerPoint.join(c).some().forAll(c.oneOf(curve));
	}

	/**
	 * Returns the c4 axiom.
	 * @return c4 
	 */
	public final Formula c4() {
		// all c: curve, p: innerPoint.c |  
		//  some p->sum.c & meet
		final Variable c = Variable.unary("C");
		final Variable p = Variable.unary("P");
		return p.product(sum.join(c)).intersection(meet).some().forAll(p.oneOf(innerPoint.join(c))).forAll(c.oneOf(curve));
	}
	
	/**
	 * Returns axioms c5 and c6
	 * @return c5 and c6
	 */
	public final Formula c5c6() {
		// all c: open | ! (lone endPoint.c)
		final Variable c = Variable.unary("C");
		return endPoint.join(c).lone().not().forAll(c.oneOf(open));
	}
	
	/**
	 * Returns the c7 axiom.
	 * @return c7 
	 */
	public final Formula c7() {
		// all c: closed |
		//  some sum.c & point.meet => endPoint.((sum.c).curve)->sum.c in meet
		final Variable c = Variable.unary("C");
		final Expression s = sum.join(c);
		final Formula f0 = s.intersection(point.join(meet)).some();
		final Formula f1 = endPoint.join(s.join(curve)).product(s).in(meet);
		return f0.implies(f1).forAll(c.oneOf(closed));
	}
	
	/**
	 * Returns the c8 axiom.
	 * @return c8 
	 */
	public final Formula c8() {
		// point.meet in sum.curve
		return point.join(meet).in(sum.join(curve));
	}
	
	/**
	 * Returns the c9 axiom.
	 * @return c9 
	 */
	public final Formula c9() {
		// all c, c1: curve | incident.c = incident.c1 => c = c1
		final Variable c = Variable.unary("C");
		final Variable c1 = Variable.unary("C1");
		return incident.join(c).eq(incident.join(c1)).implies(c.eq(c1)).
			forAll(c.oneOf(curve).and(c1.oneOf(curve)));
	}
	
	/**
	 * Returns the conjunction of all axioms and decls
	 * @returns the conjunction of all axioms and decls
	 */
	public Formula axioms() {
		return decls().and(partOfDefn()).and(sumDefn()).and(endPointDefn()).and(innerPointDefn()).
		 	and(meetDefn()).and(openDefn()).and(closedDefn()).
		 	and(c1()).and(c2()).and(c3()).and(c4()).and(c5c6()).and(c7()).and(c8()).and(c9());
	}
	
	
	/**
	 * Returns a bounds with the given number of maximum curves and points
	 * @return a bounds with the given number of maximum curves and points
	 */
	public Bounds bounds(int curves, int points) {
		assert curves > 0 && points > 0;
		List<String> atoms = new ArrayList<String>(curves + points);
		for(int i = 0; i < curves; i++) 
			atoms.add("c"+i);
		for(int i = 0; i < points; i++) 
			atoms.add("p"+i);
		final Universe u = new Universe(atoms);
		final TupleFactory f = u.factory();
		final Bounds b = new Bounds(u);
		final TupleSet c = f.range(f.tuple("c0"), f.tuple("c"+(curves-1)));
		final TupleSet p = f.range(f.tuple("p0"), f.tuple("p"+(points-1)));
		final TupleSet cc = c.product(c), pc = p.product(c);
		b.bound(curve, c);
		b.bound(point, p);
		b.bound(partOf, cc);
		b.bound(incident, pc);
		b.bound(sum, c.product(cc));
		b.bound(endPoint, pc);
		b.bound(innerPoint, pc);
		b.bound(meet, pc.product(c));
		b.bound(closed, c);
		b.bound(open, c);
		return b;
	}
	
	private static void usage() {
		System.out.println("java examples.tptp.GEO158 [# curves] [# points]");
		System.exit(1);
	}
	
	/**
	 * Usage: ava examples.tptp.GEO158 [# curves] [# points]
	 */
	public static void main(String[] args) {
		if (args.length < 2)
			usage();
		
		try {
			final int c = Integer.parseInt(args[0]);
			final int p = Integer.parseInt(args[1]);
	
			final Solver solver = new Solver();
			solver.options().setSolver(SATFactory.MiniSat);
	
			final GEO158 model = new GEO158();
			final Formula f = model.axioms().and(model.closed.some().and(model.open.some()));
			
			
//			System.out.println(model.decls());
//			System.out.println(model.partOfDefn());
//			System.out.println(model.sumDefn());
//			
//			System.out.println(model.endPointDefn());
//			System.out.println(model.innerPointDefn());
//			System.out.println(model.meetDefn());
//			
//			System.out.println(model.openDefn());
//			System.out.println(model.closedDefn());			
//			System.out.println(model.c1());
//			
//			System.out.println(model.c2());
//			System.out.println(model.c3());
//			System.out.println(model.c4());
//			
//			System.out.println(model.c5c6());
//			System.out.println(model.c7());
//			System.out.println(model.c8());
//			
//			System.out.println(model.c9());

			
			
			final Bounds b = model.bounds(c,p);
			final Solution sol = solver.solve(f,b);
			System.out.println(sol);
		} catch (TimeoutException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (NumberFormatException nfe) {
			usage();
		}
	}

}
