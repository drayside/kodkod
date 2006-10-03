/**
 * 
 */
package examples.tptp;

import java.util.ArrayList;
import java.util.List;

import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.Relation;
import kodkod.ast.Variable;
import kodkod.engine.Evaluator;
import kodkod.engine.Solution;
import kodkod.engine.Solver;
import kodkod.engine.fol2sat.HigherOrderDeclException;
import kodkod.engine.fol2sat.UnboundLeafException;
import kodkod.engine.satlab.SATFactory;
import kodkod.instance.Bounds;
import kodkod.instance.TupleFactory;
import kodkod.instance.TupleSet;
import kodkod.instance.Universe;

/**
 * A KK encoding of GRA019+1.p from http://www.cs.miami.edu/~tptp/
 * @author Emina Torlak
 */
public final class GRA019 {
	private final Relation red, green, lessThan,goal,node;
	
	/**
	 * Constructs a new instance of GRA019.
	 */
	public GRA019() {
		node = Relation.unary("N");
		red = Relation.binary("red");
		green = Relation.binary("green");
		lessThan = Relation.binary("lessThan");
		goal = Relation.unary("goal");
	}
	
	private final Formula cliqueAxiom(Expression color) {
		final Variable a = Variable.unary("A"), b = Variable.unary("B"),
		 c = Variable.unary("C"), d = Variable.unary("D");
		final Expression sum = a.product(b.union(c).union(d)).union(b.product(c.union(d))).union(c.product(d));
		return (sum.in(color)).implies(goal.some()).forAll(a.oneOf(node).and(b.oneOf(node)).
				and(c.oneOf(node)).and(d.oneOf(node)));
	}
	
	/**
	 * Returns the red clique axiom.
	 * @return red clique axiom.
	 */
	public final Formula redCliqueAxiom() {
		return cliqueAxiom(red);
	}
	
	/**
	 * Returns the green clique axiom.
	 * @return green clique axiom.
	 */
	public final Formula greenCliqueAxiom() {
		return cliqueAxiom(green);
	}
	
	/**
	 * Returns the partition axiom.
	 * @return partition axiom
	 */
	public final Formula partition() {
		return lessThan.in(red.union(green));
	}
	
	public final Formula lessThanTransitive() { 
		return lessThan.join(lessThan).in(lessThan);
	}
	
	/**
	 * Returns the conjunction of all axioms.
	 * @return conjunction of all axioms
	 */
	public final Formula axioms() {
		return redCliqueAxiom().and(greenCliqueAxiom()).and(partition()).
		and(lessThanTransitive());
	}
	
	/**
	 * Returns the goal_to_be_proved conjecture.
	 * @return goal_to_be_proved conjecture.
	 */
	public final Formula goalToBeProved() { 
		return goal.some();
	}
	
	/**
	 * Returns the bounds for the given scope.
	 * @return the bounds for the given scope.
	 */
	public final Bounds bounds(int n) {
		assert n > 0;
		final List<String> atoms = new ArrayList<String>(n);
		for(int i = 1; i <= n; i++)
			atoms.add("n"+i);
		atoms.add("goal");
		final Universe u = new Universe(atoms);
		final TupleFactory f = u.factory();
		final Bounds b = new Bounds(u);
		b.bound(goal, f.setOf("goal"));
		final TupleSet ns = f.range(f.tuple("n1"), f.tuple("n"+n));
		b.boundExactly(node, ns);
		b.bound(red, ns.product(ns));
		b.bound(green, ns.product(ns));
		final TupleSet s = f.noneOf(2);
		for(int i = 1; i <= n; i++) {
			for(int j = i+1; j <= n; j++)
				s.add(f.tuple("n"+i, "n"+j));
		}
		b.bound(lessThan, s, ns.product(ns));
		return b;
	}
	
	/**
	 * Usage: java examples.tptp.GRA019
	 */
	public  static void main(String[] args) {
		try {

			final GRA019 model = new GRA019();
			
			final Bounds b = model.bounds(18);
			final Solver solver = new Solver();
			solver.options().setSolver(SATFactory.MiniSat);
			
			final Formula f = model.axioms().and(model.goalToBeProved().not());//.and(model.red.intersection(model.green).no());
			System.out.println(f);
			System.out.println(b);
			final Solution s = solver.solve(f, b);
			System.out.println(s);
			System.out.println((new Evaluator(s.instance())).evaluate(f));
	
		} catch (HigherOrderDeclException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (UnboundLeafException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} 
	}
}
