/**
 * 
 */
package kodkod.engine;

import kodkod.ast.Formula;
import kodkod.ast.Relation;
import kodkod.instance.Bounds;

/**
 * A cost function to be minimized during {@link kodkod.engine.Solver#solve(Formula, Bounds, Cost) solving}.
 * In particular, each Cost is a total function from a set of {@link kodkod.ast.Relation relations}  
 * to a non-negative integer which represents the weight of one edge in the relation's value.  (Hence, the 
 * total cost of a relation <i>r</i> in a given instance is <i>s</i> is #s.tuples(r) * edgeCost(r).)
 * @specfield relations: set Relation // the domain of this cost function
 * @specfield cost: relations -> one [0..)
 * @author Emina Torlak
 */
public interface Cost {

	/**
	 * Returns the cost of one edge in the relational value of the given
	 * {@link Relation Relation instance}.
	 * @return this.cost[relation]
	 * @throws IllegalArgumentException - relation !in this.relations
	 */
	public abstract int edgeCost(Relation relation);
}
