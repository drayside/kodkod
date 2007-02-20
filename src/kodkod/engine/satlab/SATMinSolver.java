/**
 * 
 */
package kodkod.engine.satlab;

/**
 * Provides an interface to a SAT solver that produces
 * minimal cost solutions.  That is, given a CNF formula
 * and a function f from the variables to non-negative integer,
 * the solver produces the solution that minimizes the expression
 * sum(f(v)*valueOf(v)) for all variables v, where valueOf(v) is 
 * 1 if the variable is set to TRUE and 0 otherwise. 
 * 
 * @specfield variables: set [1..)
 * @specfield cost: variables -> one [0..)
 * @specfield clauses: set Clause
 * @invariant all i: [2..) | i in variables => i-1 in variables
 * @invariant all c: clauses | all lit: c.literals | lit in variables || -lit in variables
 * @author Emina Torlak
 */
public interface SATMinSolver extends SATSolver {

	/**
	 * Sets the cost of the given variable to the specified value.
	 * @requires variable in this.variables && cost >= 0
	 * @effects this.cost' = this.cost ++ variable -> cost
	 * @throws IllegalArgumentException - variable !in this.variables || cost < 0
	 */
	public abstract void setCost(int variable, int cost);
	
	/**
	 * Returns the cost of setting the given variable to TRUE.
	 * @requires variable in this.variables
	 * @return this.cost[variable]
	 * @throws IllegalArgumentException - variable !in this.variables
	 */
	public abstract int costOf(int variable);
	
}
