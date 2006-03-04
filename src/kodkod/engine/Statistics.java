package kodkod.engine;

/**
 * Stores the statistics gathered while solving
 * a given formula.
 * @specfield formula: Formula // the formula being solved
 * @specfield bounds: Bounds // the bounds on the formula
 */
public final class Statistics {
	private final int vars, pVars, clauses;
	private final long translation, solving; 
	
	/**
	 * Constructs a new Statistics object out of the provided
	 * values.
	 */
	Statistics(int variables, int primaryVariables, int clauses, 
			   long translationTime, long solvingTime) {
		this.vars = variables;
		this.pVars = primaryVariables;
		this.clauses = clauses;
		this.translation = translationTime;
		this.solving = solvingTime;
	}
	
	/**
	 * Returns the number of variables needed 
	 * to encode this.formula in CNF.
	 * @return the number of variables needed
	 * to encode this.formula in CNF.
	 */
	public int variables() {
		return vars;
	}
	
	/**
	 * Returns the number of primary variables
	 * used in the encoding of this.formula; i.e. the variables
	 * allocated to all the relations at the leaves
	 * of this.formula.
	 * @return the number of primary variables
	 * used in the encoding of this.formula
	 */
	public int primaryVariables() {
		return pVars;
	}
	
	/**
	 * Returns the number of clauses needed to 
	 * encode this.formula in CNF.
	 * @return the number of variables needed
	 * to encode this.formula in CNF.
	 */
	public int clauses() {
		return clauses;
	}
	
	/**
	 * Returns the number of miliseconds spent
	 * on translation this.formula to CNF.
	 * @return the number of miliseconds spent
	 * on translation this.formula to CNF.
	 */
	public long translationTime() {
		return translation;
	}
	
	/**
	 * Returns the number of miliseconds spent
	 * on solving the CNF encoding of this.formula.
	 * @return the number of miliseconds spent
	 * on solving the CNF encoding of this.formula.
	 */
	public long solvingTime() {
		return solving;
	}
	
	/**
	 * Returns a string representation of this
	 * Statistics object.
	 * @return a string representation of this
	 * Statistics object.
	 */
	public String toString() {
		final StringBuilder ret = new StringBuilder();
		ret.append("p cnf ");
		ret.append(vars);
		ret.append(" ");
		ret.append(clauses);
		ret.append("\n");
		ret.append("primary variables: ");
		ret.append(pVars);
		ret.append("\n");
		ret.append("translation time: ");
		ret.append(translation);
		ret.append(" ms\n");
		ret.append("solving time: ");
		ret.append(solving);
		ret.append(" ms\n");
		return ret.toString();
	}
}