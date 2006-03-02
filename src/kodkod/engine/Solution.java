package kodkod.engine;


import kodkod.ast.Formula;
import kodkod.instance.Instance;

/**
 * Represents the full solution to a formula:  an
 * instance if the formula is satisfiable or a
 * proof of unsatisfiability if not.
 * 
 * @specfield formula: Formula // the formula being solved
 * @specfield bounds: Bounds // the bounds on the formula
 * @author Emina Torlak
 */
public final class Solution {
	private final Outcome outcome;
	private final Statistics stats;
	private final Instance instance;
	private final Formula reduction;
	private final Proof proof;
	
	/**
	 * Constructs a Solution from the given values.
	 * @requires outcome != null && stats != null
	 * @requires outcome = SATISFIABLE => instance != null,
	 *           outcome = UNSATISFIABLE => proof != null,
	 *           outcome = TRIVIALLY_SATISFIABLE => instance != null && reduction != null,
	 *           outcome = TRIVIALLY_UNSATISFIABLE => reduction != null
	 */
	Solution(Outcome outcome, Statistics stats, Instance instance, Formula reduction, Proof proof) {
		assert outcome != null && stats != null;
		this.outcome = outcome;
		this.stats = stats;
		this.instance = instance;
		this.reduction = reduction;
		this.proof = proof;
	}
	
	/**
	 * Returns the outcome of the attempt to find
	 * a model for this.formula.  If the outcome is 
	 * SATISFIABLE or TRIVIALLY_SATISFIABLE, a satisfying 
	 * instance can be obtained by calling {@link #instance()}.
	 * If the formula is UNSATISFIABLE, a proof of unsatisfiability
	 * can be obtained by calling {@link #proof()} <i>provided that
	 * the {@link Options options} with which the {@link Solver solver}
	 * was invoked specified the use of a core extracting 
	 * {@link kodkod.engine.satlab.SATSolver sat solver}.</i>
	 * Lastly, if the returned Outcome is either TRIVIALLY_SATISFIABLE
	 * or TRIVIALLY_UNSATISFIABLE, a reduction of this.formula to its 
	 * trivially (un)satisfiable subtree can be obtained by calling
	 * {@link #reduction()}.
	 * @return an Outcome instance designating the 
	 * satisfiability of this.formula with respect to this.bounds
	 */
	public Outcome outcome() {
		return outcome;
	}
	
	/**
	 * Returns a satisfiying instance for this.formula, if the
	 * value returned by {@link #outcome() this.outcome()} is either
	 * SATISFIABLE or TRIVIALLY_SATISFIABLE.  Otherwise returns null.
	 * @return a satisfying instance for this.formula, if one exists.
	 */
	public Instance instance() {
		return instance;
	}

	/**
	 * Returns a reduction of this.formula to its trivially (un)satisfiable
	 * subtree, if the value returned  by {@link #outcome() this.outcome()} 
	 * is either TRIVIALLY_SATISFIABLE or TRIVIALLY_UNSATISFIABLE.  Otherwise
	 * returns null.
	 * @return a reduction of this.formula to its trivially (un)satisfiable
	 * subtree, if one exists.
	 */
	public Formula reduction() {
		return reduction;
	}

	/**
	 * Returns a proof of this.formula's unsatisfiability, if the value 
	 * returned  by {@link #outcome() this.outcome()} is UNSATISFIABLE.
	 * Otherwise, null is returned.  <i>If
	 * the {@link Options options} with which the {@link Solver solver}
	 * was invoked did not specify the use of a core extracting 
	 * {@link kodkod.engine.satlab.SATSolver sat solver}, calling the 
	 * {@link Proof#size()}, {@link Proof#clauses()}, {@link Proof#refine()}, 
	 * or {@link Proof#refine(int)}  
	 * methods on the returned object will raise an UnsupportedOperationException.</i>
	 * @return Returns a proof of this.formula's unsatisfiability, if 
	 * one exists.
	 */
	public Proof proof() {
		return proof;
	}
	
	/**
	 * Returns the statistics gathered while solving
	 * this.formula.
	 * @return the statistics gathered while solving
	 * this.formula.
	 */
	public Statistics stats() {
		return stats;
	}
	
	/**
	 * Enumerates the possible outcomes of an attempt
	 * to find a model for a FOL formula.
	 */
	public static enum Outcome {
		/** The formula is satisfiable with respect to the specified bounds. */
		SATISFIABLE,
		/** The formula is unsatisfiable with respect to the specified bounds. */
		UNSATISFIABLE,
		/** 
		 * The formula is trivially satisfiable with respect to the specified bounds: 
		 * a series of simple transformations reduces 
		 * the formula to the constant TRUE. 
		 **/
		TRIVIALLY_SATISFIABLE,
		/**
		 * The formula is trivially unsatisfiable with respect to the specified bounds:
		 * a series of simple transformations reduces 
		 * the formula to the constant FALSE.  
		 */
		TRIVIALLY_UNSATISFIABLE
	}
	
}
