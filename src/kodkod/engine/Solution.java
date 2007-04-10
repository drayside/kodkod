/* 
 * Kodkod -- Copyright (c) 2005-2007, Emina Torlak
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package kodkod.engine;


import kodkod.ast.Formula;
import kodkod.engine.config.Options;
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
	 * {@link #reduction()} <i>provided that the {@link Options options} 
	 * with which the {@link Solver solver}
	 * was invoked specified tracking of variables (see {@link Options#setLogTranslation(boolean)} and
	 * {@link Options#logTranslation()}).</i>.
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
	 * subformula, if the value returned  by {@link #outcome() this.outcome()} 
	 * is either TRIVIALLY_SATISFIABLE or TRIVIALLY_UNSATISFIABLE.  Otherwise
	 * returns null.  Note that this method does not guarantee to return the
	 * <b>smallest</b> subtree of this.formula that makes it trivial; it may
	 * return the formula itself.
	 * @return a reduction of this.formula to its trivially (un)satisfiable
	 * subformula,if the value returned  by {@link #outcome() this.outcome()} 
	 * is either TRIVIALLY_SATISFIABLE or TRIVIALLY_UNSATISFIABLE.
	 */
	public Formula reduction() {
		return reduction;
	}

	/**
	 * Returns a proof of this.formula's unsatisfiability, if the value 
	 * returned  by {@link #outcome() this.outcome()} is UNSATISFIABLE
	 * and the {@link Options options} with which the {@link Solver solver}
	 * was invoked specified the use of a core extracting 
	 * {@link kodkod.engine.satlab.SATProver sat solver}.
	 * Otherwise, null is returned.  
	 * @return a proof of this.formula's unsatisfiability, if 
	 * one is available.
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
	 * Returns a string representation of this Solution.
	 * @return a string representation of this Solution.
	 */
	public String toString() {
		final StringBuilder b = new StringBuilder();
		b.append("---OUTCOME---\n");
		b.append(outcome);
		b.append("\n");
		if (instance!=null) {
			b.append("\n---INSTANCE---\n");
			b.append(instance);
			b.append("\n");
		}
		if (reduction!=null) {
			b.append("\n---REDUCTION---\n");
			b.append(reduction);
			b.append("\n");
		}
		b.append("\n---STATS---\n");
		b.append(stats);
		b.append("\n");
		return b.toString();
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
