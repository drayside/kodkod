/* 
 * Kodkod -- Copyright (c) 2005-2012, Emina Torlak
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
import kodkod.engine.fol2sat.HigherOrderDeclException;
import kodkod.engine.fol2sat.SymmetryDetector;
import kodkod.engine.fol2sat.Translation;
import kodkod.engine.fol2sat.Translator;
import kodkod.engine.fol2sat.UnboundLeafException;
import kodkod.engine.satlab.SATAbortedException;
import kodkod.engine.satlab.SATFactory;
import kodkod.engine.satlab.SATSolver;
import kodkod.instance.Bounds;
import kodkod.instance.Universe;

import java.util.Stack;

/** 
 * Incremental solver with the ability to create checkpoints.
 * Checkpoints save the state of the solver on a stack so that we can
 * backtrack to an earlier state. This saves us from having to solve the same
 * problem multiple times.
 *
 * @specfield options: {@link Options} 
 * @specfield bounds: lone {@link Bounds}
 * @specfield formulas: set {@link Formula}
 * @invariant formulas.*components & Relation in bounds.relations
 * @invariant some formulas iff some bounds 
 * @invariant options.solver() && options.logTranslation = 0   
 * 
 * @see IncrementalSolver
 * @see SymmetryDetector
 * @see kodkod.engine.fol2sat.Translation.Checkpointed
 * @see Translator#translateCheckpointed(Formula, Bounds, Options)
 * @see Translator#translateCheckpointed(Formula, Bounds, kodkod.engine.fol2sat.Translation.Checkpointed)
 * 
 * @author Chris Kleynhans
 */
public final class CheckpointedSolver implements KodkodSolver {

	private final Options options;
	private Translation.Checkpointed translation;
	private Boolean outcome;

	private Stack<Boolean> outcomeCheckpoints;	
	private Stack<Translation.Checkpointed> translationCheckpoints;

	/**
	 * Initializes the solver with the given options.
	 * @ensures no this.solution' && no this.formulas' && 
	 *          no this.bounds'&& this.options' = options
	 */
	private CheckpointedSolver(Options options) { 
		this.options = options;
		this.outcome = null;
		this.outcomeCheckpoints = new Stack<Boolean>();
		this.translationCheckpoints = new Stack<Translation.Checkpointed>();
	}
	
	/**
	 * Returns a new {@link CheckpointedSolver} using the given options.   
	 * @requires options.solver.checkpointable() && options.logTranslation = 0   
	 * @return some s: CheckpointedSolver | no s.formulas  && no s.bounds  && s.options = options.clone()
	 * @throws NullPointerException  any of the arguments are null
	 * @throws IllegalArgumentException any of the preconditions on options are violated
	 */
	public static CheckpointedSolver solver(Options options) {
		Translator.checkCheckpointedOptions(options);
		return new CheckpointedSolver(options.clone());
	}
	
	/**
	 * Adds the specified formula and bounds to the solver's state, modifies 
	 * the current solution to reflect the updated state (if needed), 
	 * and returns this solver.  This solver should not be used again if a 
	 * call to this method results in an exception.
	 * @requires this.{@link #usable() usable}()
	 * @requires f.*components & Relation in (this.bounds + b).relations
	 * @requires some this.bounds => this.bounds.universe = b.universe && no b.intBound && no (this.bounds.relations & b.relations)
	 * @requires some this.bounds => 
	 *            all s: {@link SymmetryDetector#partition(Bounds) partition}(this.bounds) |  
	 * 				some p: {@link SymmetryDetector#partition(Bounds) partition}(b) | 
	 * 				   s.elements in p.elements
	 * @ensures this.formulas' = this.formulas + f
	 * @ensures some this.bounds =>
	 *            (this.bounds.relations' = this.bounds.relations + b.relations &&
	 *             this.bounds.upperBound' = this.bounds.upperBound + b.upperBound &&
	 *             this.bounds.lowerBound' = this.bounds.lowerBound + b.lowerBound) else
	 *            (this.bounds' = bounds)
	 * @return some sol: Solution | sol.instance() = null => 
	 *              UNSAT(this.formulas', this.bounds', this.options) else 
	 *              sol.instance() in MODELS(Formula.and(this.formulas'), this.bounds', this.options)
	 * @throws IllegalStateException a prior call returned an UNSAT solution or resulted in an exception
	 * @throws NullPointerException  any of the arguments are null
	 * @throws UnboundLeafException  the formula refers to an undeclared variable or a relation not mapped by this.bounds + b
	 * @throws HigherOrderDeclException  the formula contains a higher order declaration
	 * @throws IllegalArgumentException any of the remaining preconditions on {@code f} and {@code b} are violated
	 * @throws AbortedException this solving task has been aborted
	 */
	public Solution solve(Formula f, Bounds b) throws HigherOrderDeclException, UnboundLeafException, AbortedException {
		if (outcome==Boolean.FALSE)
			throw new IllegalStateException("Cannot use this solver since a prior call to solve(...) produced an UNSAT solution.");

		if (outcome != null && translation==null) 
			throw new IllegalStateException("Cannot use this solver since a prior call to solve(...) resulted in an exception.");
		
		final Solution solution;
		try {			
			final long startTransl = System.currentTimeMillis();
			translation = translation==null ? Translator.translateCheckpointed(f, b, options) : Translator.translateCheckpointed(f, b, translation);
			final long endTransl = System.currentTimeMillis();

			if (translation.trivial()) {
				final Statistics stats = new Statistics(translation, endTransl - startTransl, 0);
				if (translation.cnf().solve()) {
					solution = Solution.triviallySatisfiable(stats, translation.interpret());
				} else {
					solution = Solution.triviallyUnsatisfiable(stats, null);
				}	
			} else {
				final SATSolver cnf = translation.cnf();
				
				translation.options().reporter().solvingCNF(translation.numPrimaryVariables(), cnf.numberOfVariables(), cnf.numberOfClauses());
				final long startSolve = System.currentTimeMillis();
				final boolean sat = cnf.solve();
				final long endSolve = System.currentTimeMillis();

				final Statistics stats = new Statistics(translation, endTransl - startTransl, endSolve - startSolve);
				if (sat) {
					solution = Solution.satisfiable(stats, translation.interpret());
				} else { 
					solution = Solution.unsatisfiable(stats, null);
				}
			}
		} catch (SATAbortedException sae) {
			free();
			throw new AbortedException(sae);		
		} catch (RuntimeException e) {
			free();
			throw e;
		}
		
		if (solution.sat()) {
			outcome = Boolean.TRUE;
		} else {
			outcome = Boolean.FALSE;
		}

		return solution;
	}

	/**
	 * Returns true iff this solver has neither returned an UNSAT solution so far
	 * nor thrown an exception during solving.
	 * @return  true iff this solver has neither returned an UNSAT solution so far
	 * nor thrown an exception during solving
	 */
	public boolean usable() {
		return (outcome == Boolean.TRUE && translation != null) ||  (outcome == null);
	}
	
	/**
	 * Returns a copy of {@code this.options}.
	 * @return this.options.clone()
	 */
	public Options options() { return options.clone(); }
	
	/**
	 * Releases the resources, if any, associated with this solver.
	 */
	public void free() {
		// TODO: We can probably free all of the checkpoints as well.
		if (translation != null) {
			translation.cnf().free();
			translation = null;
		}
	}
	
	/**
	 * Pushes the current solver state onto the checkpoint stack.
	 */	
	public void checkpoint() {
		// If a translation exists then we need to checkpoint it.
		// The translation itself will be resposible for tracking its
		// own state.
		if (translation != null) {
			translation.checkpoint();
		}

		// We always push the translation onto the stack so that we know
		// whether a translation existed at that point in time.
		translationCheckpoints.push(translation);

		// Push the current outcome onto the stack.
		outcomeCheckpoints.push(outcome);
	}

	/**
	 * Pops the checkpoint stack and updates the solver state to match the
	 * checkpoint.
	 */
	public void rollback() {
		// Get the translation from the top of the stack.
		translation = translationCheckpoints.pop();

		// If the translation is not null, then the translation was
		// checkpointed, so we need to roll it back to the checkpoint.
		if (translation != null) {
			translation.rollback();
		}
		
		// Restore the checkpointed outcome.
		outcome = outcomeCheckpoints.pop();
	}
}
