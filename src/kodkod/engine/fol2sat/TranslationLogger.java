/**
 * 
 */
package kodkod.engine.fol2sat;

import kodkod.ast.Formula;
import kodkod.ast.Node;
import kodkod.engine.bool.BooleanMatrix;
import kodkod.engine.bool.BooleanValue;

/**
 * Logs the translations of all descendants of a given formula that 
 * are either formulas or that desugar to formulas.  
 * @specfield formula: Formula
 * @specfield bounds: Bounds
 * @specfield transforms: formula.*children lone-> Formula 
 * @specfield records: (iden ++ transforms).Formula -> BooleanValue -> Variable -> BooleanMatrix
 * @invariant all f: transforms[Node] | freeVariables(f) in freeVariables(transforms.f)
 * @author Emina Torlak
 */
abstract class TranslationLogger {

	/**
	 * Records the translation of the given node to the given boolean value in the specified environment.
	 * @requires n in this.formula.*children & Formula || some this.transforms[n]
	 * @effects this.records' = this.records + 
	 *  n -> translation -> {v: freeVariables((iden++this.transforms)[n]), m: BooleanMatrix | m = env.lookup(v) }
	 * @throws IllegalArgumentException - n is not a descendent of this.formula that is a formula or that desugars 
	 * to a formula
	 * @throws IllegalStateException - this log has been closed
	 */
	abstract void log(Node n, BooleanValue translation, Environment<BooleanMatrix> env);
	
	/**
	 * Closes this logger and releases associated resources.  Attempts to call {@link #log(Formula, BooleanValue, Environment)}
	 * after the log has been closed will result in an IllegalStateException.
	 * @effects closes this logger and releases associated resources. 
	 */
	abstract void close();
	
	/**
	 * Returns a TranslationLog view of this.records.
	 * @return a TranslationLog view of this.records.
	 */
	abstract TranslationLog log();
}
