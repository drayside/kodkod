/**
 * 
 */
package kodkod.engine.fol2sat;

import kodkod.ast.Formula;
import kodkod.engine.bool.BooleanMatrix;
import kodkod.engine.bool.BooleanValue;

/**
 * Logs the translations of all descendants of a given formula that 
 * are either formulas or that desugar to formulas.  
 * @specfield formula: Formula
 * @specfield bounds: Bounds
 * @specfield transforms: formula.*children lone-> Formula 
 * @specfield records: (iden ++ transforms).Formula -> BooleanValue -> Variable -> BooleanMatrix
 * @author Emina Torlak
 */
abstract class TranslationLogger {

	/**
	 * Records the translation of the source of the 
	 * given transformed formula to the given boolean value 
	 * in the specified environment.
	 * @requires some this.transforms.f
	 * @effects this.records' = this.records + 
	 *  this.transforms.f -> translation -> {v: freeVariables(f), m: BooleanMatrix | m = env.lookup(v) }
	 * @throws IllegalArgumentException - no this.transforms.f
	 * @throws IllegalStateException - this log has been closed
	 */
	abstract void log(Formula f, BooleanValue translation, Environment<BooleanMatrix> env);
	
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
