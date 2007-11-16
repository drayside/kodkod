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
 * @specfield records: (iden ++ transforms).Formula -> BooleanValue -> Environment<BooleanMatrix>
 * @author Emina Torlak
 */
abstract class TranslationLogger {

	/**
	 * Optionally records the translation of the source of the 
	 * given transformed formula to the given boolean value 
	 * in the specified environment.
	 * @requires some this.transforms.f
	 * @effects this.records' = this.records or 
	 * this.records' = this.records + this.transforms.f -> translation -> freeVariables(f)<:env
	 * @throws IllegalArgumentException - some aspect of the given translation event prevents it from being logged
	 * @throws IllegalStateException - this log has been closed
	 */
	abstract void log(Formula f, BooleanValue translation, Environment<BooleanMatrix> env);
	
	/**
	 * Closes this logger and releases associated resources.  Attempts to call {@link #log(Formula, BooleanValue, Environment)}
	 * after the log has been closed may result in an IllegalStateException.
	 * @effects closes this logger and releases associated resources. 
	 */
	abstract void close();
	
	/**
	 * Returns a TranslationLog view of this.records.
	 * @return a TranslationLog view of this.records.
	 */
	abstract TranslationLog log();
}
