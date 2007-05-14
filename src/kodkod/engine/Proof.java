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

import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Set;

import kodkod.ast.Formula;
import kodkod.ast.Node;
import kodkod.engine.fol2sat.TranslationLog;
import kodkod.engine.fol2sat.TranslationRecord;
import kodkod.engine.satlab.ReductionStrategy;
import kodkod.engine.ucore.StrategyUtils;

/**
 * Contains a proof of unsatisfiability of a
 * given FOL formula.
 * 
 * @specfield formula: Formula // the unsatisfiable formula
 * @specfield bounds: Bounds // the bounds with respect to which the formula is unsatisfiable
 * @specfield log: TranslationLog // log of the translation of this.formula with respect to this.bounds
 * @invariant log.formula = formula
 */
public abstract class Proof {
	private final TranslationLog log;
	
	/**
	 * Constructs a new ResolutionRefutation of unsatisfiability for log.formula.
	 * @requires formula = log.formula
	 */
	Proof(TranslationLog log) {
		this.log = log;
	}
	
	/**
	 * Minimizes the proof of this.formula's unsatisfiability
	 * using the specified proof reduction strategy.  Calling this method
	 * on a proof of unsatisfiability for a trivially unsatisfiable formula
	 * results in an  UnsupportedOperationException}.
	 * @effects minimizes the proof of this.formula's unsatisfiability
	 * using the specified proof reduction strategy. 
	 * @throws UnsupportedOperationException - this is a proof for a trivially
	 * unsatisifiable formula and cannot be minimized
	 * @see kodkod.engine.satlab.ReductionStrategy
	 */
	public abstract void minimize(ReductionStrategy strategy);
	
	/**
	 * Returns an iterator over the {@link TranslationRecord log records} for the nodes
	 * that are in the unsatisfiable core of this.formula.   The record objects returned by the iterator are not 
	 * guaranteed to be immutable.  In particular, the state of a record object
	 * returned by <tt>next()</tt> is guaranteed to remain the same only until the
	 * subsequent call to <tt>next()</tt>.
	 * @return  an iterator over the {@link TranslationRecord log records} for the nodes
	 * that are in the unsatisfiable core of this.formula.
	 */
	public abstract Iterator<TranslationRecord> core() ;
	
	/**
	 * Returns the unsatisfiable subset of the top-level conjunctions of this.formula
	 * as given by {@linkplain #core() this.core()}.
	 * @return the unsatisfiable subset of the top-level conjunctions of this.formula,
	 * as given by {@linkplain #core() this.core()}.
	 */
	public final Set<Formula> highLevelCore() {
		final Set<Formula> topFormulas = StrategyUtils.topFormulas(log.formula());
		final Set<Formula> topCoreFormulas = new LinkedHashSet<Formula>();
		for(Iterator<TranslationRecord> iter = core(); iter.hasNext(); ) {
			Node next = iter.next().node();
			if (topFormulas.contains(next))
				topCoreFormulas.add((Formula)next);
		}
//		System.out.println("top formulas: " + topFormulas.size());
		return topCoreFormulas;
	}
	
	/**
	 * Returns the log of the translation that resulted
	 * in this proof.
	 * @return log of the translation that resulted in this proof
	 */
	public final TranslationLog log() {
		return log;
	}
	
	/**
	 * Returns a string representation of this proof.
	 * @return a string representation of this proof.
	 * @see java.lang.Object#toString()
	 */
//	public String toString() {
//		final StringBuilder ret = new StringBuilder();
//		for(Formula f : highLevelCore()) {
//			ret.append(" ");
//			ret.append(f);
//			ret.append("\n");
//		}
//		return ret.toString();
//	}
	
}