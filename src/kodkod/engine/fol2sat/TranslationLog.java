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

import java.util.Iterator;

import kodkod.engine.bool.BooleanConstant;
import kodkod.util.ints.IntSet;
import kodkod.util.ints.Ints;

/**
 * A log of the translations of all descendants of a given formula that 
 * are either formulas or that desugar to formulas.
 * @specfield formula: Formula 
 * @specfield bounds: Bounds
 * @specfield records: set TranslationRecord
 * @invariant all r: records | r.node in formula.*children
 * @author Emina Torlak
 */
public abstract class TranslationLog {
	
	/**
	 * Returns an iterator over the translation records in this log that contain
	 * the given literals.  The iterator returns the records in the order in which
	 * they were generated.  This guarantees that records for the descendents of a 
	 * node are always returned before the record for the node itself.  
	 * 
	 * <p><b>Note:</b>The record objects returned by the iterator are not 
	 * required to be immutable.  In particular, the state of a record object
	 * returned by <tt>next()</tt> is guaranteed to remain the same only until the
	 * subsequent call to <tt>next()</tt>.</p>
	 * @return an iterator over the translation records in this log that contain the
	 * given literals
	 */
	public abstract Iterator<TranslationRecord> replay(IntSet literals);
	
	/**
	 * Returns an iterator over all translation records in this log.  The effect
	 * of this method is equivalent to calling {@link #replay(IntSet)} with an
	 * IntSet containing all integers in the range [{@link BooleanConstant#FALSE}.label, 
	 * {@link BooleanConstant#TRUE}.label].  
	 * 
	 * <p><b>Note:</b>The record objects returned by the iterator are not 
	 * required to be immutable.  In particular, the state of a record object
	 * returned by <tt>next()</tt> is guaranteed to remain the same only until the
	 * subsequent call to <tt>next()</tt>.</p>
	 * @return an iterator over all translation records in this.log.
	 * @see kodkod.engine.fol2sat.TranslationLog#replay(IntSet)
	 */
	public final Iterator<TranslationRecord> replay() {
		return replay(Ints.rangeSet(Ints.range(BooleanConstant.FALSE.label(), BooleanConstant.TRUE.label())));
	}
}
