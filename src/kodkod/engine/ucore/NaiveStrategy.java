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
package kodkod.engine.ucore;

import java.util.HashSet;
import java.util.Set;

import kodkod.engine.satlab.Clause;
import kodkod.engine.satlab.ReductionStrategy;
import kodkod.engine.satlab.ResolutionTrace;
import kodkod.util.ints.IntBitSet;
import kodkod.util.ints.IntIterator;
import kodkod.util.ints.IntSet;
import kodkod.util.ints.Ints;

/**
 * A naive minimization strategy that tests the presence
 * of each clause in the core by simply re-solving {core} - {clause}.
 * @author Emina Torlak
 */
public final class NaiveStrategy implements ReductionStrategy {
	private Set<Clause> excluded;
	
	/** 
	 * Constructs a new instance of NaiveStrategy. 
	 * @effects no this.traces' and no this.nexts'
	 **/
	public NaiveStrategy() {
		excluded = null;
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.satlab.ReductionStrategy#next(kodkod.engine.satlab.ResolutionTrace)
	 */
	public IntSet next(ResolutionTrace trace) {
		final IntSet core = trace.core();
		if (excluded==null) { // the first time this method is called
			excluded = new HashSet<Clause>((int)(StrictMath.round(core.size()*.75)));
		}
		
		for(IntIterator iter = core.iterator(); iter.hasNext();) {
			int index = iter.next();
			if (excluded.add(trace.get(index))) { // haven't tried excluding this one
				final IntSet next = new IntBitSet(core.max()+1);
				next.addAll(core);
				next.remove(index);
				return next;
			}
		}
		
		return Ints.EMPTY_SET;
	}

}
