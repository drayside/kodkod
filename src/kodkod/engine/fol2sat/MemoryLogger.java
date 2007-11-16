/* 
 * Kodkod -- Copyright (c) 2005-2008, Emina Torlak
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

import java.util.Collections;
import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;

import kodkod.ast.Formula;
import kodkod.ast.Node;
import kodkod.ast.Variable;
import kodkod.engine.bool.BooleanMatrix;
import kodkod.engine.bool.BooleanValue;
import kodkod.instance.TupleSet;
import kodkod.util.collections.FixedMap;
import kodkod.util.nodes.AnnotatedNode;
import kodkod.util.nodes.Nodes;

/**
 * A translation logger that logs root translation events
 * to memory.  In other words, this logger only logs the 
 * translations for the roots of a given formula.  The 
 * translation events for the roots' descendants are ignored.
 * @specfield annotated:  AnnotatedNode<Formula>
 * @specfield formula: annotated.source[annotated.node]
 * @specfield transforms: ~(annotated.source) & Formula->Formula
 * @specfield bounds: Bounds
 * @specfield records: transforms.Formula ->lone (BooleanValue -> Environment<BooleanMatrix>)
 * @invariant annotated.source[annotated.sourceSensitiveRoots()] = Nodes.roots(annotated.source[annotated.node])
 * @invariant dom[records] = Nodes.roots(formula)
 * @author Emina Torlak
 */
final class MemoryLogger extends TranslationLogger {
	private final FixedMap<Formula, BooleanValue> logMap;
	private final AnnotatedNode<Formula> annotated;
	
	/**
	 * Constructs a new memory logger from the given annotated formula.
	 * @requires annotated.source[annotated.sourceSensitiveRoots()] = Nodes.roots(annotated.source[annotated.node])
	 * @effects this.annotated' = annotated
	 * @effects no this.records' 
	 */
	MemoryLogger(final AnnotatedNode<Formula> annotated) {
		this.annotated = annotated;
		this.logMap = new FixedMap<Formula, BooleanValue>(annotated.sourceSensitiveRoots());
	}

	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.fol2sat.TranslationLogger#close()
	 */
	@Override
	void close() {}

	/**
	 * Logs the translation of the given formula if and only if f is in
	 * this.annotated.sourceSensitiveRoots().  
	 * @effects f in this.annotated.sourceSensitiveRoots() and no this.records[f] =>
	 *   this.records' = this.records ++ this.transforms.f -> translation -> env
	 * @throws IllegalArgumentException - some this.records[f] and this.records[f] != translation -> env
	 * @see kodkod.engine.fol2sat.TranslationLogger#log(kodkod.ast.Formula, kodkod.engine.bool.BooleanValue, kodkod.engine.fol2sat.Environment)
	 */
	@Override
	void log(Formula f, BooleanValue translation, Environment<BooleanMatrix> env) {
		if (logMap.containsKey(f)) { 
			assert env.isEmpty();
			final BooleanValue old = logMap.put(f, translation);
			if (old!=null && old!=translation) 
				throw new IllegalArgumentException("translation of root corresponding to the formula has already been logged: " + f);
		}
	}

	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.fol2sat.TranslationLogger#log()
	 */
	@Override
	TranslationLog log() { return new MemoryLog(annotated,logMap); }
	
	/**
	 * A memory-based translation log, written by a MemoryLogger.
	 * @author Emina Torlak
	 */
	private static class MemoryLog extends TranslationLog {
		private final Set<Formula> roots;
		private final int[] transls;
		
		/**
		 * Constructs a new memory log out of the given node and its corresponding log map.
		 */
		MemoryLog(AnnotatedNode<Formula> annotated, FixedMap<Formula, BooleanValue> logMap) { 
			this.roots = Collections.unmodifiableSet(Nodes.roots((Formula)annotated.sourceOf(annotated.node())));
			assert roots.size() == logMap.size();
			this.transls = new int[roots.size()];
			final FixedMap<Formula, int[]> tmp = new FixedMap<Formula, int[]>(roots);
			for(Map.Entry<Formula,BooleanValue> e : logMap.entrySet()) { 
				tmp.put((Formula)annotated.sourceOf(e.getKey()), new int[]{e.getValue().label()});
			}
			final Iterator<Formula> itr = roots.iterator();
			for(int i = 0; i < transls.length; i++) { 
				transls[i] = tmp.get(itr.next())[0];
			}
		}
		
		/**
		 * {@inheritDoc}
		 * @see kodkod.engine.fol2sat.TranslationLog#replay(kodkod.engine.fol2sat.RecordFilter)
		 */
		@Override
		public Iterator<TranslationRecord> replay(final RecordFilter filter) {
			return new Iterator<TranslationRecord>() {
				final Iterator<Formula> itr = roots.iterator();
				boolean ready = false;
				int index = -1;
				Formula root = null;
				final TranslationRecord current = new TranslationRecord() {
					@Override
					public Map<Variable, TupleSet> env() { return Collections.emptyMap(); }
					@Override
					public int literal() { return transls[index]; }
					@Override
					public Node node() { return root; }
				};
				
				@SuppressWarnings("unchecked")
				public boolean hasNext() {
					while(!ready && itr.hasNext()) { 
						root = itr.next();
						index++;
						if (filter.accept(root, transls[index], Collections.EMPTY_MAP)) { 
							ready = true;
							break;
						}
					}
					return ready;
				}
				
				public TranslationRecord next() {
					if (!hasNext()) throw new NoSuchElementException();
					ready = false;
					return current;
				}

				public void remove() { throw new UnsupportedOperationException(); }
			};
		}

		/**
		 * {@inheritDoc}
		 * @see kodkod.engine.fol2sat.TranslationLog#roots()
		 */
		@Override
		public Set<Formula> roots() { return roots; } 
		
	}

}
