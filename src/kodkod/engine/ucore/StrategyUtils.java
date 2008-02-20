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
package kodkod.engine.ucore;

import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import kodkod.ast.Formula;
import kodkod.ast.Node;
import kodkod.ast.Variable;
import kodkod.engine.fol2sat.RecordFilter;
import kodkod.engine.fol2sat.TranslationLog;
import kodkod.engine.fol2sat.TranslationRecord;
import kodkod.engine.fol2sat.Translator;
import kodkod.engine.satlab.Clause;
import kodkod.engine.satlab.ResolutionTrace;
import kodkod.instance.TupleSet;
import kodkod.util.ints.IntBitSet;
import kodkod.util.ints.IntIterator;
import kodkod.util.ints.IntSet;
import kodkod.util.ints.IntTreeSet;
import kodkod.util.ints.Ints;

/**
 * A collection of utility methods for implementing
 * logic-level reduction strategies.
 * 
 * @author Emina Torlak
 */
public final class StrategyUtils {
	private StrategyUtils() {}
	
	
	/**
	 * Returns the variables that correspond to the roots of log.formula.  
	 * @return
	 * <pre> 
	 * { v: int | some r: log.records | 
	 *   r.node in log.roots() and 
	 *   r.env.isEmpty() and
	 *   abs(r.literal) != Integer.MAX_VALUE and
	 *   v = abs(r.literal) and
	 *   no r': log.records | r'.node = r.node && log.replay.r' > log.replay.r }
	 * </pre>
	 */
	public static IntSet rootVars(TranslationLog log) {
		final IntSet rootVars = new IntTreeSet();
		final Set<Formula> roots = log.roots();
		final Map<Formula,int[]> maxRootVar = new IdentityHashMap<Formula,int[]>(roots.size());
		final RecordFilter filter = new RecordFilter() {
			public boolean accept(Node node, int literal, Map<Variable, TupleSet> env) {
				return roots.contains(node) && env.isEmpty();
			}
		};
		for(Iterator<TranslationRecord> itr = log.replay(filter); itr.hasNext();) {
			TranslationRecord record = itr.next();
			int[] var = maxRootVar.get(record.node());
			if (var==null) {
				var = new int[1];
				maxRootVar.put((Formula)record.node(), var);
			} 
			var[0] = StrictMath.abs(record.literal());
		}
		
		for(int[] var : maxRootVar.values()) {
			int topVar = var[0];
			if (topVar != Integer.MAX_VALUE) // formula simplified to TRUE
				rootVars.add(var[0]);
		}
		return rootVars;
	}
	
	/**
	 * Returns relevant  core variables -- i.e. all variables that occur both in the positive and
	 * negative phase in trace.core.
	 * @return { v: [1..) | (some p, n: trace.core | v in trace.elts[p].literals and -v in trace.elts[n].literals) }
	 */
	public static IntSet coreVars(ResolutionTrace trace) { 

		final IntSet posVars = new IntTreeSet(), negVars = new IntTreeSet();
		
		for(Iterator<Clause> iter = trace.iterator(trace.core()); iter.hasNext();) {
			Clause clause = iter.next();
			for(IntIterator lits = clause.literals(); lits.hasNext(); ) {
				int lit = lits.next();
				if (lit > 0) posVars.add(lit);
				else negVars.add(-lit);
			}
		}
		
		posVars.retainAll(negVars);
		
		assert !posVars.isEmpty();
		final IntSet ret = new IntBitSet(posVars.max()+1);
		ret.addAll(posVars);
		
		return ret;
	}
	
	/**
	 * Returns the set of all variables in the core of the given trace
	 * that form unit clauses.
	 * @return { v: [1..) | some c: trace.core | c.size() = 1 and c.maxVariable() = v }
	 */
	public static IntSet coreUnits(ResolutionTrace trace) { 
		final IntSet units = new IntTreeSet();
		
		for(Iterator<Clause> itr = trace.reverseIterator(trace.core()); itr.hasNext(); ) { 	
			Clause c = itr.next();
			if (c.size()==1) { 
				units.add(c.maxVariable());
			}
		}
		
		if (units.isEmpty()) return Ints.EMPTY_SET;
		
		return Ints.asSet(units.toArray());
	}
	
	/**
	 * Returns the set of consecutive variables at the tail of the core of the given trace
	 * that form unit clauses.
	 * @return set of consecutive variables at the tail of the core of the given trace
	 * that form unit clauses
	 */
	static IntSet coreTailUnits(ResolutionTrace trace) { 
		final IntSet units = new IntTreeSet();
		
		for(Iterator<Clause> itr = trace.reverseIterator(trace.core()); itr.hasNext(); ) { 	
			Clause c = itr.next();
			if (c.size()==1) { 
				units.add(c.maxVariable());
			} else {
				break;
			}
		}
		
		return units;
	}
	
	/**
	 * Returns the indices of all axioms
	 * in the given trace that form the translations of the formulas
	 * identified by the given variables.  This method assumes that
	 * the axioms in the given trace were generated by the Kodkod
	 * {@linkplain Translator}.
	 * @return 
	 * let C = { c: trace.prover.clauses | c.maxVariable() in relevantVars },
	 *     T = { c1, c2: C | c2.maxVariable() in abs(c1.literals) } |
	 *     C.*T 
	 */
	static IntSet clausesFor(ResolutionTrace trace, IntSet relevantVars) { 
//		System.out.println("relevant: " + relevantVars);
		final IntSet axioms = trace.axioms();

		final IntSet reachableVars = new IntBitSet(relevantVars.max()+1);
		reachableVars.addAll(relevantVars);

		final IntSet relevantAxioms = new IntBitSet(axioms.size());
		
		final Iterator<Clause> itr = trace.reverseIterator(axioms);
		for(int i = axioms.max(); i >= 0; i--) {
			Clause clause = itr.next();
			int maxVar = clause.maxVariable(), size = clause.size();
			if ((size>1 && reachableVars.contains(maxVar)) || (size==1 && relevantVars.contains(maxVar))) {
				for(IntIterator lits = clause.literals(); lits.hasNext(); ) {
					reachableVars.add(StrictMath.abs(lits.next()));
				}
				relevantAxioms.add(i);
			} 
		}
		
		return  relevantAxioms;
	}
	
	
	
}
