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
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Set;

import kodkod.ast.BinaryFormula;
import kodkod.ast.Formula;
import kodkod.ast.Node;
import kodkod.ast.Variable;
import kodkod.engine.fol2sat.RecordFilter;
import kodkod.engine.fol2sat.TranslationLog;
import kodkod.engine.fol2sat.TranslationRecord;
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
     * Returns the top-level components of the given formula.
     * In other words, returns the subformulas, {f0, ..., fk}, 
     * of the given formula such that, for all 0<=i<=k, f<sub>i</sub> is not a conjuction  and
     * [[f0 && ... && fk]] <=> [[formula]].  
     * @return subformulas, {f0, ..., fk}, of the given formula such that, for all 0<=i<=k, 
     * f<sub>i</sub> is not a conjuction and [[f0 && ... && fk]] <=> [[formula]].    
     */
	public static Set<Formula> topFormulas(Formula formula) {
	
    	final List<Formula> formulas = new LinkedList<Formula>();
		formulas.add(formula);
		if (formula instanceof BinaryFormula) {
			final BinaryFormula top = (BinaryFormula) formula;
			if (top.op()==BinaryFormula.Operator.AND) {
				int size;
				do {
					size = formulas.size();
					ListIterator<Formula> itr = formulas.listIterator();
					while(itr.hasNext()) {
						Formula f = itr.next();
						if (f instanceof BinaryFormula) {
							BinaryFormula bin = (BinaryFormula) f;
							if (bin.op()==BinaryFormula.Operator.AND) {
								itr.remove();
								itr.add(bin.left());
								itr.add(bin.right());
							}
						}
					}
				} while (formulas.size() > size);
			}
		}
		return new LinkedHashSet<Formula>(formulas);
	}

	/**
	 * Returns the variables that correspond to the top-level
	 * subformulas of log.formula
	 * @return
	 * <pre> 
	 * { v: int | some r: log.records | 
	 *   r.node in topFormulas(log.formula) and 
	 *   r.env.isEmpty() and
	 *   abs(r.literal) != Integer.MAX_VALUE and
	 *   v = abs(r.literal) and
	 *   no r': log.records | r'.node = r.node && abs(r'.literal) > v }
	 * </pre>
	 */
	public static IntSet topVars(TranslationLog log) {
		final IntSet topVars = new IntTreeSet();
		final Set<Formula> topFormulas = topFormulas(log.formula());
		final Map<Formula,int[]> maxFormulaVars = new IdentityHashMap<Formula,int[]>(topFormulas.size());
		for(Formula top : topFormulas) {
			maxFormulaVars.put(top, new int[1]);
		}
		final RecordFilter filter = new RecordFilter() {
			public boolean accept(Node node, int literal, Map<Variable, TupleSet> env) {
				return topFormulas.contains(node) && env.isEmpty();
			}
		};
		for(Iterator<TranslationRecord> itr = log.replay(filter); itr.hasNext();) {
			TranslationRecord record = itr.next();
			int[] var = maxFormulaVars.get(record.node());
			int recordVar = StrictMath.abs(record.literal());
			if (recordVar < Integer.MAX_VALUE) {
				var[0] = StrictMath.max(var[0], recordVar);
			} 
		}
		for(int[] var : maxFormulaVars.values()) {
			int topVar = var[0];
			if (topVar != 0) // topVar could be 0 if a top-level formula simplified to TRUE
				topVars.add(var[0]);
		}

		return topVars;
	}
	
	/**
	 * Returns the variables that correspond to all the subformulas of log.formula
	 * @return { lit: abs(log.records.literal) | lit < Integer.MAX_VALUE } 
	 */
	public static IntSet formulaVars(TranslationLog log) {
		final IntSet formulaVars = new IntTreeSet();
		
		for(Iterator<TranslationRecord> itr = log.replay(); itr.hasNext(); ) {
			int literal = StrictMath.abs(itr.next().literal());
			if (literal < Integer.MAX_VALUE) {
				formulaVars.add(literal);
			}
		}
		
		if (formulaVars.isEmpty()) return Ints.EMPTY_SET;
		final IntSet ret = new IntBitSet(formulaVars.max()+1);
		ret.addAll(formulaVars);
		return ret;
	}
	
	/**
	 * Returns relevant core variables -- i.e. variables that occur both in the positive and negative
	 * phase in trace.core.
	 * @return { v: [1..) | some p, n: trace.core | v in trace.elts[p].literals and -v in trace.elts[n].literals }
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
	 * Returns the indices of the clauses in the unsatifiable core of the
	 * given trace that have the specified maximum variable.
	 * @return { i: trace.core() | trace[i].maxVariable() = maxVariable }
	 */
	public static IntSet coreWithMaxVar(ResolutionTrace trace, int maxVariable) {
		final IntSet core = trace.core();
		final IntSet restricted = new IntBitSet(core.max()+1);
		final Iterator<Clause> clauses = trace.iterator(core);
		final IntIterator indices = core.iterator();
		while(clauses.hasNext()) {
			Clause clause = clauses.next();
			int index = indices.next();
			if (clause.maxVariable()==maxVariable)
				restricted.add(index);
		}
		return restricted;
	}
}
