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
package kodkod.util.nodes;

import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Set;

import kodkod.ast.BinaryFormula;
import kodkod.ast.Formula;
import kodkod.ast.NaryFormula;
import kodkod.ast.operator.FormulaOperator;

/**
 * Provides utility methods for constructing balanced
 * Kodkod trees using associative operators.
 * 
 * @author Emina Torlak
 */
public final class Nodes {
	private Nodes() {}
	
	/**
     * Returns the roots of the given formula.
     * In other words, breaks up the given formula into its conjunctive 
     * components, {f0, ..., fk}, 
     * such that, for all 0<=i<=k, f<sub>i</sub> is not a conjunction  and
     * [[f0 && ... && fk]] <=> [[formula]].  
     * @return subformulas, {f0, ..., fk}, of the given formula such that, for all 0<=i<=k, 
     * f<sub>i</sub> is not a conjuction and [[f0 && ... && fk]] <=> [[formula]].    
     */
	public static Set<Formula> roots(Formula formula) {
	
    	final List<Formula> formulas = new LinkedList<Formula>();
		formulas.add(formula);
		
		int size;
		do {
			size = formulas.size();
			ListIterator<Formula> itr = formulas.listIterator();
			while(itr.hasNext()) {
				Formula f = itr.next();
				if (f instanceof BinaryFormula) {
					BinaryFormula bin = (BinaryFormula) f;
					if (bin.op()==FormulaOperator.AND) {
						itr.remove();
						itr.add(bin.left());
						itr.add(bin.right());
					}
				} else if (f instanceof NaryFormula) { 
					NaryFormula nf = (NaryFormula) f;
					if (nf.op()==FormulaOperator.AND) { 
						itr.remove();
						for(Formula child : nf) { 
							itr.add(child);
						}
					}
				}
			}
		} while (formulas.size() > size);
		
		return new LinkedHashSet<Formula>(formulas);
	}
	
}
