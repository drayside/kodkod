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

import java.util.Map;

import kodkod.ast.Node;
import kodkod.ast.Variable;
import kodkod.instance.TupleSet;

/**
 * Record of a translation event.
 * @specfield node: Node // formula (or a node desugared to a formula) that was translated
 * @specfield literal: int // cnf literal representing the meaning of this.node in this.env
 * @specfield env: Variable ->one TupleSet // bindings for free, non-skolemized variables 
 *                                         // for which this.node (or its desugared form) evaluates to this.literal   
 * @author Emina Torlak
 */
public abstract class TranslationRecord {
			
	/**
	 * Returns this.node.
	 * @return this.node.
	 */
	public abstract Node node();
	
	/**
	 * Returns this.literal.
	 * @return this.literal
	 */
	public abstract int literal();
	
	/**
	 * Returns a map view of this.env.
	 * @return this.env
	 */
	public abstract Map<Variable,TupleSet> env();
	
	/**
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		final StringBuilder ret = new StringBuilder();
		ret.append("< node: ");
		ret.append(node());
		ret.append(", literal: ");
		ret.append(literal());
		ret.append(", env: ");
		ret.append(env());
		ret.append(">");
		return ret.toString();
	}		
}