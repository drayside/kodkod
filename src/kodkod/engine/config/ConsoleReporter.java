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
package kodkod.engine.config;


import java.util.List;

import kodkod.ast.Decl;
import kodkod.ast.Formula;
import kodkod.ast.Relation;
import kodkod.engine.bool.BooleanFormula;
import kodkod.instance.Bounds;

/**
 * An implementation of the reporter interface that prints messages
 * to the standard output stream.
 * @author Emina Torlak
 */
public final class ConsoleReporter implements Reporter {
	
	/**
	 * Constructs a new instance of the ConsoleReporter.
	 */
	public ConsoleReporter() {}
	
	/**
	 * @see kodkod.engine.config.Reporter#generatingSBP()
	 */
	public void generatingSBP() {
		System.out.println("generating lex-leader symmetry breaking predicate ...");
	}

	/**
	 * @see kodkod.engine.config.Reporter#flattening(kodkod.engine.bool.BooleanFormula)
	 */
	public void flattening(BooleanFormula circuit) {
		System.out.println("flattening ...");
	}

	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.config.Reporter#skolemizing(kodkod.ast.Decl, kodkod.ast.Relation, java.util.List)
	 */
	public void skolemizing(Decl decl, Relation skolem, List<Decl> context) {
		System.out.println("skolemizing " + decl + ": skolem relation=" + skolem + ", arity=" + skolem.arity());
	}

	/**
	 * @see kodkod.engine.config.Reporter#solvingCNF(int, int, int)
	 */
	public void solvingCNF(int primaryVars, int vars, int clauses) {
		System.out.println("solving p cnf " + vars + " " + clauses);
	}


	/**
	 * @see kodkod.engine.config.Reporter#optimizingBounds()
	 */
	public void optimizingBounds() {
		System.out.println("optimizing bounds (breaking predicate symmetries) ...");
	}

	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.config.Reporter#optimizingFormula()
	 */
	public void optimizingFormula() {
		System.out.println("optimizaing formula (inlining predicates and skolemizing) ...");
	}
	
	/**
	 * @see kodkod.engine.config.Reporter#translatingToBoolean(kodkod.ast.Formula, kodkod.instance.Bounds)
	 */
	public void translatingToBoolean(Formula formula, Bounds bounds) {
		System.out.println("translating to boolean ...");
	}

	/**
	 * @see kodkod.engine.config.Reporter#translatingToCNF(kodkod.engine.bool.BooleanFormula)
	 */
	public void translatingToCNF(BooleanFormula circuit) {
		System.out.println("translating to cnf ...");
	}
	
	/**
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return "ConsoleReporter";
	}

	

}
