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
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Set;

import kodkod.ast.BinaryFormula;
import kodkod.ast.Formula;
import kodkod.ast.Node;
import kodkod.engine.fol2sat.TranslationLog;
import kodkod.engine.fol2sat.TranslationRecord;
import kodkod.engine.satlab.ReductionStrategy;

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
	 * Constructs a new Proof of unsatisfiability for log.formula.
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
//	
//	/**
//	 * Returns a formula that represents the logical "unsatisfiable core" of this.formula,
//	 * computed using the raw core returned by the {@linkplain #core()} method.  Structurally,
//	 * the returned formula f is a subgraph of this.formula which itself is unsatisfiable.
//	 * @return the logical "unsatisfiable core" of this.formula
//	 */
//	public final Formula logicalCore() {
//		final Set<Node> core = new IdentityHashSet<Node>();
//		for(Iterator<TranslationRecord> iter = core(); iter.hasNext(); ) {
//			core.add(iter.next().node());
//		}
//		System.out.println("Core: " + core);
//		return LogicalCoreConstuctor.construct(log.formula(), core);
//	}

	/**
	 * Flattens the top level conjunction.  In other words, returns the subformulas
	 * f0, ..., fk of the given formula such that calling f0.and(f1)...and(fk) produces
	 * an AST isomorphic to the given formula.  The formulas f0, ..., fk are guaranteed not to be
	 * conjunctions.
	 * @return a set containing the flattened children of the top level conjunction
	 */
	private static final Set<Formula> flatten(Formula formula) {
		final List<Formula> formulas = new LinkedList<Formula>();
		formulas.add(formula);
		if (formula instanceof BinaryFormula) {
			final BinaryFormula top = (BinaryFormula) formula;
			final BinaryFormula.Operator op = top.op();
			if (op==BinaryFormula.Operator.AND) {
				int size;
				do {
					size = formulas.size();
					ListIterator<Formula> itr = formulas.listIterator();
					while(itr.hasNext()) {
						Formula f = itr.next();
						if (f instanceof BinaryFormula) {
							BinaryFormula bin = (BinaryFormula) f;
							if (bin.op()==op) {
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
	 * Returns the unsatisfiable subset of the top-level conjunctions of this.formula
	 * as given by {@linkplain #core() this.core()}.
	 * @return the unsatisfiable subset of the top-level conjunctions of this.formula,
	 * as given by {@linkplain #core() this.core()}.
	 */
	public final Set<Formula> highLevelCore() {
		final Set<Formula> topFormulas = flatten(log.formula());
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
	
//	/**
//	 * Returns a string representation of this proof.
//	 * @return a string representation of this proof.
//	 * @see java.lang.Object#toString()
//	 */
//	public String toString() {
//		final StringBuilder ret = new StringBuilder();
//		final Iterator<Formula> iter = topCoreFormulas().iterator();
//		if (iter.hasNext()) 
//			ret.append(iter.next());
//		while(iter.hasNext()) {
//			ret.append(" &&\n");
//			ret.append(iter.next());
//		}
//		return ret.toString();
//	}
	
	/**
	 * Constructs the logical unsatisfiable core for a given formula.
	 * This visitor should only be used through the {@linkplain #construct(Formula, Set)} method.
	 * 
	 * @specfield root: Formula
	 * @specfield core: set Node
	 * @invariant root in core 
	 * @invariant all n: core | n in root.*children
	 * @invariant all n: core | some s: set core | n + root in s and (s - root).~children in s
	 * 
	 * @author Emina Torlak
	 */
//	private static final class LogicalCoreConstuctor extends AbstractReplacer {
//		private final Set<Node> core;
//		
//		/**
//		 * Constructs a logical core constructor for the given core.
//		 * @effects this.core' = core
//		 */
//		@SuppressWarnings("unchecked")
//		protected LogicalCoreConstuctor(Set<Node> core) {
//			super(Collections.EMPTY_SET);
//			this.core = core;
//		}
//		
//		/**
//		 * Constructs the unsatisfiable subgraph the given formula,
//		 * using the given core nodes.
//		 * @return  unsatisfiable subgraph the given formula constructed
//		 * from the given core nodes.
//		 */
//		static Formula construct(Formula root, Set<Node> core) {
//			final LogicalCoreConstuctor constructor = new LogicalCoreConstuctor(core);
//			return root.accept(constructor);
//		}
//
//		/* The following methods treat their arguments as "elementary" formulas; that is, 
//		 * they don't visit the their argument's children.  The children could be visited
//		 * to potentially get a smaller core, but there are a number of tricky corner cases
//		 * to work out, so, for now, we just treat these formulas as basic blocks.  */
//		public Formula visit(QuantifiedFormula quantFormula) { return quantFormula; }
//		public Formula visit(ComparisonFormula compFormula) { return compFormula; }
//		public Formula visit(MultiplicityFormula multFormula) { return multFormula; }
//		public Formula visit(RelationPredicate pred) {  return pred; }
//		public Formula visit(IntComparisonFormula intComp) { return intComp; }
//		
//		/** 
//		 * The fact that this method has been called guarantees that this.core.contains(not).
//		 * The method visits the formula's child if the child is in the core.  Otherwise it returns the argument.
//		 * @return this.core.contains(not.formula) => not.formula.accept(this).not() else not
//		 **/
//		public Formula visit(NotFormula not) {
//			return core.contains(not.formula()) ? not.formula().accept(this).not() : not;
//		}
//		
//		/** 
//		 * The fact that this method has been called guarantees that this.core.contains(binFormula).
//		 * The method returns the result of combining relevant parts of the core children with binFormula.op.
//		 * @return  the result of combining relevant parts of the core children with binFormula.op.
//		 **/
//		public Formula visit(BinaryFormula binFormula) {
//			final BinaryFormula.Operator op = binFormula.op();
//			final Formula l = binFormula.left(), r = binFormula.right();
//			final boolean lvisit = core.contains(l), rvisit = core.contains(r);
//			
//			final Formula lnew, rnew;
//			
//			if (lvisit && rvisit) {
//				lnew = l.accept(this);
//				rnew = r.accept(this);
//			} else if (!lvisit && !rvisit) {
//				lnew = l;
//				rnew = r;
//			} else {
//				switch(op) {
//				case AND : 
//					lnew = lvisit ? l.accept(this) : Formula.TRUE;
//					rnew = rvisit ? r.accept(this) : Formula.TRUE;
//					break;
//				case OR : 
//					lnew = lvisit ? l.accept(this) : Formula.FALSE;
//					rnew = rvisit ? r.accept(this) : Formula.FALSE;
//					break;
//				case IMPLIES: // !l || r
//					lnew = lvisit ? l.accept(this) : Formula.TRUE;
//					rnew = rvisit ? r.accept(this) : Formula.FALSE;
//					break;
//				case IFF: // (!l || r) && (l || !r) 
//					lnew = lvisit ? l.accept(this) : l;
//					rnew = rvisit ? r.accept(this) : r;
//					break;
//				default :
//					throw new IllegalArgumentException("Unknown operator: " + op);
//				}
//			}
//			
//			return l==lnew && r==rnew ? binFormula : lnew.compose(op, rnew);
//		}
//	}
}