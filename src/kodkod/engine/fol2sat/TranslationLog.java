/**
 * 
 */
package kodkod.engine.fol2sat;

import java.util.Iterator;
import java.util.Map;

import kodkod.ast.Node;
import kodkod.ast.Variable;
import kodkod.engine.bool.BooleanConstant;
import kodkod.instance.TupleSet;
import kodkod.util.ints.IntSet;
import kodkod.util.ints.Ints;

/**
 * A log of the translations of all descendants of a given formula that 
 * are either formulas or that desugar to formulas.
 * @specfield formula: Formula 
 * @specfield bounds: Bounds
 * @specfield records: set Record
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
	public abstract Iterator<Record> replay(IntSet literals);
	
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
	public final Iterator<Record> replay() {
		return replay(Ints.rangeSet(Ints.range(BooleanConstant.FALSE.label(), BooleanConstant.TRUE.label())));
	}
		
	/**
	 * A record of a translation event.
	 * @specfield node: Node // formula (or a node desugared to a formula) that was translated
	 * @specfield literal: int // cnf literal representing the meaning of this.node in this.env
	 * @specfield env: Variable ->one TupleSet // bindings for free, non-skolemized variables 
	 *                                         // for which this.node (or its desugared form) evaluates to this.literal   
	 * @author Emina Torlak
	 */
	public static abstract class Record {
				
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
}
