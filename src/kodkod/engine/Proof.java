package kodkod.engine;

import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import kodkod.ast.Node;
import kodkod.engine.satlab.SATSolver;
import kodkod.util.IntSet;
import kodkod.util.Ints;

/**
 * Contains a proof of unsatisfiability of a
 * given FOL formula.
 * 
 * @specfield formula: Formula // the unsatisfiable formula
 * @specfield bounds: Bounds // the bounds with respect to which the formula is unsatisfiable
 */
public final class Proof {
	private final SATSolver solver;
	private final Map<Node,IntSet> node2vars;
	private boolean fixed;
	/**
	 * Constructs a new Proof that will extract the 
	 * unsatisfiable core for this.formula from 
	 * the given solver.  The given map is required to 
	 * either be null or map the descendents of this.formula to the 
	 * CNF variables assigned to them during translation.
	 * @requires solver.solve() has been called and 
	 * it returned false.
	 * @requires node2vars.map.IntSet in this.formula.*children
	 * && nod2vars.map[Node].ints in [1..solver.numberOfVariables].
	 */
	Proof(SATSolver solver, Map<Node,IntSet> node2vars) {
		assert solver != null;
		this.solver = solver;
		this.fixed = false;
		this.node2vars = node2vars;
	}
	
	/**
	 * Returns the size of the proof:  the
	 * number of clauses in this.formula's
	 * unsatisfiable core.
	 * @return the size of this proof
	 * @throws UnsupportedOperationException -- the {@link Options options} 
	 * with which the {@link Solver solver}
	 * was invoked did not specify the use of a core extracting 
	 * {@link kodkod.engine.satlab.SATSolver sat solver} 
	 */
	public int size() {
		return solver.coreSize();
	}
	
	/**
	 * Returns an iterator over the CNF clauses 
	 * that constitute the proof of this.formula's
	 * unsatisfiability wirth respect to this.bounds.
	 * @return an iterator over this formula's 
	 * unsatisfiable core.
	 * @throws UnsupportedOperationException -- the {@link Options options} 
	 * with which the {@link Solver solver}
	 * was invoked did not specify the use of a core extracting 
	 * {@link kodkod.engine.satlab.SATSolver sat solver} 
	 */
	public Iterator<int[]> clauses() {
		return solver.unsatCore();
	}
	
	/**
	 * Refines the proof of this.formula's unsatisfiability
	 * until a fixed point is reached; that is, until the 
	 * formula's unsatisfiable core cannot be minimized any
	 * further.
	 * @effects refines this proof until a fixed point is reached.
	 * @throws UnsupportedOperationException -- the {@link Options options} 
	 * with which the {@link Solver solver}
	 * was invoked did not specify the use of a core extracting 
	 * {@link kodkod.engine.satlab.SATSolver sat solver} 
	 */
	public void refine() {
		if (fixed) return;
	
		for(int size = solver.coreSize(); solver.coreSize() < size; ) {
			solver.retainCore();
			try {
				solver.solve();
			} catch (TimeoutException e) {
				break;
			}
		}
		
		fixed = true;
	}
	
	/**
	 * Refines the proof of this.formula's unsatisfiability
	 * <code>numOfIterations</code> times or until a fixed point is reached,
	 * whichever comes first.
	 * @effects refines this proof 
	 * <code>numOfIterations</code> times or until a fixed point is reached,
	 * whichever comes first.
	 * @throws UnsupportedOperationException -- the {@link Options options} 
	 * with which the {@link Solver solver}
	 * was invoked did not specify the use of a core extracting 
	 * {@link kodkod.engine.satlab.SATSolver sat solver}
	 * @throws IllegalArgumentException - numOfIterations < 0 
	 */
	public void refine(int numOfIterations) {
		if (numOfIterations < 0)
			throw new IllegalArgumentException("numOfIterations < 0: " + numOfIterations);
		if (fixed) return;
		
		for(int size = solver.coreSize(); numOfIterations > 0 && solver.coreSize() < size; 
		    numOfIterations--) {
			solver.retainCore();
			try {
				solver.solve();
			} catch (TimeoutException e) {
				break;
			}
		}
		
		fixed = numOfIterations > 0;
	}
	
	/**
	 * Returns the set of CNF variables, identified by
	 * integer literals, allocated to the
	 * given node during the translation of this.formula.  
	 * These may be used to establish a 
	 * correspondence between the contents of this.formula's
	 * unsatisfiable core and its nodes.  If no variables
	 * were allocated to the given node, an empty set is returned.
	 * Note that the returned set will contain only positive integers;
	 * the presence of a negative integer in a clause indicates that
	 * its corresponding variable is negated in that clause.
	 * @requires node in this.trackedNodes()
	 * @return the set of CNF variables allocated to the given node
	 * during the translation of this.formula
	 * @throws UnsupportedOperationException - the {@link Options options} 
	 * with which the {@link Solver solver}
	 * was invoked did not specify that variables be tracked 
	 * (see {@link Options#setTrackVars(boolean)} and
	 * {@link Options#trackVars()}).
	 */
	public IntSet variablesFor(Node node) {
		if (node2vars==null)
			throw new UnsupportedOperationException("variable tracking not enabled during translation.");
		final IntSet ret = node2vars.get(node);
		return ret==null ? Ints.EMPTY_SET : ret;
	}
	
	/**
	 * Returns the set of all descdents of this.formula that did 
	 * not reduce to a constant value during translation, <i>
	 * provided the {@link Options options} 
	 * with which the {@link Solver solver}
	 * was invoked specified that variables be tracked </i>
	 * @return the set of all descdents of this.formula that did 
	 * not reduce to a constant value during translation.  <b>The 
	 * returned set tests for containment using reference equality.</b>
	 * @throws UnsupportedOperationException - the {@link Options options} 
	 * with which the {@link Solver solver}
	 * was invoked did not specify that variables be tracked 
	 * (see {@link Options#setTrackVars(boolean)} and
	 * {@link Options#trackVars()}).
	 */
	public Set<Node> trackedNodes() {
		if (node2vars==null)
			throw new UnsupportedOperationException("variable tracking not enabled during translation.");
		return node2vars.keySet();
	}
	
}