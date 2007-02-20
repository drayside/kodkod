package kodkod.engine;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import kodkod.engine.fol2sat.TranslationLog;
import kodkod.engine.satlab.Clause;
import kodkod.engine.satlab.EmptyClauseConeStrategy;
import kodkod.engine.satlab.SATFactory;
import kodkod.engine.satlab.SATProver;
import kodkod.engine.satlab.TraversalStrategy;
import kodkod.util.collections.ArrayStack;
import kodkod.util.collections.Stack;
import kodkod.util.ints.IntSet;
import kodkod.util.ints.IntTreeSet;

/**
 * Contains a proof of unsatisfiability of a
 * given FOL formula.
 * 
 * @specfield formula: Formula // the unsatisfiable formula
 * @specfield bounds: Bounds // the bounds with respect to which the formula is unsatisfiable
 */
public final class Proof {
	private final TranslationLog log;
	private SATProver solver;

	/**
	 * Constructs a new Proof that will extract the 
	 * unsatisfiable core for this.formula from the given solver.  
	 * @requires the given factory produces SATProver instances
	 * @requires solver.solve() has been called and 
	 * it returned false.
	 * @requires log.formula is the formula whose translation
	 * resulted in the given SATProver
	 */
	Proof(SATFactory factory, SATProver solver, TranslationLog log) {
		this.solver = solver;
		this.log = log;
	}
	
	/**
	 * Returns the magnitude of the literal with the greatest
	 * absolute value in the given clause.  This number uniquely
	 * ties each clause to its corresponding  subformula of this.formula.
	 * @return the magnitude of the literal with the greatest
	 * absolute value in the given clause
	 */
	private static int idLiteral(Clause clause) {
		final IntSet literals = clause.literals();
		return StrictMath.max(StrictMath.abs(literals.min()), StrictMath.abs(literals.max()));
	}
	
	/**
	 * Collects the {@link #idLiteral(kodkod.engine.satlab.Clause) identifying literals}
	 * of the core clauses in an instance of IntSet.
	 * @return an IntSet initialized with the identifying literals of the core clauses
	 */
	private IntSet coreLiterals() {
		final IntSet idLits = new IntTreeSet();
		
		for(Clause clause : solver.proof()) {
			if (!clause.learned()) 
				idLits.add(idLiteral(clause)); 
		}
		return idLits;
	}

	
	/**
	 * Refines the proof of this.formula's unsatisfiability
	 * until a fixed point is reached, using the Empty-Clause Cone
	 * algorithm (L. Zhang and S. Malik. <i>Extracting small unsatisfiable cores from unsatisfiable
	 * Boolean formula.</i>  In Proceedings of Sixth International Conference on Theory and Applications of 
	 * Satisfiability Testing (SAT '03). 2003.).  The resulting proof is 
	 * not guaranteed to be minimal. 
	 * @effects refines this proof until a fixed point is reached.
	 * @see L. Zhang and S. Malik. <i>Extracting small unsatisfiable cores from unsatisfiable
	 * Boolean formula.</i>  In Proceedings of Sixth International Conference on Theory and Applications of 
	 * Satisfiability Testing (SAT '03). 2003.
	 */
	public void refine() {
		solver.proof(new EmptyClauseConeStrategy());
	}
	
	/**
	 * Returns the relative hardness of the proof of this.formula's
	 * unsatisfiability.  The higher this number, the harder the proof 
	 * is to minimize.  
	 * @return the relative hardness of the proof of this.formula's unsatisfiability.
	 */
	public double relativeHardness() {
		return solver.proof().relativeHardness();
	}

	
	/**
	 * Returns a mapping from each leaf (original clause) in the resolution trace to 
	 * the indices of its ancestors.
	 * @return a mapping from each leaf (original clause) in the resolution trace to 
	 * the indices of its ancestors
	 */
	 Map<Clause, IntSet> ancestors() {
		
		final Map<Clause, IntSet> ret = new HashMap<Clause, IntSet>();
		final Stack<Clause> path = new ArrayStack<Clause>();
		
		for(Clause clause : solver.proof()) {
			if (clause.learned()) {
				if (!path.empty() && !path.peek().antecedents().contains(clause)) path.pop();
				path.push(clause);
			} else {
				IntSet ancestors = ret.get(clause);
				if (ancestors==null) ancestors = new IntTreeSet();
				for(Clause ancestor : path) {
					ancestors.add(ancestor.index());
				}
				ret.put(clause, ancestors);
			}
		}
		return ret;
	}
	
	/**
	 * Returns a mapping from each leaf (original clause) in the resolution trace to 
	 * the length of the shortest path from the conflict clause to that leaf.
	 * @return a mapping from each leaf (original clause) in the resolution trace to 
	 * the length of the shortest path from the conflict clause to that leaf.
	 */
	 Map<Clause, Integer> shortestPaths() {
		final Clause conflict = solver.proof().conflict();
		final int maxIndex = solver.proof().maxIndex();
		final Map<Clause, Integer> ret = new HashMap<Clause, Integer>();
		
		// traverse the dag in a topologically sorted order, and compute shortest paths
		final int[] dist = new int[maxIndex+1];
		java.util.Arrays.fill(dist, 0, maxIndex+1, Integer.MAX_VALUE);
		dist[conflict.index()] = 0;
		
		for(Iterator<Clause> itr = solver.proof().iterator(TraversalStrategy.TOPOLOGICAL); itr.hasNext();) {
			Clause next = itr.next();
			if (next.learned()) {
				for(Clause ante : next.antecedents()) {
					if (dist[ante.index()] > dist[next.index()]+1) { // relax
						dist[ante.index()] = dist[next.index()]+1;
					}
				}
			} else { // core
				ret.put(next, null);
			}
		}
		
		for(Map.Entry<Clause, Integer> e : ret.entrySet()) {
			e.setValue(dist[e.getKey().index()]);
		}
		
		return ret;
	}
	
//	private Map<ProofClause, Integer> graphShortestPaths() {
//		final Map<ProofClause, Integer> ret = new HashMap<ProofClause, Integer>();
//		
//		final SimpleDirectedGraph<ProofClause, DefaultEdge> g = 
//			new SimpleDirectedGraph<ProofClause, DefaultEdge>(DefaultEdge.class);
//		g.addVertex(solver.proof());
//			
//			
//		final IntSet visited = new IntBitSet(solver.proof().index()+1);	
//		final Stack<ProofClause> stack = new ArrayStack<ProofClause>();
//		stack.push(solver.proof());
//		while(!stack.empty()) {
//			ProofClause front = stack.pop();
//			if (front.learned()) {
//				for(ProofClause ante : front.antecedents()) {
//					if (visited.add(ante.index())) { // not yet visited
//						stack.push(ante);
//						g.addVertex(ante);
//					}
//					g.addEdge(front, ante);
//				}
//			} else {
//				ret.put(front, null);
//			}
//		}
//		
//		final BellmanFordShortestPath<ProofClause, DefaultEdge> sp = 
//			new BellmanFordShortestPath<ProofClause, DefaultEdge>(g, solver.proof());
//		
//		for(Map.Entry<ProofClause,Integer> e: ret.entrySet()) {
//			e.setValue((int)sp.getCost(e.getKey()));
//		}
//		
//		return ret;
//	}
	
	
	/**
	 * Minimizes the proof of this.formula's unsatisfiability
	 * using a variant of the Complete Resolution Refutation algorithm 
	 * (N. Dershowitz, Z. Hanna, and A. Nadel.  <i>A scalable algorithm for minimal unsatisfiable core
	 * extraction.</i>  In Proceedings of Ninth International Conference on Theory and Applications of 
	 * Satisfiability Testing (SAT '06). 2006.).  The speed of minimization
	 * corresponds, roughly, to the {@link #relativeHardness() relative hardness} of the proof.  In other words,
	 * the higher the relative hardness, the longer the minimization process.
	 * @effects minimizes the proof of this.formula's unsatisfiability
	 * using a variant of the Complete Resolution Refutation algorithm
	 * @see N. Dershowitz, Z. Hanna, and A. Nadel.  <i>A scalable algorithm for minimal unsatisfiable core
	 * extraction.</i>  In Proceedings of Ninth International Conference on Theory and Applications of 
	 * Satisfiability Testing (SAT '06). 2006.
	 */
	public void minimize() {
		
	}
	
	/**
	 * Returns an iterator over the {@link TranslationLog.Record log records} for the formulas
	 * that are in the unsatisfiable core of this.formula.   The record objects returned by the iterator are not 
	 * guaranteed to be immutable.  In particular, the state of a record object
	 * returned by <tt>next()</tt> is guaranteed to remain the same only until the
	 * subsequent call to <tt>next()</tt>.
	 * @return  an iterator over the {@link TranslationLog.Record log records} for the formulas
	 * that are in the unsatisfiable core of this.formula.
	 */
	public Iterator<TranslationLog.Record> core() { 
		return log.replay(coreLiterals());
	}
	
}