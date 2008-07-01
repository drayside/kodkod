package kodkod.engine;

import java.util.AbstractSet;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import kodkod.ast.BinaryFormula;
import kodkod.ast.ComparisonFormula;
import kodkod.ast.ConstantFormula;
import kodkod.ast.Decl;
import kodkod.ast.Formula;
import kodkod.ast.IntComparisonFormula;
import kodkod.ast.MultiplicityFormula;
import kodkod.ast.Node;
import kodkod.ast.NotFormula;
import kodkod.ast.QuantifiedFormula;
import kodkod.ast.RelationPredicate;
import kodkod.ast.Variable;
import kodkod.ast.visitor.AbstractVoidVisitor;
import kodkod.engine.fol2sat.RecordFilter;
import kodkod.engine.fol2sat.TranslationLog;
import kodkod.engine.fol2sat.TranslationRecord;
import kodkod.engine.satlab.ReductionStrategy;
import kodkod.instance.TupleSet;
import kodkod.util.collections.Containers;
import kodkod.util.collections.IdentityHashSet;
import kodkod.util.ints.SparseSequence;
import kodkod.util.ints.TreeSequence;

/**
 * A proof of unsatisfiability for a trivially unsatisfiable formula.
 * A formula is considered trivally unsatisfiable if its unsatisfiability
 * is discovered through translation alone.
 *  
 * @author Emina Torlak
 */
final class TrivialProof extends Proof {
	private Set<Formula> coreRoots;
	private RecordFilter coreFilter;
	
	/**
	 * Constructs a proof of unsatisfiability for the trivially unsatisfiable
	 * formula whose translation is recorded in the given log.
	 * @requires log != null
	 * @effects this.formula' = log.formula
	 */
	TrivialProof(TranslationLog log) {
		super(log);
		this.coreFilter = null;
		this.coreRoots = null;
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.Proof#core()
	 */
	public final Iterator<TranslationRecord> core() { 
		if (coreFilter==null) {
			coreFilter = new RecordFilter() {
				final Set<Node> coreNodes = NodePruner.relevantNodes(log(),  coreRoots==null ? log().roots() : coreRoots);
				public boolean accept(Node node, int literal, Map<Variable, TupleSet> env) {
					return coreNodes.contains(node) ;
				}
			};
		}
		return log().replay(coreFilter); 
	}

	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.Proof#highLevelCore()
	 */
	public final Set<Formula> highLevelCore() {
		if (coreRoots==null) { 
			final Iterator<TranslationRecord> iter = core();
			final Set<Formula> roots = log().roots();
			coreRoots = new LinkedHashSet<Formula>();
			while( iter.hasNext() ) {
				Node next = iter.next().node();
				if (roots.contains(next))
					coreRoots.add((Formula)next);
			}
			coreRoots = Collections.unmodifiableSet(coreRoots);
		}
		return coreRoots;
	}
	
	/**
	 * Minimizes the current core using the trivial strategy
	 * that does one of the following: (1) if there is a 
	 * root that simplified to FALSE, sets the minimal core
	 * to that root; or (2) if not, there must be two
	 * roots that translated to x and -x, where x is a boolean 
	 * literal, so we pick those two as the minimal core.
	 * The strategy argument is ignored (it can be null).
	 * @see Proof#minimize(ReductionStrategy)
	 */
	@Override
	public void minimize(ReductionStrategy strategy) {	
		final Map<Formula, int[]> outputs = new LinkedHashMap<Formula,int[]>();
		final Set<Formula> roots = log().roots();
		
		for(Iterator<TranslationRecord> iter = core(); iter.hasNext();) { 
			TranslationRecord rec = iter.next();
			if (roots.contains(rec.node())) { 
				// simply record the most recent output value for each formula:
				// this is guaranteed to be the final output value for that 
				// formula because of the translation log guarantee that the
				// log is replayed in the order of translation:  i.e. a child's
				// output value is always recorded before the parent's
				int[] val = outputs.get(rec.node());
				if (val==null) { 
					val = new int[1]; 
					outputs.put((Formula)rec.node(), val);
				}
				val[0] = rec.literal();
			}
		}
		
		final SparseSequence<Collection<Formula>> lits = new TreeSequence<Collection<Formula>>();
		for(Map.Entry<Formula,int[]> entry : outputs.entrySet()) { 
			final int lit = entry.getValue()[0];
			if (lit==-Integer.MAX_VALUE) { 
				coreRoots = Collections.singleton(entry.getKey());
				break;
			}
			if (lits.containsIndex(-lit)) { 
				final Formula f0 = lits.get(-lit).iterator().next();
				final Formula f1 = entry.getKey();
				coreRoots = new AbstractSet<Formula>() {					
					public Iterator<Formula> iterator() { return Containers.iterate(f0, f1); }
					public int size() { return 2; }
				};
				break;
			} else {
				Collection<Formula> litFormulas = lits.get(lit);
				if (litFormulas==null) { 
					litFormulas = new ArrayList<Formula>(2);
					lits.put(lit, litFormulas);
				}
				litFormulas.add(entry.getKey());
			}
		}
		
		coreFilter = null;
		assert coreRoots.size()==1 && outputs.get(coreRoots.iterator().next())[0]==-Integer.MAX_VALUE || coreRoots.size()==2;
	}

	/**
	 * Given a translation log for a trivially unsatisfiable formula, finds the nodes 
	 * necessary for proving the formula's unsatisfiability.  Instances of this
	 * visitor should be constructed and applied using the {@linkplain #relevantNodes(TranslationLog)}
	 * 
	 * @specfield log: TranslationLog
	 * @author Emina Torlak
	 */
	private static final class NodePruner extends AbstractVoidVisitor {
		private final Set<Node> visited, relevant;
		private final Map<Node,Boolean> constNodes;
		
		/**
		 * Constructs a proof finder for the given log.
		 * @effects this.log' = log
		 */
		@SuppressWarnings("unchecked")
		NodePruner(TranslationLog log) {
			visited = new IdentityHashSet<Node>();
			relevant = new IdentityHashSet<Node>();
						
			final RecordFilter filter = new RecordFilter() {
				public boolean accept(Node node, int literal, Map<Variable, TupleSet> env) {
					return env.isEmpty();
				}	
			};
			
			constNodes = new LinkedHashMap<Node,Boolean>();
			for(Iterator<TranslationRecord> itr = log.replay(filter); itr.hasNext(); ) { 
				TranslationRecord rec = itr.next();
				int lit = rec.literal();
				if (Math.abs(lit) != Integer.MAX_VALUE) { 
					constNodes.remove(rec.node());
				} else if (lit==Integer.MAX_VALUE) { 
					constNodes.put(rec.node(), Boolean.TRUE);
				} else {
					constNodes.put(rec.node(), Boolean.FALSE);
				}
			}
		}
		
		/**
		 * Returns the nodes necessary for proving the trivial unsatisfiability of log.formula.
		 * @requires some r: log.records | r.node = log.formula && r.literal = BooleanConstant.FALSE.label
		 * @requires highLevelCore in log.roots() and unsatisfiable(highLevelCore, log.bounds, log.options)
		 * @return nodes necessary for proving the trivial unsatisfiability of log.formula.
		 */
		static Set<Node> relevantNodes(TranslationLog log, Set<Formula> highLevelCore) {
			final NodePruner finder = new NodePruner(log);
			for(Formula root : highLevelCore) {
				if (!finder.isTrue(root)) {
					root.accept(finder);		
				}
			}
			return finder.relevant;
		}
		
		/**
		 * Returns true if the given node has been visited before.
		 * @effects this.visited' = this.visited + n
		 * @return n in this.visited 
		 */
		@Override
		protected boolean visited(Node n) {
			return !visited.add(n);
		}
		
		/**
		 * Returns true if the node was simplified to TRUE during translation.
		 * @return some r: this.log.records | r.node = node && no r.env && r.literal = BooleanConstant.TRUE.label
		 */
		final boolean isTrue(Node node) { return constNodes.get(node)==Boolean.TRUE; }
		
		/**
		 * Returns true if the node was simplified to FALSE during translation.
		 * @return some r: this.log.records | r.node = node && no r.env && r.literal = BooleanConstant.FALSE.label
		 */
		final boolean isFalse(Node node) { return constNodes.get(node)==Boolean.FALSE; }
		
		/**
		 * Returns true if the node was simplified to a constant during translation. 
		 * @return isFalse(node) or isTrue(node)
		 */
		final boolean isConstant(Node node) { return constNodes.containsKey(node);	}
		
		public void visit(Decl decl) { 
			if (visited(decl)) return;
			relevant.add(decl);
		}
		
		public void visit(QuantifiedFormula quantFormula) { 
			if (visited(quantFormula)) return;
			relevant.add(quantFormula);
		}
		
		public void visit(ComparisonFormula compFormula) { 
			if (visited(compFormula)) return;
			relevant.add(compFormula); 	
		}
		public void visit(MultiplicityFormula multFormula) { 
			if (visited(multFormula)) return;
			relevant.add(multFormula);	
		}
		public void visit(RelationPredicate pred) { 
			if (visited(pred)) return;
			relevant.add(pred); 	
		}
		public void visit(IntComparisonFormula intComp) { 
			if (visited(intComp)) return;
			relevant.add(intComp); 	
		}
		
		public void visit(ConstantFormula formula) { 
			relevant.add(formula);
		}
		
		/**
		 * If the argument node has been been visited, adds it to this.relevant and visits its child.
		 */
		public void visit(NotFormula not) {
			if (visited(not)) return; 
			relevant.add(not);
			not.formula().accept(this);
		}
		
	
		/**
		 * If this formula should be visited, then we visit its children only
		 * if they could have contributed to the unsatisfiability of the top-level
		 * formula.  For example, let binFormula = "p && q", binFormula simplified
		 * to FALSE, p simplified to FALSE and q was not simplified, then only p 
		 * should be visited since p caused binFormula's reduction to FALSE.
		 */
		public void visit(BinaryFormula binFormula) {
			if (visited(binFormula)) return;
			relevant.add(binFormula);
			
			final Formula l = binFormula.left(), r = binFormula.right();
			final boolean ltrue = isTrue(l), lfalse = isFalse(l);
			final boolean rtrue = isTrue(r), rfalse = isFalse(r);
			
			boolean lrelevant = true, rrelevant = true;
			
			switch(binFormula.op()) {
			case AND : 
				if (isFalse(binFormula)) {
					lrelevant = !ltrue && (lfalse || !rfalse);
					rrelevant = !rtrue && (rfalse || !lfalse);
				} else if (!isTrue(binFormula)) {
					lrelevant = !ltrue;
					rrelevant = !rtrue;
				}
				break;
			case OR :
				if (isTrue(binFormula)) {
					lrelevant = !lfalse && (ltrue || !rtrue);
					rrelevant = !rfalse && (rtrue || !ltrue);
				} else if (!isFalse(binFormula)) {
					lrelevant = !lfalse;
					rrelevant = !rfalse;
				}
				break;
			case IMPLIES: // !l || r
				if (isTrue(binFormula)) {
					lrelevant = !ltrue && (lfalse || !rtrue);
					rrelevant = !rfalse && (rtrue || !lfalse);
				} else if (!isFalse(binFormula)) {
					lrelevant = !ltrue;
					rrelevant = !rfalse;
				}
				break;
			case IFF: // (!l || r) && (l || !r) 
				break;
			default :
				throw new IllegalArgumentException("Unknown operator: " + binFormula.op());
			}	
			
			if (lrelevant) { l.accept(this); }
			if (rrelevant) { r.accept(this); }
		}
	}
	
}