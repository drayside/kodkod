package kodkod.engine;

import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import kodkod.ast.BinaryFormula;
import kodkod.ast.ComparisonFormula;
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
import kodkod.util.collections.IdentityHashSet;

/**
 * A proof of unsatisfiability for a trivially unsatisfiable formula.
 * A formula is considered trivally unsatisfiable if its unsatisfiability
 * is discovered through translation alone.
 *  
 * @author Emina Torlak
 */
final class TrivialProof extends Proof {
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
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.Proof#core()
	 */
	public final Iterator<TranslationRecord> core() { 
		if (coreFilter==null) {
			coreFilter = new RecordFilter() {
				final Set<Node> core = NodePruner.relevantNodes(log());
				public boolean accept(Node node, int literal, Map<Variable, TupleSet> env) {
					return core.contains(node) && (StrictMath.abs(literal)==Integer.MAX_VALUE) && env.isEmpty();
				}
			};
		}
		return log().replay(coreFilter); 
	}

	/**
	 * @throws UnsupportedOperationException
	 * @see kodkod.engine.Proof#minimize(kodkod.engine.satlab.ReductionStrategy)
	 */
	@Override
	public void minimize(ReductionStrategy strategy) {	throw new UnsupportedOperationException(); }

	/**
	 * Given a translation log for a trivially unsatisfiable formula, finds the nodes 
	 * necessary for proving the formula's unsastisfiability.  Instances of this
	 * visitor should be constructed and applied using the {@linkplain #relevantNodes(TranslationLog)}
	 * 
	 * @specfield log: TranslationLog
	 * @author Emina Torlak
	 */
	private static final class NodePruner extends AbstractVoidVisitor {
		private final Set<Node> trueNodes, falseNodes, visited, relevant;
		
		/**
		 * Constructs a proof finder for the given log.
		 * @effects this.log' = log
		 */
		NodePruner(TranslationLog log) {
			visited = new IdentityHashSet<Node>();
			relevant = new IdentityHashSet<Node>();
			
			trueNodes = new IdentityHashSet<Node>();
			falseNodes = new IdentityHashSet<Node>();
			
			final RecordFilter filter = new RecordFilter() {
				public boolean accept(Node node, int literal, Map<Variable, TupleSet> env) {
					return env.isEmpty() && (StrictMath.abs(literal) == Integer.MAX_VALUE );
				}	
			};
			
			for(Iterator<TranslationRecord> iter = log.replay(filter); iter.hasNext();) {
				TranslationRecord rec = iter.next();
				if (rec.literal()>0) 	trueNodes.add(rec.node());
				else					falseNodes.add(rec.node());
			}
			
			if (!falseNodes.contains(log.formula()))
				throw new AssertionError("trivially unsatisfiable formula not logged as FALSE");
		}
		
		/**
		 * Returns the nodes necessary for proving the trivial unsatisfiability of log.formula.
		 * @requires some r: log.records | r.node = log.formula && r.literal = BooleanConstant.FALSE.label
		 * @return nodes necessary for proving the trivial unsatisfiability of log.formula.
		 */
		static Set<Node> relevantNodes(TranslationLog log) {
			final NodePruner finder = new NodePruner(log);
			log.formula().accept(finder);
			return finder.relevant;
		}
		
		/**
		 * Returns true if the given node has been visited before or if 
		 * it was not simplified to a constant during translation (since 
		 * that means it couldn't have affected unsatisfiability so we don't
		 * want to visit it). 
		 * @effects this.visited' = this.visited + n
		 * @return n in this.visited || !isConstant(n)
		 */
		@Override
		protected boolean visited(Node n) {
			return !(visited.add(n) && isConstant(n));
		}
		
		/**
		 * Returns true if the node was simplified to TRUE during translation.
		 * @return some r: this.log.records | r.node = node && no r.env && r.literal = BooleanConstant.TRUE.label
		 */
		final boolean isTrue(Node node) { return trueNodes.contains(node); }
		
		/**
		 * Returns true if the node was simplified to FALSE during translation.
		 * @return some r: this.log.records | r.node = node && no r.env && r.literal = BooleanConstant.FALSE.label
		 */
		final boolean isFalse(Node node) { return falseNodes.contains(node); }
		
		/**
		 * Returns true if the node was simplified to a constant during translation. 
		 * @return isFalse(node) or isTrue(node)
		 */
		final boolean isConstant(Node node) { return isFalse(node) || isTrue(node);	}
		
		/* The following methods treat their arguments as "elementary" formulas; that is, they don't visit the
		 * their argument's children to prune away any potentially irrelevant subformulas.  
		 * Each of the following methods simply adds its argument to this.relevant if it has not been 
		 * {@linkplain #visited(Node)}.  */
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
			
			boolean binValue = isTrue(binFormula); 
			final Formula l = binFormula.left(), r = binFormula.right();
		
			switch(binFormula.op()) {
			case AND : 
				if (binValue || isFalse(l)) { l.accept(this); }
				if (binValue || isFalse(r)) { r.accept(this); }
				break;
			case OR : 
				if (!binValue || isTrue(l)) { l.accept(this); }
				if (!binValue || isTrue(r)) { r.accept(this); }
				break;
			case IMPLIES: // !l || r
				if (!binValue || isFalse(l)) { l.accept(this); }  
				if (!binValue || isTrue(r))  { r.accept(this); }
				break;
			case IFF: // (!l || r) && (l || !r) 
				l.accept(this);
				r.accept(this);
				break;
			default :
				throw new IllegalArgumentException("Unknown operator: " + binFormula.op());
			}	
		}
	}
	
}