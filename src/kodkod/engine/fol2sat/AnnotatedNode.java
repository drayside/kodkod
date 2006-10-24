package kodkod.engine.fol2sat;

import static kodkod.ast.BinaryFormula.Operator.AND;
import static kodkod.ast.BinaryFormula.Operator.IMPLIES;
import static kodkod.ast.BinaryFormula.Operator.OR;
import static kodkod.ast.RelationPredicate.Name.ACYCLIC;
import static kodkod.ast.RelationPredicate.Name.FUNCTION;
import static kodkod.ast.RelationPredicate.Name.TOTAL_ORDERING;

import java.util.Collections;
import java.util.EnumMap;
import java.util.IdentityHashMap;
import java.util.Map;
import java.util.Set;

import kodkod.ast.BinaryFormula;
import kodkod.ast.ComparisonFormula;
import kodkod.ast.ConstantExpression;
import kodkod.ast.ExprToIntCast;
import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.IntComparisonFormula;
import kodkod.ast.IntToExprCast;
import kodkod.ast.MultiplicityFormula;
import kodkod.ast.Node;
import kodkod.ast.NotFormula;
import kodkod.ast.QuantifiedFormula;
import kodkod.ast.Relation;
import kodkod.ast.RelationPredicate;
import kodkod.ast.visitor.DepthFirstDetector;
import kodkod.ast.visitor.DepthFirstVoidVisitor;
import kodkod.util.collections.IdentityHashSet;

/**
 * A node annotated with information about
 * structural sharing in its ast/dag.  The class
 * also provides utility methods for collecting
 * various information about annotated nodes.
 * 
 * @specfield node: N // annotated node
 * @author Emina Torlak
 */ 
final class AnnotatedNode<N extends Node> {
	private final N node;
	private final Set<Node> sharedNodes;
	
	
	/**
	 * Constructs a new annotator for the given node.
	 * @effects this.node' = node
	 */
	AnnotatedNode(N node) {
		this.node = node;
		final SharingDetector detector = new SharingDetector();
		node.accept(detector);
		this.sharedNodes = Collections.unmodifiableSet(detector.sharedNodes());
	}

	/**
	 * Constructs a new annotator for the given node.
	 * @requires sharedNodes = {n: Node | some n.children && #(n.~children & node.*children) > 1 }
	 * @effects this.node' = node
	 */
	AnnotatedNode(N node, Set<Node> sharedNodes) {
		this.node = node;
		this.sharedNodes = sharedNodes;
	}
	
	/**
	 * Returns this.node.
	 * @return this.node
	 */
	final N node() {
		return node;
	}
	
	/**
	 * Returns the set of all non-leaf descendents
	 * of this.node that have more than one parent.
	 * @return {n: Node | some n.children && #(n.~children & this.node.*children) > 1 }
	 */
	final Set<Node> sharedNodes() { 
		return sharedNodes;
	}
	
	/**
	 * Returns the set of all relations at the leaves of the given annotated node.
	 * @return Relation & annotated.node.*children
	 */
	static Set<Relation> relations(final AnnotatedNode<? extends Node> annotated) {
		final Set<Relation> relations = new IdentityHashSet<Relation>();
		final DepthFirstVoidVisitor visitor = new DepthFirstVoidVisitor() {
			private final Set<Node> shared = annotated.sharedNodes;
			private final Set<Node> visited = new IdentityHashSet<Node>(shared.size());
			protected boolean visited(Node n) {
				return shared.contains(n) && !visited.add(n);
			}
			public void visit(Relation relation) {
				relations.add(relation);
			}
		};
		annotated.node.accept(visitor);
		return relations;
	}
	
	/**
	 * Returns true if the given node contains a child whose meaning depends on 
	 * integer bounds (i.e. an ExprToIntCast node with SUM operator or an IntToExprCast node).
	 * @return true if the given node contains a child whose meaning depends on 
	 * integer bounds (i.e. an ExprToIntCast node with SUM operator or an IntToExprCast node).
	 */
	static boolean usesIntBounds(final AnnotatedNode<? extends Node> annotated) {
		final DepthFirstDetector detector = new DepthFirstDetector(annotated.sharedNodes) {
			public Boolean visit(IntToExprCast expr) {
				return Boolean.TRUE;
			}
			public Boolean visit(ExprToIntCast intExpr) {
				if (intExpr.op()==ExprToIntCast.Operator.CARDINALITY)
					super.visit(intExpr);
				return Boolean.TRUE;
			}
			public Boolean visit(ConstantExpression expr) {
				return expr==Expression.INTS ? Boolean.TRUE : Boolean.FALSE;
			}
		};
		return (Boolean)annotated.node.accept(detector);
	}
	
	/**
	 * Returns a map of RelationPredicate names to sets of top-level relation predicates with
	 * the corresponding names in the given annotated formula.
	 * @return a map of RelationPredicate names to sets of top-level relation predicates with
	 * the corresponding names in the annotated formula.  A predicate is considered 'top-level'  
	 * if it is a component of the top-level conjunction, if any, of annotated.node.  
	 */
	static Map<RelationPredicate.Name, Set<RelationPredicate>> predicates(AnnotatedNode<Formula> annotated) {
		final PredicateCollector collector = new PredicateCollector(annotated.sharedNodes);
		annotated.node.accept(collector);
		return collector.preds;
	}
	
	/**
	 * Detects shared non-leaf descendents of a given node.
	 * 
	 * @specfield node: Node // node to which the analyzer is applied
	 */
	private static final class SharingDetector extends DepthFirstVoidVisitor {
		/* maps each internal node with more than one parent to TRUE and all
		 * other internal nodes to FALSE */
		final IdentityHashMap<Node,Boolean> sharingStatus;
		/* @invariant numShareNodes = #sharingStatus.TRUE */
		int numSharedNodes;
		
		SharingDetector() {
			sharingStatus = new IdentityHashMap<Node,Boolean>();
		}
		
		/**
		 * Returns the shared internal nodes of this.node.  This method should
		 * be called only after this visitor has been applied to this.node.
		 * @return {n: Node | #(n.~children & node.*children) > 1 }
		 */
		IdentityHashSet<Node> sharedNodes() {
			final IdentityHashSet<Node> shared = new IdentityHashSet<Node>(numSharedNodes);
			for(Map.Entry<Node,Boolean> entry : sharingStatus.entrySet()) {
				if (entry.getValue()==Boolean.TRUE)
					shared.add(entry.getKey());
			}
			return shared;
		}
		
		/**
		 * Records the visit to the given node in the status map.
		 * If the node has not been visited before, it is mapped
		 * to Boolean.FALSE and false is returned.  Otherwise, 
		 * it is mapped to Boolean.TRUE and true is returned.
		 * The first time a Node is mapped to true, numSharedNodes
		 * is incremented by one.
		 * @effects no this.shared[node] => this.shared' = this.shared + node->FALSE,
		 *          this.shared[node] = FALSE => this.shared' = this.shared + node->TRUE,
		 *          this.shared' = this.shared
		 * @return this.shared'[node]
		 */
		protected final boolean visited(Node node) {
			Boolean status = sharingStatus.get(node);
			if (!Boolean.TRUE.equals(status)) {
				if (status==null) {
					status = Boolean.FALSE;
				} else { // status == Boolean.FALSE
					status = Boolean.TRUE;
					numSharedNodes++;
				}
				sharingStatus.put(node,status);
			}
			return status;
		}
	}

	/**
	 * A visitor that detects and collects
	 * top-level relation predicates; i.e. predicates that
	 * are components in the top-level conjunction, if any, on ANY
	 * path starting at the root formula.
	 */
	private static final class PredicateCollector extends DepthFirstVoidVisitor {
		protected boolean negated;
		private final Set<Node> sharedNodes;
		/* if a given node is not mapped at all, it means that it has not been visited;
		 * if it is mapped to FALSE, it has been visited with negated=FALSE, 
		 * if it is mapped to TRUE, it has been visited with negated=TRUE,
		 * if it is mapped to null, it has been visited with both values of negated. */
		private final Map<Node,Boolean> visited;	
		/* holds the top level predicates at the the end of the visit*/
		final EnumMap<RelationPredicate.Name, Set<RelationPredicate>> preds;
		/**
		 * Constructs a new collector.
		 * @effects this.negated' = false
		 */
		PredicateCollector(Set<Node> sharedNodes) {
			this.sharedNodes = sharedNodes;
			this.visited = new IdentityHashMap<Node,Boolean>();
			this.negated = false;
			preds = new EnumMap<RelationPredicate.Name, Set<RelationPredicate>>(RelationPredicate.Name.class);	
			preds.put(ACYCLIC, new IdentityHashSet<RelationPredicate>(4));
			preds.put(TOTAL_ORDERING, new IdentityHashSet<RelationPredicate>(4));
			preds.put(FUNCTION, new IdentityHashSet<RelationPredicate>(8));
		}
		/**
		 * Returns true if n has already been visited with the current value of the
		 * negated flag; otherwise returns false.
		 * @effects records that n is being visited with the current value of the negated flag
		 * @return true if n has already been visited with the current value of the
		 * negated flag; otherwise returns false.
		 */
		@Override
		protected final boolean visited(Node n) {
			if (sharedNodes.contains(n)) {
				if (!visited.containsKey(n)) { // first visit
					visited.put(n, Boolean.valueOf(negated));
					return false;
				} else {
					final Boolean visit = visited.get(n);
					if (visit==null || visit==negated) { // already visited with same negated value
						return true; 
					} else { // already visited with different negated value
						visited.put(n, null);
						return false;
					}
				}
			}
			return false;
		}
		/**
		 * Calls visited(intComp); intComp's children are not top-level formulas
		 * so they are not visited.
		 */
		public void visit(IntComparisonFormula intComp) {
			visited(intComp);
		}
		/**
		 * Calls visited(quantFormula); quantFormula's children are not top-level formulas
		 * so they are not visited.
		 */
		public void visit(QuantifiedFormula quantFormula) {
			visited(quantFormula);
		}
		/**
		 * Visits the children of the given formula if it has not been visited already with
		 * the given value of the negated flag and if binFormula.op==IMPLIES && negated or
		 * binFormula.op==AND && !negated or binFormula.op==OR && negated.  Otherwise does nothing.
		 * @see kodkod.ast.visitor.DepthFirstVoidVisitor#visit(kodkod.ast.BinaryFormula)
		 */
		public void visit(BinaryFormula binFormula) {
			if (!visited(binFormula)) {
				final BinaryFormula.Operator op = binFormula.op();
			
				if ((!negated && op==AND) || (negated && op==OR)) { // op==AND || op==OR
					binFormula.left().accept(this);
					binFormula.right().accept(this);
				} else if (negated && op==IMPLIES) { // !(a => b) = !(!a || b) = a && !b
					negated = !negated;
					binFormula.left().accept(this);
					negated = !negated;
					binFormula.right().accept(this);
				} 
			}
		}
		/**
		 * Visits the children of the child of the child formula, with
		 * the negation of the current value of the negated flag, 
		 * if it has not already been visited 
		 * with the current value of this.negated; otherwise does nothing.
		 */
		public void visit(NotFormula not) {
			if (!visited(not)) {
				negated = !negated;
				not.formula().accept(this);
				negated = !negated;
			}
			
		}
		/**
		 * Calls visited(compFormula); compFormula's children are not top-level formulas
		 * so they are not visited.
		 */
		public void visit(ComparisonFormula compFormula) {
			visited(compFormula);
		}
		/**
		 * Calls visited(multFormula); multFormula's child is not top-level formulas
		 * so it is not visited.
		 */
		public void visit(MultiplicityFormula multFormula) {
			visited(multFormula);
		}
		/**
		 * Records the visit to this predicate if it is not negated.
		 */
		public void visit(RelationPredicate pred) {
			if (!visited(pred)) {
				if (!negated) {
					preds.get(pred.name()).add(pred);
				}
			}
		}
	}
}
