package kodkod.engine.fol2sat;

import static kodkod.ast.RelationPredicate.Name.ACYCLIC;
import static kodkod.ast.RelationPredicate.Name.FUNCTION;
import static kodkod.ast.RelationPredicate.Name.TOTAL_ORDERING;

import java.util.EnumMap;
import java.util.IdentityHashMap;
import java.util.Map;
import java.util.Set;

import kodkod.ast.BinaryExpression;
import kodkod.ast.BinaryFormula;
import kodkod.ast.ComparisonFormula;
import kodkod.ast.Comprehension;
import kodkod.ast.Decl;
import kodkod.ast.Decls;
import kodkod.ast.Formula;
import kodkod.ast.IfExpression;
import kodkod.ast.MultiplicityFormula;
import kodkod.ast.Node;
import kodkod.ast.NotFormula;
import kodkod.ast.QuantifiedFormula;
import kodkod.ast.Relation;
import kodkod.ast.RelationPredicate;
import kodkod.ast.UnaryExpression;
import kodkod.ast.visitor.DepthFirstVoidVisitor;
import kodkod.util.IdentityHashSet;

/**
 * Provides utility methods for collecting various 
 * structural information about a given syntax tree / dag.
 * @author Emina Torlak
 */ 
final class NodeAnalyzer {

	private NodeAnalyzer() {}
	
	/**
	 * Returns the relations at the leaves of the given node.
	 * @return Relation & node.*children
	 * @throws NullPointerException - node = null
	 */
	static Set<Relation> detectRelations(Node node) {
		final SharingAndRelationDetector analyzer = new SharingAndRelationDetector();
		node.accept(analyzer);
		return analyzer.relations;
	}
	
	/**
	 * Returns the set of all non-leaf descendents
	 * of the given node that have more than one parent.
	 * @return {n: Node | #(n.~children & node.*children) > 1 }
	 * @throws NullPointerException - node = null
	 */
	static Set<Node> detectSharing(Node node) {
		final SharingAndRelationDetector analyzer = new SharingAndRelationDetector();
		node.accept(analyzer);
		return analyzer.sharedNodes();
	}
	
	/**
	 * Annotates the given formula with the information about
	 * syntactic sharing of internal nodes, leaves, and top-level
	 * predicates.  The results of the analysis can be obtained
	 * by calling {@link FormulaAnnotations#sharedNodes()}, 
	 * {@link FormulaAnnotations#relations()}, and {@link FormulaAnnotations#topLevelPredicates()}.
	 * @return { annotations: FormulaAnnotations | annotations.formula = formula }
	 * @throws NullPointerException - formula = null
	 */
	static FormulaAnnotations annotate(Formula formula) {
		final SharingAndRelationDetector analyzer = new SharingAndRelationDetector();
		formula.accept(analyzer);
		// extract the shared nodes from the sharing status map
		final Set<Node> sharedNodes = analyzer.sharedNodes();
		// reuse the sharing status map
		analyzer.sharingStatus.clear();
		final PredicateDetector detector = new PredicateDetector(sharedNodes, analyzer.sharingStatus);
		formula.accept(detector);
		return new FormulaAnnotations(sharedNodes, analyzer.relations, detector.preds);
	}
	
	/**
	 * Stores the annotations computed by 
	 * {@link NodeAnalyzer#annotate(Formula) NodeAnalyzer.annotate}. 
	 * All the comments assume that implications are interpreted as 
	 * disjunctions:  that is, a => b is interpreted as !a || b.
	 * 
	 * @specfield formula: Formula // the formula being annotated
	 */
	static final class FormulaAnnotations {
		private final Set<Node> sharedNodes;
		private final Set<Relation> relations;
		private final Set<RelationPredicate.Function> functions;
		private final Set<RelationPredicate.TotalOrdering> totals;
		private final Set<RelationPredicate.Acyclic> acyclics;
		
		@SuppressWarnings("unchecked")
		private FormulaAnnotations(Set<Node> sharedNodes, Set<Relation> relations, 
				                  EnumMap<RelationPredicate.Name, ?> preds) {
			this.sharedNodes = sharedNodes;
			this.relations = relations;
			this.functions = (Set<RelationPredicate.Function>) preds.get(RelationPredicate.Name.FUNCTION);
			this.totals = (Set<RelationPredicate.TotalOrdering>) preds.get(RelationPredicate.Name.TOTAL_ORDERING);
			this.acyclics = (Set<RelationPredicate.Acyclic>) preds.get(RelationPredicate.Name.ACYCLIC);
		}
		
		/**
		 * Returns the set of all non-leaf descendents
		 * of this.formulaa that have more than one parent.
		 * @return {n: Node | #(n.~children & formula.*children) > 1 }
		 */
		Set<Node> sharedNodes() {
			return sharedNodes;
		}
		
		/**
		 * Returns the relations that are leaves of this.formula.
		 * @return Relation & this.formula.^children
		 */
		Set<Relation> relations() {
			return relations;
		}
	
		/**
		 * Returns all top-level RelationPredicate.Functions in this.formula.
		 * Top-level predicates are those that are (semantically) components
		 * of the top-loevel conjunction.
		 * @return {pred: this.formula.*children & RelationPredicate.Function | 
		 *            some path: children | pred in this.formula.*path &&
		 *               no QuantifiedFormula & this.formula.*path && 
		 *               #{this.formula.*path & NotFormula} % 2 = 0}
		 */
		Set<RelationPredicate.Function> topLevelFunctions() {
			return functions;
		}
		
		/**
		 * Returns all top-level RelationPredicate.Acyclics in this.formula.
		 * Top-level predicates are those that are (semantically) components
		 * of the top-loevel conjunction.
		 * @return {pred: this.formula.*children & RelationPredicate.Acyclic | 
		 *            some path: children | pred in this.formula.*path &&
		 *               #{this.formula.*path & NotFormula} % 2 = 0}
		 */
		Set<RelationPredicate.Acyclic> topLevelAcyclics() {
			return acyclics;
		}
		
		/**
		 * Returns all top-level RelationPredicate.TotalOrderings in this.formula.
		 * Top-level predicates are those that are (semantically) components
		 * of the top-loevel conjunction.
		 * @return {pred: this.formula.*children & RelationPredicate.TotalOrdering | 
		 *            some path: children | pred in this.formula.*path &&
		 *               #{this.formula.*path & NotFormula} % 2 = 0}
		 */
		Set<RelationPredicate.TotalOrdering> topLevelOrders() {
			return totals;
		}
		
	}

	/**
	 * Detects shared non-leaf descendents of a given node,
	 * as well as the relations at its leaves.
	 * 
	 * @specfield node: Node // node to which the analyzer is applied
	 */
	private static final class SharingAndRelationDetector extends DepthFirstVoidVisitor {
		/* contains the relations at the leaves of the analyzed node */
		IdentityHashSet<Relation> relations = new IdentityHashSet<Relation>();
		/* maps each internal node with more than one parent to TRUE and all
		 * other internal nodes to FALSE */
		IdentityHashMap<Node,Boolean> sharingStatus = new IdentityHashMap<Node,Boolean>();
		/* @invariant numShareNodes = #sharingStatus.TRUE */
		int numSharedNodes;
				
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
		private final boolean visited(Node node) {
			Boolean status = sharingStatus.get(node);
			if (status != Boolean.TRUE) {
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
		
		public void visit(Relation relation) {
			relations.add(relation);
		}
			
		public void visit(Decls decls) {
			if (!visited(decls))
				super.visit(decls);
		}
		
		public void visit(Decl decl) {
			if (!visited(decl)) 
				super.visit(decl);
		}
	
		public void visit(BinaryExpression binExpr) {
			if (!visited(binExpr)) 
				super.visit(binExpr);
		}
		
		public void visit(UnaryExpression unaryExpr) {
			if (!visited(unaryExpr)) 
				super.visit(unaryExpr);
		}
		
		public void visit(Comprehension comprehension) {
			if (!visited(comprehension))
				super.visit(comprehension);
		}
		
		public void visit(IfExpression ifExpr) {
			if (!visited(ifExpr))
				super.visit(ifExpr);
		}
		
		public void visit(QuantifiedFormula quantFormula) {
			if (!visited(quantFormula))
				super.visit(quantFormula);
		}
		
		public void visit(BinaryFormula binFormula) {
			if (!visited(binFormula))
				super.visit(binFormula);
		}
		
		public void visit(NotFormula not) {
			if (!visited(not))
				super.visit(not);
		}
		
		public void visit(ComparisonFormula compFormula) {
			if (!visited(compFormula))
				super.visit(compFormula);
		}
		
		public void visit(MultiplicityFormula multFormula) {
			if (!visited(multFormula))
				super.visit(multFormula);
		}
		
		public void visit(RelationPredicate pred) {
			if (!visited(pred))
				super.visit(pred);
		}
	}
	
	/*---------------RELATION PREDICATE DETECTION----------------------*/
	/**
	 * Collects top-level relation predicates in a formula.
	 * The top-level predicates are those that are (semantically)
	 * components of the top-level conjunction.
	 */
	private static final class PredicateDetector extends DepthFirstVoidVisitor {
		final Set<Node> sharedNodes;
		
		/* @invariant for each node n in sharedNodes, if n has not been visited,
		 * visitedNodes has no mapping for it.  If n has been visited and during each
		 * visit the value of the negated flag was b (b in {TRUE, FALSE}), then visitedNodes
		 * maps n to b.  If n has been visited and the negated flag had different values
		 * during visits, then visitedNodes maps n to null. 
		 */
		final Map<Node, Boolean> visitedNodes;
		
		/* contains the top-level predicated collected so far */
		final EnumMap<RelationPredicate.Name, Set<RelationPredicate>> preds;
		
		boolean negated, quantified;
	
		/**
		 * Constructs a new predicate detector that will use the sharing
		 * information in the given set.  The provided map is used to 
		 * keep track of visited nodes; the map is required to be empty.
		 */
		PredicateDetector(Set<Node> sharedNodes, Map<Node,Boolean> visitedNodes) {
			this.negated = this.quantified = false;
			this.sharedNodes = sharedNodes;
			assert visitedNodes.size() == 0;
			this.visitedNodes = visitedNodes; //new IdentityHashMap<Node, Boolean>(sharedNodes.size());
			this.preds = new EnumMap<RelationPredicate.Name, Set<RelationPredicate>>(RelationPredicate.Name.class);	
			preds.put(ACYCLIC, new IdentityHashSet<RelationPredicate>(4));
			preds.put(TOTAL_ORDERING, new IdentityHashSet<RelationPredicate>(4));
			preds.put(FUNCTION, new IdentityHashSet<RelationPredicate>(8));
		}
		
		/**
		 * Returns true if the given node has already been visited.
		 * Otherwise returns false.
		 */
		private boolean visited(Node node) {
			if (sharedNodes.contains(node)) {
				if (visitedNodes.containsKey(node)) {
					final Boolean visitValue = visitedNodes.get(node);
					if (visitValue==null || visitValue.booleanValue()==negated)
						return true;
					visitedNodes.put(node, null);
				} else {
					visitedNodes.put(node, negated ? Boolean.TRUE : Boolean.FALSE);
				}
			}
			return false;
		}
		
		public void visit(QuantifiedFormula quantFormula) {
			if (!visited(quantFormula)) {
				final boolean oldQuant = quantified;
				quantified = true;
				quantFormula.formula().accept(this);
				quantified = oldQuant;
			}
		}
		
		public void visit(BinaryFormula binFormula) {
			if (!visited(binFormula)) {
				switch(binFormula.op()) {
				case AND 	:  if (!negated) super.visit(binFormula); break;
				case OR  	:  if (negated) super.visit(binFormula); break; // !(a || b) = !a && !b
				case IMPLIES	: // !(a => b) = !(!a || b) = a && !b
					if (negated) { 
						negated = false;
						binFormula.left().accept(this);
						negated = true;
						binFormula.right().accept(this);
					}
					break;
				case IFF : break; // a <=> b = (!a || b) && (!b || a) ... nothing to do			
				default :
					throw new IllegalArgumentException("unknown operator: " + binFormula.op());
				}
			}
		}
		
		public void visit(NotFormula not) {
			if (!visited(not)) {
				negated = !negated;
				not.formula().accept(this);
				negated = !negated;
			}
		}
		
		public void visit(ComparisonFormula compFormula) {
			// nothing to do
		}
		
		public void visit(MultiplicityFormula multFormula) {
			// nothing to do
		}
		
		public void visit(RelationPredicate pred) {
			if (!negated && !visited(pred)) {
				if (pred.name() != RelationPredicate.Name.FUNCTION || !quantified) {
					preds.get(pred.name()).add(pred);
				}
			}
		}
	}
}
