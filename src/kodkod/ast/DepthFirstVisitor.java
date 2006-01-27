/*
 * DepthFirstVisitor.java
 * Created on May 20, 2005
 */
package kodkod.ast;



/** 
 * Implements the depth first traversal of the AST.  Each node is
 * visited once for each parent.  That is, if a node has exactly one
 * parent, it is visited exactly once.  If it is status by two parents,
 * it is visited twice, following each of the parents' child pointers.
 *
 * @author Emina Torlak 
 */
public abstract class DepthFirstVisitor<E, F, D> implements Visitor<E, F, D> {
	
	protected DepthFirstVisitor() { }
	
	/**
	 * Visits all the children of the given declarations node, and returns null.
	 * @effects all d: declarations.declarations | d.variable.accept(this) && d.expression.accept(this)
	 * @return null
	 */
	public D visit(Decls decls) {
		for (Decl decl : decls) {
			decl.accept(this);
		}
		return null; 
	}
	
	/**
	 * Visits the variable and expression of this decl and returns null.
	 * @effects decl.variable.accept(this) && decl.expression.accept(this)
	 * @return null
	 */
	public D visit(Decl decl) {
		decl.variable().accept(this);
		decl.expression().accept(this);
		return null; 
	}
	
	
	/**
	 * Returns null.
	 * @return null
	 */
	public E visit(Relation relation) {
		return null;
	}
	
	/**
	 * Returns null.
	 * @return null
	 */
	public E visit(Variable variable) {
		return null;
	}
	
	/**
	 * Returns null.
	 * @return null
	 */
	public E visit(ConstantExpression constExpr) {
		return null;
	}
	
	/**
	 * Visits the left and right subexpressions, and returns null
	 * @effects binExpr.left.accept(this) && binExpr.right.accept(this)
	 * @return null
	 */
	public E visit(BinaryExpression binExpr) {
		binExpr.left().accept(this);
		binExpr.right().accept(this);
		return null;
	}
	
	/**
	 * Visits the subexpression, and returns null.
	 * @effects unaryExpr.expression.accept(this)
	 * @return null
	 */
	public E visit(UnaryExpression unaryExpr) {
		unaryExpr.expression().accept(this);
		return null;
	}
	
	/**
	 * Visits the declarations and the formula, and returns null.
	 * @effects comprehension.declarations.accept(this) && comprehension.formula.accept(this)
	 * @return null
	 */
	public E visit(Comprehension comprehension) {
		comprehension.declarations().accept(this);
		comprehension.formula().accept(this);
		return null;
	}

	/**
	 * Visits the if-condition, the then-expression, and the else-expression,
	 * and returns null.
	 * @effects ifExpr.condition.accept(this) && ifExpr.thenExpr.accept(this) &&
	 *          ifExpr.elseExpr.accept(this)
	 * @return null
	 */
	public E visit(IfExpression ifExpr) {
		ifExpr.condition().accept(this);
		ifExpr.thenExpr().accept(this);
		ifExpr.elseExpr().accept(this);
		return null;
	}

	/**
	 * Visits the declarations and the formula and returns null.
	 * @effects quantFormula.declarations.accept(this) && quantFormula.formula.accept(this)
	 * @return null
	 */
	public F visit(QuantifiedFormula quantFormula) {
		quantFormula.declarations().accept(this);
		quantFormula.formula().accept(this);
		return null;
	}
	
	/**
	 * Visits the left and right children, and returns null.
	 * @effects binFormula.left.accept(this) && binFormula.right.accept(this)
	 * @return null
	 */
	public F visit(BinaryFormula binFormula) {
		binFormula.left().accept(this);
		binFormula.right().accept(this);
		return null;
	}
	
	/**
	 * Visits the subformula and returns null.
	 * @effects not.formula.accept(this)
	 * @return null
	 */
	public F visit(NotFormula not) {
		not.formula().accept(this);
		return null;
	}
	
	/**
	 * Returns null.
	 * @return null
	 */
	public F visit(ConstantFormula constant) {
		return null;
	}
	
	/**
	 * Visits the left and right children and returns null.
	 * @effects compFormula.left.accept(this) && compFormula.right.accept(this)
	 * @return null
	 */
	public F visit(ComparisonFormula compFormula) {
		compFormula.left().accept(this);
		compFormula.right().accept(this);
		return null;
	}
	
	/**
	 * Visits the formula and returns null.
	 * @effects multFormula.expression.accept(this)
	 * @return null
	 */
	public F visit(MultiplicityFormula multFormula) {
		multFormula.expression().accept(this);
		return null;
	}
	
	/**
	 * Visits the children of the predicate and returns null.
	 * @effects pred.relation.accept(this) && 
	 *          (pred.name = FUNCTION => pred.domain.accept(this) && pred.range.accept(this)) && 
	 *          (pred.name = TOTAL_ORDERING => 
	 *            pred.ordered.accept(this) && pred.first.accept(this) && pred.last.accept(this) )
	 * @return null
	 */
	public F visit(RelationPredicate pred) {
		pred.relation().accept(this);
		if (pred.name()==RelationPredicate.Name.FUNCTION) {
			final RelationPredicate.Function fp = (RelationPredicate.Function) pred;
			fp.domain().accept(this);
			fp.range().accept(this);
		} else if (pred.name()==RelationPredicate.Name.TOTAL_ORDERING) {
			final RelationPredicate.TotalOrdering tp = (RelationPredicate.TotalOrdering) pred;
			tp.ordered().accept(this);
			tp.first().accept(this);
			tp.last().accept(this);
		}
		return null;
	}
	
}
