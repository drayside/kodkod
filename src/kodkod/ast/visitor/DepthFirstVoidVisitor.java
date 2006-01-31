package kodkod.ast.visitor;

import kodkod.ast.BinaryExpression;
import kodkod.ast.BinaryFormula;
import kodkod.ast.ComparisonFormula;
import kodkod.ast.Comprehension;
import kodkod.ast.ConstantExpression;
import kodkod.ast.ConstantFormula;
import kodkod.ast.Decl;
import kodkod.ast.Decls;
import kodkod.ast.IfExpression;
import kodkod.ast.MultiplicityFormula;
import kodkod.ast.NotFormula;
import kodkod.ast.QuantifiedFormula;
import kodkod.ast.Relation;
import kodkod.ast.RelationPredicate;
import kodkod.ast.UnaryExpression;
import kodkod.ast.Variable;

/** 
 * Implements the depth first traversal of the AST.  Each node is
 * visited once for each parent.  That is, if a node has exactly one
 * parent, it is visited exactly once.  If it is status by two parents,
 * it is visited twice, following each of the parents' child pointers.
 *
 * @author Emina Torlak 
 */
public abstract class DepthFirstVoidVisitor implements VoidVisitor {

	protected DepthFirstVoidVisitor() {}

	/**
	 * Visits all the children of the given declarations node.
	 * @effects all d: declarations.declarations | d.variable.accept(this) && d.expression.accept(this)
	 */
	public void visit(Decls decls) {
		for (Decl decl : decls) {
			decl.accept(this);
		}
	}

	/**
	 * Visits the variable and expression of this decl.
	 * @effects decl.variable.accept(this) && decl.expression.accept(this)
	 */
	public void visit(Decl decl) {
		decl.variable().accept(this);
		decl.expression().accept(this);
	}

	/**
	 * Does nothing.
	 */
	public void visit(Relation relation) {}

	/**
	 * Does nothing.
	 */
	public void visit(Variable variable) {}

	/**
	 * Does nothing.
	 */
	public void visit(ConstantExpression constExpr) {}

	/**
	 * Visits the left and right subexpressions.
	 * @effects binExpr.left.accept(this) && binExpr.right.accept(this)
	 */
	public void visit(BinaryExpression binExpr) {
		binExpr.left().accept(this);
		binExpr.right().accept(this);
	}

	/**
	 * Visits the subexpression.
	 * @effects unaryExpr.expression.accept(this)
	 */
	public void visit(UnaryExpression unaryExpr) {
		unaryExpr.expression().accept(this);
	}

	/**
	 * Visits the declarations and the formula.
	 * @effects comprehension.declarations.accept(this) && comprehension.formula.accept(this)
	 */
	public void visit(Comprehension comprehension) {
		comprehension.declarations().accept(this);
		comprehension.formula().accept(this);
	}

	/**
	 * Visits the if-condition, the then-expression, and the else-expression.
	 * @effects ifExpr.condition.accept(this) && ifExpr.thenExpr.accept(this) &&
	 *          ifExpr.elseExpr.accept(this)
	 */
	public void visit(IfExpression ifExpr) {
		ifExpr.condition().accept(this);
		ifExpr.thenExpr().accept(this);
		ifExpr.elseExpr().accept(this);
	}

	/**
	 * Visits the declarations and the formula.
	 * @effects quantFormula.declarations.accept(this) && quantFormula.formula.accept(this)
	 */
	public void visit(QuantifiedFormula quantFormula) {
		quantFormula.declarations().accept(this);
		quantFormula.formula().accept(this);
	}

	/**
	 * Visits the left and right children.
	 * @effects binFormula.left.accept(this) && binFormula.right.accept(this)
	 */
	public void visit(BinaryFormula binFormula) {
		binFormula.left().accept(this);
		binFormula.right().accept(this);
	}

	/**
	 * Visits the subformula.
	 * @effects not.formula.accept(this)
	 */
	public void visit(NotFormula not) {
		not.formula().accept(this);
	}

	/**
	 * Does nothing.
	 */
	public void visit(ConstantFormula constant) {}

	/**
	 * Visits the left and right children.
	 * @effects compFormula.left.accept(this) && compFormula.right.accept(this)
	 */
	public void visit(ComparisonFormula compFormula) {
		compFormula.left().accept(this);
		compFormula.right().accept(this);
	}

	/**
	 * Visits the formula.
	 * @effects multFormula.expression.accept(this)
	 */
	public void visit(MultiplicityFormula multFormula) {
		multFormula.expression().accept(this);
	}

	/**
	 * Visits the children of the predicate.
	 * @effects pred.relation.accept(this) && 
	 *          (pred.name = FUNCTION => pred.domain.accept(this) && pred.range.accept(this)) && 
	 *          (pred.name = TOTAL_ORDERING => 
	 *            pred.ordered.accept(this) && pred.first.accept(this) && pred.last.accept(this) )
	 */
	public void visit(RelationPredicate pred) {
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

	}

}
