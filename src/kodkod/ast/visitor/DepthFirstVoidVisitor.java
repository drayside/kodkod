package kodkod.ast.visitor;

import kodkod.ast.BinaryExpression;
import kodkod.ast.BinaryFormula;
import kodkod.ast.BinaryIntExpression;
import kodkod.ast.ComparisonFormula;
import kodkod.ast.Comprehension;
import kodkod.ast.ConstantExpression;
import kodkod.ast.ConstantFormula;
import kodkod.ast.Decl;
import kodkod.ast.Decls;
import kodkod.ast.ExprIntCast;
import kodkod.ast.IfExpression;
import kodkod.ast.IfIntExpression;
import kodkod.ast.IntComparisonFormula;
import kodkod.ast.IntConstant;
import kodkod.ast.IntExprCast;
import kodkod.ast.MultiplicityFormula;
import kodkod.ast.Node;
import kodkod.ast.NotFormula;
import kodkod.ast.QuantifiedFormula;
import kodkod.ast.Relation;
import kodkod.ast.RelationPredicate;
import kodkod.ast.UnaryExpression;
import kodkod.ast.Variable;

/** 
 * Implements a depth first traversal of the kodkod AST.  
 *
 * @author Emina Torlak 
 */
public abstract class DepthFirstVoidVisitor implements VoidVisitor {
	
	protected DepthFirstVoidVisitor() {}
	
	/**
	 * Returns true if this node has already been visited. 
	 * Otherwise returns false.  
	 * @return true if this node has already been visited. 
	 */
	protected abstract boolean visited(Node n) ;
	
	/**
	 * Visits all the children of the given declarations node if
	 * this.visited(decls) returns false.  Otherwise does nothing.
	 * @effects all d: declarations.declarations | d.variable.accept(this) && d.expression.accept(this)
	 */
	public void visit(Decls decls) {
		if (!visited(decls)) {
			for (Decl decl : decls) {
				decl.accept(this);
			}
		}
	}
	
	/**
	 * Visits the variable and expression of this decl if
	 * this.visited(decl) returns false.  Otherwise does nothing.
	 * @effects decl.variable.accept(this) && decl.expression.accept(this)
	 */
	public void visit(Decl decl) {
		if (!visited(decl)) {
			decl.variable().accept(this);
			decl.expression().accept(this);
		}
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
	 * Visits the left and right subexpressions  if
	 * this.visited(binExpr) returns false.  Otherwise does nothing.
	 * @effects binExpr.left.accept(this) && binExpr.right.accept(this)
	 */
	public void visit(BinaryExpression binExpr) {
		if (!visited(binExpr)) {
			binExpr.left().accept(this);
			binExpr.right().accept(this);
		}
	}
	
	/**
	 * Visits the subexpression  if
	 * this.visited(unaryExpr) returns false.  Otherwise does nothing.
	 * @effects unaryExpr.expression.accept(this)
	 */
	public void visit(UnaryExpression unaryExpr) {
		if (!visited(unaryExpr)) {
			unaryExpr.expression().accept(this);
		}
	}
	
	/**
	 * Visits the declarations and the formula  if
	 * this.visited(comprehension) returns false.  Otherwise does nothing.
	 * @effects comprehension.declarations.accept(this) && comprehension.formula.accept(this)
	 */
	public void visit(Comprehension comprehension) {
		if (!visited(comprehension)) {
			comprehension.declarations().accept(this);
			comprehension.formula().accept(this);
		}
	}
	
	/**
	 * Visits the if-condition, the then-expression, and the else-expression  if
	 * this.visited(ifExpr) returns false.  Otherwise does nothing.
	 * @effects ifExpr.condition.accept(this) && ifExpr.thenExpr.accept(this) &&
	 *          ifExpr.elseExpr.accept(this)
	 */
	public void visit(IfExpression ifExpr) {
		if (!visited(ifExpr)) {
			ifExpr.condition().accept(this);
			ifExpr.thenExpr().accept(this);
			ifExpr.elseExpr().accept(this);
		}
	}
	
	/**
	 * Visits castExpr.intExpr  if
	 * this.visited(castExpr) returns false.  Otherwise does nothing.
	 * @effects castExpr.expression.accept(this)
	 */
	public void visit(IntExprCast castExpr) {
		if (!visited(castExpr)) {
			castExpr.intExpr().accept(this);
		}
	}

	/**
	 * Does nothing.
	 */
	public void visit(IntConstant intConst) {}
	
	/**
	 * Visits the if-condition, the then-expression, and the else-expression  if
	 * this.visited(intExpr) returns false.  Otherwise does nothing.
	 * @effects intExpr.condition.accept(this) && intExpr.thenExpr.accept(this) &&
	 *          intExpr.elseExpr.accept(this)
	 */
	public void visit(IfIntExpression intExpr) {
		if (!visited(intExpr)) {
			intExpr.condition().accept(this);
			intExpr.thenExpr().accept(this);
			intExpr.elseExpr().accept(this);
		}
	}
	
	/**
	 * Visits intExpr.expression  if
	 * this.visited(intExpr) returns false.  Otherwise does nothing.
	 * @effects intExpr.expression.accept(this)
	 */
	public void visit(ExprIntCast intExpr) {
		if (!visited(intExpr)) {
			intExpr.expression().accept(this);
		}
	}
	
	/**
	 * Visits the children of the given integer expression  if
	 * this.visited(intExpr) returns false.  Otherwise does nothing.
	 * @effects intExpr.left.accept(this) && intExpr.right.accept(this)
	 */
	public void visit(BinaryIntExpression intExpr) {
		if (!visited(intExpr)) {
			intExpr.left().accept(this);
			intExpr.right().accept(this);
		}
	}
	
	/**
	 * Visits the children of the given integer comparison formula  if
	 * this.visited(intComp) returns false.  Otherwise does nothing.
	 * @effects intComp.left.accept(this) && intComp.right.accept(this)
	 */
	public void visit(IntComparisonFormula intComp) {
		if (!visited(intComp)) {
			intComp.left().accept(this);
			intComp.right().accept(this);
		}
	}
	
	/**
	 * Visits the declarations and the formula  if
	 * this.visited(quantFormula) returns false.  Otherwise does nothing.
	 * @effects quantFormula.declarations.accept(this) && quantFormula.formula.accept(this)
	 */
	public void visit(QuantifiedFormula quantFormula) {
		if (!visited(quantFormula)) {
			quantFormula.declarations().accept(this);
			quantFormula.formula().accept(this);
		}
	}
	
	/**
	 * Visits the left and right children  if
	 * this.visited(binFormula) returns false.  Otherwise does nothing.
	 * @effects binFormula.left.accept(this) && binFormula.right.accept(this)
	 */
	public void visit(BinaryFormula binFormula) {
		if (!visited(binFormula)) {
			binFormula.left().accept(this);
			binFormula.right().accept(this);
		}
	}
	
	/**
	 * Visits the subformula  if
	 * this.visited(not) returns false.  Otherwise does nothing.
	 * @effects not.formula.accept(this)
	 */
	public void visit(NotFormula not) {
		if (!visited(not)) {
			not.formula().accept(this);
		}
	}
	
	/**
	 * Does nothing.
	 */
	public void visit(ConstantFormula constant) {}
	
	/**
	 * Visits the left and right children  if
	 * this.visited(compFormula) returns false.  Otherwise does nothing.
	 * @effects compFormula.left.accept(this) && compFormula.right.accept(this)
	 */
	public void visit(ComparisonFormula compFormula) {
		if (!visited(compFormula)) {
			compFormula.left().accept(this);
			compFormula.right().accept(this);
		}
	}
	
	/**
	 * Visits the formula  if
	 * this.visited(multFormula) returns false.  Otherwise does nothing.
	 * @effects multFormula.expression.accept(this)
	 */
	public void visit(MultiplicityFormula multFormula) {
		if (!visited(multFormula)) {
			multFormula.expression().accept(this);
		}
	}
	
	/**
	 * Visits the children of the predicate  if
	 * this.visited(pred) returns false.  Otherwise does nothing.
	 * @effects pred.relation.accept(this) && 
	 *          (pred.name = FUNCTION => pred.domain.accept(this) && pred.range.accept(this)) && 
	 *          (pred.name = TOTAL_ORDERING => 
	 *            pred.ordered.accept(this) && pred.first.accept(this) && pred.last.accept(this) )
	 */
	public void visit(RelationPredicate pred) {
		if (!visited(pred)) {
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
	
}
