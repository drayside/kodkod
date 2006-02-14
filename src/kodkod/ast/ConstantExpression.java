package kodkod.ast;

import kodkod.ast.visitor.ReturnVisitor;
import kodkod.ast.visitor.VoidVisitor;

/**
 * Represents a constant valued expression.
 * 
 * @invariant no children
 * @author Emina Torlak
 */
public final class ConstantExpression extends LeafExpression {
	
	static final ConstantExpression UNIV = new ConstantExpression("univ", 1);
	
	static final ConstantExpression IDEN = new ConstantExpression("iden", 2);
	
	static final ConstantExpression NONE = new ConstantExpression("none", 1);
	
	/**
	 * Constructs a constant expression with the given arity.
	 */
	private ConstantExpression(String name, int arity) {
		super(name, arity);
	}

	@Override
	public Formula apply(Multiplicity mult) {
		if (this == NONE) {
			switch(mult) {
			case LONE: return Formula.TRUE;
			case NO:   return Formula.TRUE;
			case ONE:  return Formula.FALSE;
			case SOME: return Formula.FALSE;
			}
		}
		return super.apply(mult);
	}

	@Override
	public Expression apply(UnaryExpression.Operator op) {
		if (this == IDEN && op == UnaryExpression.Operator.TRANSPOSE) {
			return IDEN;
		}
		return super.apply(op);
	}

	@Override
	public Expression compose(BinaryExpression.Operator op, Expression expr) {
		if (this == UNIV) {
			switch(op) {
			case UNION:        return UNIV;
			case INTERSECTION: return expr;
			}
		} else if (this == NONE) {
			switch(op) {
			case UNION:        return expr;
			case INTERSECTION: return NONE;
			}
		}
		return super.compose(op, expr);
	}

	@Override
	public Formula compose(ComparisonFormula.Operator op, Expression expr) {
		if (this == NONE && op == ComparisonFormula.Operator.SUBSET) {
			return Formula.TRUE;
		}
		return super.compose(op, expr);
	}

	/**
     * Accepts the given visitor and returns the result.
     * @see kodkod.ast.Node#accept(kodkod.ast.visitor.ReturnVisitor)
     */
	@Override
	public <E, F, D> E accept(ReturnVisitor<E, F, D> visitor) {
		return visitor.visit(this);
	}
	
	/**
     * Accepts the given visitor.
     * @see kodkod.ast.Node#accept(kodkod.ast.visitor.VoidVisitor)
     */
    public void accept(VoidVisitor visitor) {
        visitor.visit(this);
    }
}
