package kodkod.ast;


import java.util.List;

import kodkod.ast.visitor.ReturnVisitor;
import kodkod.ast.visitor.VoidVisitor;

/**
 * Represents a constant valued expression.
 * 
 * @invariant no children
 * @author Emina Torlak
 */
public abstract class ConstantExpression extends LeafExpression {
	
	static final ConstantExpression NONE = new ConstantExpression("none", 1) {
		
		@Override
		public Formula apply(Multiplicity mult) {
			switch(mult) {
			case LONE: case NO: return Formula.TRUE;
			case ONE: case SOME: return Formula.FALSE;
			default: return super.apply(mult);
			}
		}
		
		@Override
		public Expression compose(BinaryExpression.Operator op, Expression expr) {
			switch(op) {
			case DIFFERENCE: case INTERSECTION: return NONE;
			case UNION: return expr;
			default: return super.compose(op, expr);
			}
		}
		
		@Override
		public Expression project(List<IntExpression> columns) {
			if (columns.isEmpty())
				throw new IllegalArgumentException("no columns specified for projection");
			return this;
		}
		
		@Override
		public Formula compose(ComparisonFormula.Operator op, Expression expr) {
			if (op == ComparisonFormula.Operator.SUBSET) return Formula.TRUE;
			return super.compose(op, expr);
		}
		
	};
	
	static final ConstantExpression UNIV = new ConstantExpression("univ", 1) {
		
		@Override
		public Expression compose(BinaryExpression.Operator op, Expression expr) {
			switch(op) {
			case UNION: return UNIV;
			case INTERSECTION: return expr;
			default: return super.compose(op, expr);
			}
		}
		
	};
	
	static final ConstantExpression IDEN = new ConstantExpression("iden", 2) {
		
		@Override
		public Expression apply(UnaryExpression.Operator op) {
			if (op == UnaryExpression.Operator.TRANSPOSE) return IDEN;
			return super.apply(op);
		}
		
	};
	
	static final ConstantExpression INTS = new ConstantExpression("ints", 1) {
		
	};
	
	/**
	 * Constructs a constant expression with the given arity.
	 */
	private ConstantExpression(String name, int arity) {
		super(name, arity);
	}

	/**
     * Accepts the given visitor and returns the result.
     * @see kodkod.ast.Node#accept(kodkod.ast.visitor.ReturnVisitor)
     */
	@Override
	public <E, F, D, I> E accept(ReturnVisitor<E, F, D, I> visitor) {
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
