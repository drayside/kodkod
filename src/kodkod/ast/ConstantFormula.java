/*
 * BooleanLiteral.java
 * Created on Oct 12, 2005
 */
package kodkod.ast;

import kodkod.ast.visitor.ReturnVisitor;
import kodkod.ast.visitor.VoidVisitor;

/** 
 * A boolean literal {@link kodkod.ast.Formula formula}, true or false.
 *
 * @invariant no children
 * @author Emina Torlak 
 */
public abstract class ConstantFormula extends Formula {

	static final ConstantFormula TRUE = new ConstantFormula() {
		
		@Override
	    public Formula compose(BinaryFormula.Operator op, Formula formula) {
			switch(op) {
			case OR: return TRUE;
			case AND: case IMPLIES: case IFF: return formula;
			default: return super.compose(op, formula);
			}
	    }

		@Override
		public Formula not() {
			return FALSE;
		}

		@Override
		public Expression thenElse(Expression thenExpr, Expression elseExpr) {
			return thenExpr;
		}
		
		@Override
		public boolean booleanValue() { return true; }
		
		
	};
	
	static final ConstantFormula FALSE = new ConstantFormula() {
		
		@Override
	    public Formula compose(BinaryFormula.Operator op, Formula formula) {
			switch(op) {
			case AND: return FALSE;
			case OR: return formula;
			case IMPLIES: return TRUE;
			case IFF: return formula.not();
			default: return super.compose(op, formula);
			}
	    }

		@Override
		public Formula not() {
			return TRUE;
		}

		@Override
		public Expression thenElse(Expression thenExpr, Expression elseExpr) {
			return elseExpr;
		}
		
		@Override
		public boolean booleanValue() { return false; }
		
	};
	
	private ConstantFormula() {}
    
	/**
	 * Returns the boolean value that corresponds to this 
	 * constant formula.
	 * @return this=TRUE => true, false
	 */
	public abstract boolean booleanValue();
	
	/**
     * Accepts the given visitor and returns the result.
     * @see kodkod.ast.Node#accept(kodkod.ast.visitor.ReturnVisitor)
     */
	@Override
	public <E, F, D> F accept(ReturnVisitor<E, F, D> visitor) {
		return visitor.visit(this);
	}
	
	/**
     * Accepts the given visitor.
     * @see kodkod.ast.Node#accept(kodkod.ast.visitor.VoidVisitor)
     */
    public void accept(VoidVisitor visitor) {
        visitor.visit(this);
    }
	
	public String toString() {
		return "" + (this == TRUE);
	}
	
	/**
     * Returns true if and only if compared to itself; otherwise returns false.
     * 
     * @return this = o
     */
    public final boolean equals(Object o) {
        return this == o;
    }
	
}

