package kodkod.engine.bool;

import java.util.Iterator;


/**
 * Represents a gate with two or more inputs; an AND or an OR gate.
 *  
 * @specfield op: Operator
 * @invariant #inputs > 1
 * @invariant some components.this => literal in [1..Integer.MAX_VALUE), literal in [0..Integer.MAX_VALUE)
 * @invariant no c1, c2: inputs | c1.literal = -c2.literal
 * @invariant this.literal > 0 => all c: inputs | |c.literal| < this.literal
 * @author Emina Torlak
 */
public abstract class MultiGate extends BooleanFormula {
	final Operator op;
	
	private NotGate negation = null;
	private final int literal;
	
	/**
	 * Constructs a new MultiGate gate with the given owner and
	 * operator.
	 * @requires op != null && owner != null
	 * @effects this.op' = op 
	 * @throws NullPointerException - owner = null
	 */
	MultiGate(Operator op, int literal) {
		assert op != null;
		assert literal >= 0;
		this.op = op;
		this.literal = literal;
	}
	
	@Override
	final BooleanFormula negation() {
		if (negation==null) {
			negation = new NotGate(this);
		}
		return negation;
	}
	
	@Override
	public final int literal() { return literal; }

	/**
	 * Returns true if the given value is a part of this
	 * circuit with respect to the given operator, when this formula's
	 * tree is checked down to the given depth.  For a multigate,
	 * this operation returns true iff this.op = op and v is a descendent
	 * of this that is reachable in depth steps.  For example, consider
	 * the formula 1 | (2 | (!(3 | 4) | (6 & 7))), v = !(3 | 4) && depth = 2.
	 * The operation will return true (1 is at depth 0, 2 at depth 1, and !(3 | 4) at depth 2).
	 * If the operation is called with depth = 1, it will return false. 
	 * @requires depth >= 0 
	 * @return true if the given value is an atomic part of this
	 * circuit with respect to the given operator, when this formula's
	 * tree is check down to the given depth; otherwise returns false.
	 */
	@Override
	boolean contains(Operator op, BooleanFormula f, int depth) {
		if (f==this) return true;
		else if (depth <= 0 || this.op != op) return false;
		else {
			final int vabs = f.literal() > 0 ? f.literal() : -f.literal();
			for(Iterator<BooleanValue> children = inputs(); children.hasNext();) {
				BooleanFormula child = (BooleanFormula) children.next();
				if (f==child || (vabs < child.literal() && child.contains(op, f, depth-1))) {
					return true;
				}
			}
			return false;
		}
	}
	
	/** 
	 * Returns true if this formula is a part of the given formula
	 * with respect to the specified operator, when the trees of this and f
	 * are checked to down to the depths thisDepth and fDepth, respectively.
	 * For a multigate, this method returns true iff flattenning this formula 
	 * down to depth thisDepth yields a superset of parts obtained by flattening
	 * f down to depth fDepth.  For example, consider
	 * the formula 1 | (2 | (!(3 | 4) | (6 & 7))), f = 1 | !(3 | 4), thisDepth = 2 && 
	 * fDepth = 1. The operation will return true since flattening this down to depth 2
	 * yields {1, 2, !(3 | 4), (6 | 7) } and flattening f to depth 1 yields
	 * {1, !(3 | 4)}.  If the operation is called with thisDepth = 1 or fDepth = 0, 
	 * it will return false.    
	 * @return true if this formula is a part of the given formula
	 * with respect to the specified operator, when the trees of this and f
	 * are checked to down to the depths thisDepth and fDepth, respectively.
	 */
	@Override
	boolean isPartOf(Operator op, BooleanFormula f, int thisDepth, int fDepth) {
		if (f.contains(op, this, fDepth)) return true;
		else if (thisDepth <= 0 || this.op != op) return false; 
		else {
			for(Iterator<BooleanValue> children = inputs(); children.hasNext();) {
				if (!((BooleanFormula)children.next()).isPartOf(op, f, thisDepth-1, fDepth)) {
					return false;
				}
			}
			return true;
		}
	}
	
	/**
	 * Returns the number of 'irreducible' parts of this circuit, 
	 * with respect to the given operator. That is, suppose that this.op = op.  
	 * If we were to flatten out this gate so that none
	 * of its drivers are the same kind of circuit as this, the
	 * resulting inputs would be its irreducible inputs.  For example,
	 * suppose that this is the circuit 1 | (2 | (3 | (!(4 | 5) | (6 & (7 & (8 | 9)))))),
	 * then this has two inputs, 1 and  (2 | (3 | (!(4 | 5) | (6 & (7 & (8 | 9)))))), and
	 * 5 irreducible inputs: 1, 2, 3, !(4 | 5), and (6 & (7 & (8 | 9)).
	 * When this.op != op, then the number of atomic parts is 1 (just the circuit itself)
	 * @return the number of atomic parts of this circuit
	 */
	@Override
	abstract int numAtomicParts(Operator op);
		
	/**
	 * Returns the operator used to combine the input
	 * variables of this connective gate.
	 * @return this.op
	 */
	public final Operator op() { return op; }
	
	@Override
	public <T, A> T accept(BooleanVisitor<T,A> visitor, A arg) {
		return visitor.visit(this, arg);
	}
	
	public String toString() {
		final StringBuilder builder = new StringBuilder("(");
		final Iterator<BooleanValue> children = inputs();
		builder.append(children.next());
		while(children.hasNext()) {
			builder.append(op);
			builder.append(children.next());
		}
		builder.append(")");
		return builder.toString();
	}
	
	/**
	 * Represents the operator used to combine 
	 * the input variables of a MultiGate.
	 * 
	 */
	public static enum Operator {
		AND {
			public String toString() { return "&"; }
			/** @return true */
			public BooleanConstant identity() { return BooleanConstant.TRUE; }
			/** @return false */
			public BooleanConstant shortCircuit() { return BooleanConstant.FALSE; }
			/** @return OR */
			public Operator complement() {  return OR; }
		},
		OR {
			public String toString() { return "|"; }
			/** @return false */
			public BooleanConstant identity() { return BooleanConstant.FALSE; }
			/** @return true */
			public BooleanConstant shortCircuit() { return BooleanConstant.TRUE; }
			/** @return AND */
			public Operator complement() {  return AND; }
		};
		/**
		 * Returns the boolean constant <i>c</i> such that 
		 * for all logical values <i>x</i>, <i>c</i> composed
		 * with <i>x</i> using this operator will result in <i>x</i>.
		 * @return the identity value of this operator
		 */
		public abstract BooleanConstant identity();
		/**
		 * Returns the boolean constant <i>c</i> such that 
		 * for all logical values <i>x</i>, <i>c</i> composed
		 * with <i>x</i> using this operator will result in <i>c</i>.
		 * @return the short circuiting value of this operator
		 */
		public abstract BooleanConstant shortCircuit();
		/**
		 * Returns the Operator whose identity and short circuit
		 * values are the negation of this operator's identity and
		 * short circuit.
		 * @return the complement of this operator
		 */
		public abstract Operator complement();
	}

	
}
