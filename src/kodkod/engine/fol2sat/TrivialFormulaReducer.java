package kodkod.engine.fol2sat;

import java.util.Iterator;
import java.util.Set;

import kodkod.ast.BinaryFormula;
import kodkod.ast.ComparisonFormula;
import kodkod.ast.Formula;
import kodkod.ast.IntComparisonFormula;
import kodkod.ast.MultiplicityFormula;
import kodkod.ast.Node;
import kodkod.ast.QuantifiedFormula;
import kodkod.ast.RelationPredicate;
import kodkod.ast.visitor.AbstractReplacer;
import kodkod.engine.bool.BooleanConstant;
import kodkod.util.collections.IdentityHashSet;
import kodkod.util.ints.IntSet;
import kodkod.util.ints.IntTreeSet;


/**
 * Reduces a trivially (un)satisfiable formula to a subtree that 
 * caused the formula's (un)satisfiability.
 * @author Emina Torlak
 */
final class TrivialFormulaReducer extends AbstractReplacer {

	private final Set<Node> trues, falses;
	
	/**
	 * Constructs a reducer for the given annotated formula, using the provided
	 * sets of formulas to guide the reduction.
	 */
	private TrivialFormulaReducer(AnnotatedNode<Formula> reducible, TranslationLog log) {
		super(reducible.sharedNodes());
		this.trues = new IdentityHashSet<Node>();
		this.falses = new IdentityHashSet<Node>();
		final IntSet constants = new IntTreeSet();
		constants.add(BooleanConstant.TRUE.label());
		constants.add(BooleanConstant.FALSE.label());
		for(Iterator<TranslationLog.Record> i = log.replay(constants); i.hasNext(); ) {
			TranslationLog.Record next = i.next();
			if (next.env().isEmpty()) { // no free variables
				if (next.literal()>0) 	{ trues.add(next.node()); }
				else 					{ falses.add(next.node()); }
			}
		}
		completeConstantInit(reducible.node());
	}
	
	/**
	 * Completes the initialization of the sets trues and falses.
	 * @effects updates the sets trues and falses with the nodes whose
	 * (in)validity is implied by the current members of the two sets.
	 */
	private void completeConstantInit(Formula formula) {
		if (!isConstant(formula)) {
			if (formula instanceof BinaryFormula) {
				final BinaryFormula bin = (BinaryFormula) formula;
				if (bin.op()==BinaryFormula.Operator.AND) {
					completeConstantInit(bin.left());
					completeConstantInit(bin.right());
					if (isFalse(bin.left())||isFalse(bin.right())) {
						falses.add(formula);
					} else if (isTrue(bin.left())&&isTrue(bin.right())) {
						trues.add(formula);
					}
				} else if (bin.op()==BinaryFormula.Operator.OR) {
					completeConstantInit(bin.left());
					completeConstantInit(bin.right());
					if (isTrue(bin.left())||isTrue(bin.right())) {
						trues.add(formula);
					} else if (isFalse(bin.left())&&isFalse(bin.right())) {
						falses.add(formula);
					}
				}
			}
		}
	}
	
	/**
	 * Reduces the node of the given annotated formula to the subformula that causes it to simplify to a constant.
	 * @param broken predicates on which symmetries have been broken
	 * @param log the translation log
	 * @requires reducible.node() = log.formula
	 * @return the subformula of the given formula that causes it to simplify to a constant.
	 */
	static Formula reduce(AnnotatedNode<Formula> reducible, Set<RelationPredicate> broken, TranslationLog log) {
		final TrivialFormulaReducer r = new TrivialFormulaReducer(reducible, log);
		Formula reduced = reducible.node().accept(r);
	
		for(RelationPredicate p : broken) {
			reduced = reduced.and(p);
		}
		return reduced;
	}

	/**
	 * Returns true if the formula was simplified to TRUE.
	 * @return true if the formula was simplified to TRUE 
	 */
	private final boolean isTrue(Formula formula) {
		return trues.contains(formula);
	}
	
	/**
	 * Returns true if the formula was simplified to FALSE. 
	 * @return true if the formula was simplified to FALSE 
	 */
	private final boolean isFalse(Formula formula) {
		return falses.contains(formula);
	}
	
	/**
	 * Returns true if the formula was simplified to a constant. 
	 * @return true if the formula was simplified to a constant. 
	 */
	private final boolean isConstant(Formula formula) {
		return falses.contains(formula) || trues.contains(formula);
	}
	
	/**
	 * Reduces the given binary formula and returns the result.
	 * @return the reduced formula
	 */
	public Formula visit(BinaryFormula binFormula) {
		Formula ret = lookup(binFormula);
		
		if (ret==null) {
			// if this method was called with this argument,
			// binFormula must be either in trues or falses
			boolean binValue = isTrue(binFormula); 
			final Formula l = binFormula.left(), r = binFormula.right();
			final Formula lnew, rnew; 
			if (!isConstant(l) && !isConstant(r)) {
				lnew = l; rnew = r;
			} else {
				switch(binFormula.op()) {
				case AND : 
					lnew = binValue || isFalse(l) ? l.accept(this) : Formula.TRUE;
					rnew = binValue || isFalse(r) ? r.accept(this) : Formula.TRUE;
					break;
				case OR : 
					lnew = !binValue || isTrue(l) ? l.accept(this) : Formula.FALSE;
					rnew = !binValue || isTrue(r) ? r.accept(this) : Formula.FALSE;
					break;
				case IMPLIES: // !l || r
					lnew = !binValue || isFalse(l) ? l.accept(this) : Formula.FALSE;  
					rnew = !binValue || isTrue(r) ? r.accept(this) : Formula.FALSE;
					break;
				case IFF: 
					lnew = isConstant(l) ? l.accept(this) : l;
					rnew = isConstant(r) ? r.accept(this) : r;
					break;
				default :
					throw new IllegalArgumentException("Unknown operator: " + binFormula.op());
				}
			}
			ret = (lnew==l && rnew==r) ? binFormula : lnew.compose(binFormula.op(), rnew);     
		}
		return cache(binFormula,ret);
	}
	
	/**
	 * Does nothing.  No reduction is possible inside quantified formulas.
	 * @return quantFormula
	 */
	public Formula visit(QuantifiedFormula quantFormula) {
		return quantFormula;
	}
	
	/**
	 * Does nothing.  No reduction is possible inside comparison formulas.
	 * @return compFormula
	 */
	public Formula visit(ComparisonFormula compFormula) {
		return compFormula;
	}
	
	/**
	 * Does nothing.  No reduction is possible inside multiplicity formulas.
	 * @return multFormula
	 */
	public Formula visit(MultiplicityFormula multFormula) {
		return multFormula;
	}
	
	/**
	 * Does nothing.  No reduction is possible inside predicates.
	 * @return pred
	 */
	public Formula visit(RelationPredicate pred) {
		return pred;
	}
	
	/**
	 * Does nothing.  No reduction is possible inside integer comparison formulas.
	 * @return intComp
	 */
	public Formula visit(IntComparisonFormula intComp) {
		return intComp;
	}

}
