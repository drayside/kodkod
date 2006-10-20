package kodkod.engine.fol2sat;

import java.util.Map;
import java.util.Set;

import kodkod.ast.BinaryFormula;
import kodkod.ast.ComparisonFormula;
import kodkod.ast.Formula;
import kodkod.ast.IntComparisonFormula;
import kodkod.ast.MultiplicityFormula;
import kodkod.ast.QuantifiedFormula;
import kodkod.ast.RelationPredicate;
import kodkod.ast.visitor.DepthFirstReplacer;


/**
 * Reduces a trivially (un)satisfiable formula to a subtree that 
 * caused the formula's (un)satisfiability.
 * @author Emina Torlak
 */
final class TrivialFormulaReducer extends DepthFirstReplacer {

	private final Set<Formula> trues, falses;
	
	/**
	 * Constructs a reducer for the given annotated formula, using the provided
	 * sets of formulas to guide the reduction.
	 */
	private TrivialFormulaReducer(AnnotatedNode<Formula> reducible,FOL2BoolTranslator acircuit) {
		super(reducible.sharedNodes());
		this.trues = acircuit.trueFormulas();
		this.falses = acircuit.falseFormulas();
	}
	
	/**
	 * Returns a conjunction of all predicates from formulaPreds that are 
	 * reduced to a constant, and that are not present in reducedPreds.
	 * @return a conjunction of all predicates from formulaPreds that are 
	 * reduced to a constant, and that are not present in reducedPreds.
	 */
	private Formula predicates(Set<RelationPredicate> formulaPreds, Set<RelationPredicate> reducedPreds) {
		Formula ret = Formula.TRUE;
		for(RelationPredicate p: formulaPreds) {
			if (!reducedPreds.contains(p) && isConstant(p))
				ret = ret.and(p);
		}
	    return ret;
	}
		
	/**
	 * Reduces the node of the given annotated formula to the subformula that causes it to simplify
	 * to a constant.
	 * @requires preds = AnnotatedNode.predicates(reducible)
	 * @requires acircuit.formula = reducible.node
	 * @return the subformula of the given formula that causes it to simplify
	 * to a constant.
	 */
	static Formula reduce(AnnotatedNode<Formula> reducible, Map<RelationPredicate.Name, Set<RelationPredicate>> preds, FOL2BoolTranslator fol2bool) {
		final TrivialFormulaReducer r = new TrivialFormulaReducer(reducible, fol2bool);
		Formula reduced = reducible.node().accept(r);
		final Map<RelationPredicate.Name, Set<RelationPredicate>> rpreds = 
			AnnotatedNode.predicates(new AnnotatedNode<Formula>(reduced));
		
		for(Map.Entry<RelationPredicate.Name, Set<RelationPredicate>> e : preds.entrySet()) {
			reduced = reduced.and(r.predicates(e.getValue(), rpreds.get(e.getKey())));
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
			// binFormula must be either in trues or falseDecendents
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
