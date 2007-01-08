package kodkod.engine.fol2sat;

import java.util.IdentityHashMap;
import java.util.Map;
import java.util.Set;

import kodkod.ast.Formula;
import kodkod.ast.Node;
import kodkod.engine.bool.BooleanConstant;
import kodkod.engine.bool.BooleanFormula;
import kodkod.engine.bool.BooleanMatrix;
import kodkod.engine.bool.BooleanValue;
import kodkod.util.collections.IdentityHashSet;
import kodkod.util.ints.IntSet;
import kodkod.util.ints.Ints;

/**
 * A TranslationCache that tracks variables in addition to 
 * caching translations.
 * 
 * @invariant node in Formula
 * @specfield varUsage: node.*children & (Expression + Formula) -> set int
 * @specfield trueFormulas: set node.*children & Formula
 * @specfield falseFormulas: set node.*children & Formula
 */
final class TrackingCache extends TranslationCache {
	private final Map<Node, IntSet> varUsage;
	private final Set<Formula> trueFormulas, falseFormulas;
	
	/**
	 * Constructs a new tracking translation cache for the given annotated node.
	 * @effects this.node' = annotated.node 
	 */
	TrackingCache(AnnotatedNode<Formula> annotated) {
		super(annotated);
		this.varUsage = new IdentityHashMap<Node,IntSet>();
		this.trueFormulas = new IdentityHashSet<Formula>();
		this.falseFormulas = new IdentityHashSet<Formula>();
	}

	/**
	 * Returns this.varUsage
	 * @return this.varUsage
	 */
	Map<Node, IntSet> varUsage() {
		return varUsage;
	}
	
	/**
	 * Return this.trueFormulas
	 * @return this.trueFormulas
	 */
	Set<Formula> trueFormulas() {
		return trueFormulas;
	}
	
	/**
	 * Return this.falseFormulas
	 * @return this.falseFormulas
	 */
	Set<Formula> falseFormulas() {
		return falseFormulas;
	}
	
	/**
	 * If the given formula is one for which we are caching translations,
	 * the provided translation is cached and returned.  Otherwise,
	 * the translation is simply returned. In addition, this method records
	 * the label of the translation in the varUsage map, if the translation
	 * is non-constant.  If it is constant, it records the formula as being
	 * constant.  
	 * @return translation
	 * @effects if the formula is one for which we are caching translations,
	 * the provided translation is cached.
	 * @effects translation = BooleanConstant.TRUE => 
	 * 	          this.trueFormulas' = this.trueFormulas + formula,
	 *          translation = BooleanConstant.FALSE => 
	 * 	          this.falseFormulas' = this.falseFormulas + formula,
	 *          this.varUsage' = this.varUsage + formula -> |translation.label|
	 */
	@Override
	BooleanValue cache(Formula formula, BooleanValue translation, Environment<BooleanMatrix> env) {
		if (translation==BooleanConstant.TRUE) {
			if (env.isEmpty()) trueFormulas.add(formula);
		} else if (translation==BooleanConstant.FALSE) {
			if (env.isEmpty()) falseFormulas.add(formula);
		} else if (env.isEmpty()) { // top-level formula
			varUsage.put(formula, Ints.singleton(StrictMath.abs(((BooleanFormula)translation).label())));	
		} else {
			IntSet vars = varUsage.get(formula);
			if (vars==null)
				vars = Ints.bestSet(1, Integer.MAX_VALUE-1);
			vars.add(StrictMath.abs(((BooleanFormula)translation).label()));
			varUsage.put(formula, vars);
		}
		return super.cache(formula, translation, env);
	}
	
}