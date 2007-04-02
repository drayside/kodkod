/**
 * 
 */
package kodkod.engine.fol2sat;

import java.util.IdentityHashMap;
import java.util.Map;
import java.util.Set;

import kodkod.ast.Formula;
import kodkod.ast.Node;
import kodkod.ast.RelationPredicate;
import kodkod.ast.visitor.AbstractReplacer;

/**
 * Given an annotated formula, replaces the predicates on which symmetries 
 * have not been broken with their bodies and those on which symmetries
 * have been broken with the constant TRUE.
 * @specfield broken: set RelationPredicate
 * @author Emina Torlak
 */
abstract class PredicateInliner extends AbstractReplacer {
	private final Set<RelationPredicate> broken;
	/**
	 * Constructs a predicate inliner for the given annotated formula
	 * and broken set.
	 */
	private PredicateInliner(AnnotatedNode<Formula> annotated, Set<RelationPredicate> broken) {
		super(annotated.sharedNodes());
		this.broken = broken;
	}

	/**
	 * Returns an annotated formula f such that f.node is equivalent
	 * to annotated.node, with its relation predicates replaced
	 * by their bodies, if they are not in the broken set, or the constant TRUE
	 * if they are.   If trackSources flag is true and annotated.node contains relation
	 * predicates, f will contain transitive source information 
	 * for each of the descendents of f.node.  Specifically, let t be a subformula of f.node, and
	 * s be a descdendent of annotated.node from which t was derived.  Then, 
	 * f.source[t] = annotated.source[s].  If options.trackFormulas is false, no source 
	 * information will be recorded (i.e. f.source[t] = t for all descendents t of f).
	 * @return an annotated formula f such that f is equivalent
	 * to the given formula, with its relation predicates replaced
	 * by their bodies, if they are not in the broken set, or the constant TRUE
	 * if they are.
	 **/
	static final AnnotatedNode<Formula> inline(final AnnotatedNode<Formula> annotated, Set<RelationPredicate> broken, boolean trackSources) {
		if (trackSources) {
			final Map<Node,Node> source = new IdentityHashMap<Node,Node>();
			final PredicateInliner inliner = new PredicateInliner(annotated, broken){
				@Override
				protected <N extends Node> N cache(N node, N replacement) {
					final Node nsource = annotated.sourceOf(node);
					if (replacement!=nsource) source.put(replacement, nsource);
					return super.cache(node, replacement);
				}
			};
			final Formula inlined = annotated.node().accept(inliner);
			return inlined==annotated.node() ? annotated : new AnnotatedNode<Formula>(inlined,source);
		} else {
			final PredicateInliner inliner = new PredicateInliner(annotated, broken){};
			final Formula inlined = annotated.node().accept(inliner);
			return inlined==annotated.node() ? annotated : new AnnotatedNode<Formula>(inlined);
		}
	}
	
	/** 
	 * Calls lookup(pred) and returns the cached value, if any.  
	 * If a replacement has not been cached and symmetry was broken
	 * on the given predicate, replacement is TRUE.  If symmetry was not broken
	 * on the predicate, replacement the result of visiting pred.toConstraints().
	 * The replacement formula is cached and returned.
	 * @return p in broken => TRUE else pred.toConstraints().accept(this)
	 */
	public Formula visit(RelationPredicate pred) {
		Formula ret = lookup(pred);
		if (ret!=null) return ret;
		return broken.contains(pred) ? 
			   cache(pred, Formula.TRUE) : 
			   cache(pred, pred.toConstraints().accept(this));
	}

	
}
