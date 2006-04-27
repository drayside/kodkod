/*
 * BooleanVariableAllocator.java
 * Created on Jun 29, 2005
 */
package kodkod.engine.fol2sat;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import kodkod.ast.Relation;
import kodkod.ast.RelationPredicate;
import kodkod.engine.bool.BooleanConstant;
import kodkod.engine.bool.BooleanFactory;
import kodkod.engine.bool.BooleanMatrix;
import kodkod.engine.bool.Dimensions;
import kodkod.instance.Bounds;
import kodkod.instance.Universe;
import kodkod.util.collections.IdentityHashSet;
import kodkod.util.ints.IntIterator;
import kodkod.util.ints.IntRange;
import kodkod.util.ints.IntSet;
import kodkod.util.ints.Ints;


/**
 * Allocates {@link kodkod.engine.bool.BooleanVariable boolean variables} to 
 * {@link kodkod.ast.Relation relations}.  The literals of variables 
 * assigned to a given {@link kodkod.ast.Relation relation} form a contiguous 
 * range of integers.  All allocated variables are produced by the same 
 * {@link kodkod.engine.bool.BooleanFactory boolean factory}.
 * 
 * @specfield factory: BooleanFactory
 * @specfield bounds: Bounds
 * @specfield formulas: Relation -> BooleanMatrix
 * @invariant formulas[relation] in factory.components
 * 
 * @author Emina Torlak
 */
final class BooleanVariableAllocator extends BooleanFormulaAllocator {
	private final BooleanFactory factory;
	private final Bounds bounds;
	/* maps a relation to the intrange whose minimum and maximum values represent
	 * the minimum and maximum literal of the variables allocated to represent
	 * that relation.
	 */
	private final Map<Relation, IntRange> literals; 

	
	/* relations that are functions, which can
	 * therefore be compactly represented.
	 */
	private final Set<Relation> functions;
	
	/**  
	 * Constructs a new variable allocator for the given Bounds.  
	 * The function predicates are used to determine which relations 
	 * can be compactly represented.
	 * 
	 * @requires preds.relation in bounds.relations
	 * @effects no this.formulas' 
	 */
	BooleanVariableAllocator(Bounds bounds, Set<RelationPredicate.Function> preds) {
		this.bounds = bounds;
		this.literals = new HashMap<Relation,IntRange>();
		this.factory = BooleanFactory.factory(assignLiterals(bounds,literals));
		this.functions = Collections.unmodifiableSet(extractRelations(preds));
	}
	
	/**
	 * @return preds.relation
	 */
	private static Set<Relation> extractRelations(Set<RelationPredicate.Function> preds) {
		final Set<Relation> rels = new IdentityHashSet<Relation>(preds.size());
		for(RelationPredicate p : preds)
			rels.add(p.relation());
		return rels;
	}
	
	/**
	 * Determines the ranges of literals that need to be assigned to each
	 * relation in order to represent it as a matrix of boolean values.
	 * Returns the total number of literals needed to represent all relations.
	 */
	private static int assignLiterals(Bounds bounds, Map<Relation, IntRange> literals) {
		int maxLit = 1;
		for(Relation r : bounds.relations()) {
			int rLits = bounds.upperBound(r).size() - bounds.lowerBound(r).size();
			if (rLits > 0) {
				literals.put(r, Ints.range(maxLit, maxLit + rLits - 1));
				maxLit += rLits;
			}
		}
		return maxLit-1;
	}
	
	@Override
	Universe universe() { return bounds.universe(); }
	
	@Override
	public final BooleanFactory factory() { return factory; }
	
	/**
	 * Returns a mapping of relations with non-exact bounds to the literals
	 * assigned to it by this allocator.
	 * @return a mapping of relations with non-exact bounds to the literals
	 * assigned to it by this allocator.
	 */
	Map<Relation, IntRange> allocationMap() {
		return literals;
	}
	
	/**
	 * Returns the relations that are functions.
	 * @return the relations that are functions.
	 */
	Set<Relation> functions() {
		return functions;
	}
	/**
	 * @return this.bounds
	 */
	public Bounds bounds() { return bounds; }
	
	
	/**
	 * @throws IllegalArgumentException - !this.model.contains(r)
	 */
	private void validate(Relation r) {
		if (!bounds.relations().contains(r)) {
			throw new IllegalArgumentException("!this.model.contains(r) : r = " + r);
		}
	}
	/**
	 * Returns a {@link kodkod.engine.bool.BooleanMatrix matrix m} of 
	 * {@link kodkod.engine.bool.BooleanVariable boolean variables} representing
	 * the specified relation.  The allocated matrix is sparse, with the 
	 * non-empty region given by the space to which this.model maps the relation.  
	 * The indices in the non-empty region that correspond to tuples to which  
	 * this.model maps the relation are assigned the value BooleanConstant.TRUE.   
	 * 
	 * @return {m : BooleanMatrix | m.zero = FALSE && m.elements[int] in this.dag.nodes }
	 * @effects no this.formulas[r] => this.formulas' = this.formulas + r->m
	 * @throws NullPointerException - r = null
	 * @throws IllegalArgumentException - !this.bounds.contains(r)
	 */
	public BooleanMatrix allocate(final Relation r) {
		validate(r);

			
		int varId = literals.containsKey(r) ? literals.get(r).min() : 0; // no vars allocated to r
		
		final IntSet lowerBound = bounds.lowerBound(r).indexView();
				
		final BooleanMatrix m = factory.matrix(Dimensions.square(r.arity(), bounds.universe().size()), BooleanConstant.FALSE);
		
		for (IntIterator indeces = bounds.upperBound(r).indexView().iterator(); indeces.hasNext();) {
			int tupleIndex = indeces.nextInt();
			if (lowerBound.contains(tupleIndex)) m.set(tupleIndex, BooleanConstant.TRUE);
			else m.set(tupleIndex, factory.variable(varId++));
		}
		
		return m;
	}

}
