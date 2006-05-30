package kodkod.instance;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

import kodkod.ast.Relation;


/**
 * <p>A Bounds object maps a {@link kodkod.ast.Relation relation} <i>r</i> to two
 * {@link kodkod.instance.TupleSet sets of tuples}, <i>rL</i> and <i>rU</i>, which represent the lower and upper
 * bounds on the {@link kodkod.instance.Tuple set of tuples} to which an {@link kodkod.instance.Instance instance}
 * based on these bounds may map <i>r</i>.  The set <i>rL</i> represents all the tuples
 * that a given relation <i>must</i> contain.  The set <i>rU</i> represents all the tuples
 * that a relation <i>may</i> contain.  All bounding sets range over the same {@link kodkod.instance.Universe universe}.   
 * </p>
 * @specfield universe: Universe
 * @specfield relations: set Relation
 * @specfield lowerBound: relations -> one TupleSet
 * @specfield upperBound: relations -> one TupleSet
 * @invariant lowerBound[relations].universe = upperBound[relations].universe = universe
 * @invariant all r: relations | lowerBound[r].arity = upperBound[r].arity = r.arity
 * @invariant all r: relations | lowerBound[r].tuples in upperBound[r].tuples       
 * @author Emina Torlak
 **/
public final class Bounds implements Cloneable {
	private final TupleFactory factory;
	private final Map<Relation, BoundPair> bounds;
	
	/**
	 * Constructs a Bounds object with the given factory and mappings.
	 */
	private Bounds(TupleFactory factory, Map<Relation, BoundPair> bounds) {
		this.factory = factory;
		this.bounds = bounds;
	}
	
	/**
	 * Constructs new Bounds over the given universe.
	 * @effects this.universe' = universe && no this.relations'
	 * @throws NullPointerException - universe = null
	 */
	public Bounds(Universe universe) {
		this.factory = universe.factory();
		bounds = new LinkedHashMap<Relation, BoundPair>();
	}
	
	/**
	 * Returns this.universe.
	 * @return this.universe
	 */
	public Universe universe() { return factory.universe(); }
	
	
	/**
	 * Returns the set of all relations bound by this Bounds.
	 * The returned set does not support the add operation.
	 * It supports removal iff this is not an unmodifiable
	 * Bounds.
	 * @return this.relations
	 */
	public Set<Relation> relations() {
		return bounds.keySet();
	}
	
	/**
	 * Returns the set of tuples that r must contain (the lower bound on r's contents).
	 * If r is not mapped by this, null is returned.
	 * @return r in this.relations => lowerBound[r], null
	 */
	public TupleSet lowerBound(Relation r) {
		if (bounds.containsKey(r)) {
			return bounds.get(r).lower;
		}
		return null;
	}
	
	/**
	 * Returns the set of tuples that r may contain (the upper bound on r's contents).
	 * If r is not mapped by this, null is returned.
	 * @return r in this.relations => upperBound[r], null
	 */
	public TupleSet upperBound(Relation r) {
		if (bounds.containsKey(r)) {
			return bounds.get(r).upper;
		}
		return null;
	}
	
	/**
	 * @throws IllegalArgumentException - arity != bound.arity
	 * @throws IllegalArgumentException - bound.universe != this.universe
	 */
	private void checkBound(int arity, TupleSet bound) {
		if (arity != bound.arity())
			throw new IllegalArgumentException("bound.arity != r.arity");
		if (!bound.universe().equals(factory.universe()))
			throw new IllegalArgumentException("bound.universe != this.universe");	
	}
	
	/**
	 * Sets both the lower and upper bounds of the given relation to 
	 * the given set of tuples. 
	 * 
	 * @requires tuples.arity = r.arity && tuples.universe = this.universe
	 * @effects this.relations' = this.relations + r 
	 *          this.lowerBound' = this.lowerBound' ++ r->tuples &&
	 *          this.upperBound' = this.lowerBound' ++ r->tuples
	 * @throws NullPointerException - r = null || tuples = null 
	 * @throws IllegalArgumentException - tuples.arity != r.arity || tuples.universe != this.universe
	 */
	public void boundExactly(Relation r, TupleSet tuples) {
		checkBound(r.arity(), tuples);
		final TupleSet unmodifiableTuplesCopy = tuples.clone().unmodifiableView();
		bounds.put(r, new BoundPair(unmodifiableTuplesCopy, unmodifiableTuplesCopy));
	}
	
	/**
	 * Sets the lower and upper bounds for the given relation. 
	 * 
	 * @requires lower.tuples in upper.tuples && lower.arity = upper.arity = r.arity &&
	 *           lower.universe = upper.universe = this.universe 
	 * @effects this.relations' = this.relations + r &&
	 *          this.lowerBound' = this.lowerBound ++ r->lower &&
	 *          this.upperBound' = this.upperBound ++ r->upper
	 * @throws NullPointerException - r = null || lower = null || upper = null
	 * @throws IllegalArgumentException - lower.arity != r.arity || upper.arity != r.arity
	 * @throws IllegalArgumentException - lower.universe != this.universe || upper.universe != this.universe
	 * @throws IllegalArgumentException - lower.tuples !in upper.tuples                               
	 */
	public void bound(Relation r, TupleSet lower, TupleSet upper) {
		if (!upper.containsAll(lower))
			throw new IllegalArgumentException("lower.tuples !in upper.tuples");
		if (upper.size()==lower.size()) { 
			// upper.containsAll(lower) && upper.size()==lower.size() => upper.equals(lower)
			boundExactly(r, lower);
		} else {
			checkBound(r.arity(), lower);
			checkBound(r.arity(), upper);		
			bounds.put(r, new BoundPair(lower.clone().unmodifiableView(), upper.clone().unmodifiableView()));
		}
	}
	
	/**
	 * Makes the specified tupleset the upper bound on the contents of the given relation.  
	 * The lower bound automatically becomen an empty tupleset with the same arity as
	 * the relation. 
	 * 
	 * @requires upper.arity = r.arity && upper.universe = this.universe
	 * @effects this.relations' = this.relations + r 
	 *          this.lowerBound' = this.lowerBound ++ r->{s: TupleSet | s.universe = this.universe && s.arity = r.arity && no s.tuples} && 
	 *          this.upperBound' = this.upperBound ++ r->upper
	 * @throws NullPointerException - r = null || upper = null 
	 * @throws IllegalArgumentException - upper.arity != r.arity || upper.universe != this.universe
	 */
	public void bound(Relation r, TupleSet upper) {
		checkBound(r.arity(), upper);
		bounds.put(r, new BoundPair(factory.noneOf(r.arity()).unmodifiableView(), upper.clone().unmodifiableView()));
	}
	
	

	/**
	 * Returns an unmodifiable view of this Bounds object.
	 * @return an unmodifiable view of his Bounds object.
	 */
	public Bounds unmodifiableView() {
		return new Bounds(factory, Collections.unmodifiableMap(bounds));
	}
	
	/**
	 * Returns a deep copy of this Bounds object.
	 * @return a deep copy of this Bounds object.
	 */
	public Bounds clone() {
		return new Bounds(factory, new LinkedHashMap<Relation, BoundPair>(bounds));
	}
	
	public String toString() {
		return bounds.toString();
	}
	
	/**
	 * Represents a pair of bounds, upper and lower.
	 * 
	 * @specfield lower: one TupleSet
	 * @specfield upper: one TupleSet
	 * @invariant lower.tuples in upper.tuples
	 * @invariant lower.universe = upper.universe && lower.arity = upper.arity
	 */
	private static final class BoundPair {
		final TupleSet lower;
		final TupleSet upper;
		
		/**
		 * Constructs a new BoundPair with the given lower and upper sets.
		 * @requires lower.tuples in upper.tuples && lower.universe = upper.universe && lower.arity = upper.arity
		 * @effects this.lower' = lower && this.upper' = upper
		 */
		BoundPair(TupleSet lower, TupleSet upper) {	
			this.lower = lower;
			this.upper = upper;
		}
		
		public String toString() {
			return "[" + lower + ", " + upper + "]";
		}
	}
}
