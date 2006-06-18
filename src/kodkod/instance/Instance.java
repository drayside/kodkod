/*
 * Instance.java
 * Created on May 19, 2005
 */
package kodkod.instance;

import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

import kodkod.ast.Relation;


/**
 * Represents a model (an instance) of a relational formula, which is a mapping
 * from {@link kodkod.ast.Relation relations} to {@link kodkod.instance.TupleSet sets of tuples}
 * drawn from a given {@link kodkod.instance.Universe universe}.
 * 
 * @specfield universe: Universe
 * @specfield relations: set Relation
 * @specfield tuples: relations -> set Tuple
 * @invariant all r: tuples.Tuple | r.arity = tuples[r].arity && tuples[r].universe = universe
 * 
 * @author Emina Torlak
 */
public final class Instance implements Iterable<Relation> {
	private final Map<Relation, TupleSet> tuples;
	private final Universe universe;
	
	/**
	 * Constructs an empty instance over the given universe
	 * 
	 * @effects this.universe' = universe && no this.relations' 
	 * @throws NullPointerException - universe = null
	 */
	public Instance(final Universe universe) {
		if (universe==null) throw new NullPointerException("universe=null");
		this.universe = universe;
		this.tuples = new LinkedHashMap<Relation, TupleSet>();
	}
	
	/**
	 * Returns the universe from which the tuples in this instance
	 * are drawn.
	 * @return this.universe
	 */
	public Universe universe() {
		return universe;
	}
	
	/** 
	 * Returns true if this instance maps this relation to a set
	 * of tuples; otherwise returns false.
	 * @return r in this.relations
	 */
	public boolean contains(Relation relation) {
		return tuples.containsKey(relation);
	}
	
	/**
	 * Returns the relations mapped by this instance.
	 * @return this.relations
	 */
	public Set<Relation> relations() {
		return Collections.unmodifiableSet(tuples.keySet());
	}
	
	/**
	 * Returns an iterator over the Relations in this instance.
	 * @return an iterator over this.relations
	 */
	public Iterator<Relation> iterator() {
		return relations().iterator();
	}
	
	/**
	 * Maps the given relation to the given tuple set.  If the relation
	 * is already mapped by this BasicInstance, throws an IllegalStateException
	 * @effects this.tuples' = this.tuples + relation->s
	 * @throws NullPointerException - relation = null || s = null
	 * @throws IllegalStateException - r in this.relations
	 * @throws IllegalArgumentException - relation.arity != s.arity
	 * @throws IllegalArgumentException - s.universe != this.universe
	 */
	public void add(final Relation relation, TupleSet s) {
		if (contains(relation))
			throw new IllegalStateException("r in this.relations");
		if (!s.universe().equals(universe))
			throw new IllegalArgumentException("s.universe!=this.universe");
		if (relation.arity()!=s.arity())
			throw new IllegalArgumentException("relation.arity!=s.arity");
		tuples.put(relation, s);
	}

	/**
	 * Returns the (possibly unmodifiable) set of tuples assigned to the given relation by this Model.
	 * If the relation is not mapped by the model, null is returned.
	 * 
	 * @return this.tuples[relation]
	 */
	public TupleSet tuples(Relation relation) {
		return tuples.get(relation);
	}
	
	
	public String toString() {
		return tuples.toString();
	}

}
