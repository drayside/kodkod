package kodkod.engine.fol2sat;

import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Set;

import kodkod.ast.Relation;
import kodkod.ast.RelationPredicate;
import static kodkod.ast.RelationPredicate.Name.*;
import kodkod.instance.Bounds;
import kodkod.instance.TupleFactory;
import kodkod.instance.TupleSet;
import kodkod.util.collections.IdentityHashSet;
import kodkod.util.ints.IntIterator;
import kodkod.util.ints.IntSet;
import kodkod.util.ints.Ints;

/**
 * <p>A BoundsOptimizer minimizes the number and size of mappings
 * in a given Bounds object.  Given a Bounds B, a set of relations R, a set of integers I,
 * a set of RelationPredicate.TotalOrdering T, and a set of RelationPredicate.Acyclic A,
 * a BoundsOptimizer first reduces B.relations to R and B.intBound.TupleSet to I.  Then, it partitions B.universe
 * into sets of equivalent atoms, based on the bounds of the relations in R and the integers in I.  
 * (Atoms within each partition 
 * can be permuted without affecting the meaning of a formula whose relation leaves are
 * a subset of R.)   </p>
 * 
 * <p>Using the partitioning, the optimizer attempts to minimize B by breaking symmetries 
 * on relations constrained by predicates in the sets T and A:  exact bounds are given to total
 * orders and upper bounds on acyclic relations are halved whenever the equivalence and 
 * fixness constraints on atoms allow it.  Specifically, given a total ordering predicate
 * Tp, symmetry is broken on Tp.first, Tp.last, Tp.relation, and Tp.ordered iff 
 * Tp.ordered is a partition of B.universe and symmetry has not already been broken on it.
 * Given an acyclic predicate Ap, symmetry is broken on Ap.relation iff its domain and
 * range are each partitions of B.universe and symmetry has been broken on neither.     </p>
 * 
 * <p>Note that in subsequent comments, atoms are identified with their index in bounds.universe.</p>
 *              
 * @author Emina Torlak
 */
final class BoundsOptimizer {
	private final Bounds bounds;
	private final List<IntSet> parts;
	private final int usize;
	
	/**
	 * Constructs a new BoundsOptimizer that reduces bounds.relations to relations and
	 * bounds.intBound.TupleSet to ints and initializes partitions to an empty list.
	 */
	private BoundsOptimizer(Bounds bounds, Set<Relation> relations, IntSet ints) {
		this.bounds = bounds;
		bounds.relations().retainAll(relations);
		if (relations.size() != bounds.relations().size()) {
			relations.removeAll(bounds.relations());
			throw new UnboundLeafException("Unbound relation", relations.iterator().next());
		}
		bounds.ints().retainAll(ints);
		if (ints.size() != bounds.ints().size()) {
			ints.removeAll(bounds.ints());
			throw new IllegalArgumentException("Unbound ints: " + ints);
		}
		this.parts = new LinkedList<IntSet>();
		this.usize = bounds.universe().size();
	}
	
	/**
	 * Optimizes the given bounds as described above, and returns
	 * the partitions on which symmetries have not been broken.
	 * @return the partitions on which symmetries have not been broken
	 * @effects optimizes the given bounds
	 * @throws NullPointerException - any of the arguments are null
	 * @throws UnsupportedOperationException - bounds is unmodifiable
	 * @throws UnboundLeafException - some relations - bounds.relations
	 * @throws IllegalArgumentException - some ints - bounds.intBound.TupleSet
	 * @throws IllegalArgumentException - some relations - preds[TOTAL_ORDERING].(relation + first + last + ordered)
	 * @throws IllegalArgumentException - some relations - preds[ACYCLIC].relation
	 */
	static Set<IntSet> optimize(Bounds bounds, Set<Relation> relations, IntSet ints,
			Map<RelationPredicate.Name, Set<RelationPredicate>> preds) {
		final BoundsOptimizer opt = new BoundsOptimizer(bounds, relations, ints);
		opt.computePartitions();
		final Set<RelationPredicate> totals = preds.get(TOTAL_ORDERING);
		final Set<RelationPredicate> acyclics = preds.get(ACYCLIC);
		
		for(RelationPredicate.TotalOrdering pred : 
			opt.sort(totals.toArray(new RelationPredicate.TotalOrdering[totals.size()]))) {
			opt.minimizeTotalOrder(pred);
		}
		
		for(RelationPredicate.Acyclic pred : 
			opt.sort(acyclics.toArray(new RelationPredicate.Acyclic[acyclics.size()]))) {
			opt.minimizeAcyclic(pred);
		}
		
		// convert the list of remaining partitions into a set of unmodifiable partitions
		final Set<IntSet> partSet = new IdentityHashSet<IntSet>(opt.parts.size());
		for(IntSet s : opt.parts) {
			partSet.add(Ints.unmodifiableIntSet(s));
		}
		
		return partSet;
	}
	
	/*---------------SYMMETRY BREAKING FOR TOTAL ORDERS AND ACYCLIC RELATIONS----------------------*/
	/**
	 * Sorts the predicates in the given array in the ascending order of 
	 * the names of the predicates' relations, and returns it.
	 * @return preds'
	 * @effects all i: [0..preds.size()) | all j: [0..i) | 
	 *            preds[j].relation.name <= preds[i].relation.name 
	 */
	private final <P extends RelationPredicate> P[] sort(final P[] preds) {
		final Comparator<RelationPredicate> cmp = new Comparator<RelationPredicate>() {
			public int compare(RelationPredicate o1, RelationPredicate o2) {
				return String.valueOf(o1.relation().name()).compareTo(String.valueOf(o2.relation().name()));
			}
		};
		Arrays.sort(preds, cmp);
		return preds;
	}		
	
	/**
	 * If possible, reduces the upper bound of acyclic.relation to the elements above
	 * the diagonal and removes the partitions comprising the upper bound of acyclic.relation 
	 * from this.parts.
	 * @effects (some s: this.parts[int] | 
	 *            this.bounds.upperBound[acyclic.relation].project([0..1]).indexView() = s) =>
	 *          (this.bounds.upperBound' = this.bounds.upperBound ++ 
	 *           acyclic.relation -> {t: Tuple | t.arity = 2 && t.atoms[0] < t.atoms[1] }) &&
	 *          (this.parts'[int] = 
	 *           this.parts[int] - this.bounds.upperBound[acyclic.relation].project(0).indexView())
	 */
	private final void minimizeAcyclic(RelationPredicate.Acyclic acyclic) {
		final IntSet[] colParts = symmetricColumnPartitions(acyclic.relation());
		if (	colParts!=null) {
			final IntSet upper = bounds.upperBound(acyclic.relation()).indexView();
			final IntSet reduced = Ints.bestSet(usize*usize);
			for(IntIterator tuples = upper.iterator(); tuples.hasNext(); ) {
				int tuple = tuples.nextInt();
				int mirror = (tuple / usize) + (tuple % usize)*usize;
				if (tuple != mirror) {
					if (!upper.contains(mirror)) return;
					if (!reduced.contains(mirror))
						reduced.add(tuple);	
				}
			}
			bounds.bound(acyclic.relation(), bounds.universe().factory().setOf(2, reduced));
			// remove the column partitions from the set of symmetric partitions
			for(Iterator<IntSet> partIter = parts.iterator(); partIter.hasNext(); ) {
				IntSet part = partIter.next();
				if (part.contains(colParts[0].min()) || parts.contains(colParts[1].min())) {
					partIter.remove();
				}			
			}
		}
	}
	
	/**
	 * If possible, reduces the upper bounds of total.relation, total.ordered, total.first,
	 * and total.last, to exact bounds and removes the partition comprising the upper bound 
	 * of total.ordered from this.parts.
	 * @effects (some s: this.parts[int] | bounds.upperBound[total.ordered].indexView() = s) =>
	 *          this.bounds.upperBound' = this.bounds.upperBound ++ 
	 *           (total.relation -> {t: Tuple | t.arity = 2 && t.atoms[0] < t.atoms[1] 
	 *                                && t.atoms in this.bounds.upperBound[total.ordered] } + 
	 *            total.first -> this.bounds.upperBound[total.ordered].indexView().min() + 
	 *            total.last -> this.bounds.upperBound[total.ordered].indexView().max()) &&
	 *          (this.bounds.lowerBound' = this.bounds.lowerBound ++ 
	 *            (total.(relation + ordered + first + last))<:this.bounds.upperBound') && 
	 *          (this.parts'[int] = 
	 *           this.parts[int] - this.bounds.upperBound[total.ordered].indexView())
	 */
	private final void minimizeTotalOrder(RelationPredicate.TotalOrdering total) {
		final IntSet ordered = bounds.upperBound(total.ordered()).indexView();
				
		if (symmetricColumnPartitions(total.ordered())!=null && 
			bounds.upperBound(total.first()).indexView().contains(ordered.min()) && 
			bounds.upperBound(total.last()).indexView().contains(ordered.max())) {
			// construct the natural ordering that corresponds to the 
			// ordering of the atoms in the universe
			final IntSet ordering = Ints.bestSet(usize*usize);
			int prev = ordered.min();
			for(IntIterator atoms = ordered.iterator(prev+1, usize); atoms.hasNext(); ) {
				int next = atoms.nextInt();
				ordering.add(prev*usize + next);
				prev = next;
			}
			// set total.relation, total.ordered, total.first, and total.last to reflect
			// the natural ordering, if possible
			if (ordering.containsAll(bounds.lowerBound(total.relation()).indexView()) &&
				bounds.upperBound(total.relation()).indexView().containsAll(ordering)) {
				final TupleFactory f = bounds.universe().factory();
				bounds.boundExactly(total.relation(), f.setOf(2, ordering));
				bounds.boundExactly(total.ordered(), bounds.upperBound(total.ordered()));
				bounds.boundExactly(total.first(), f.setOf(f.tuple(1, ordered.min())));
				bounds.boundExactly(total.last(), f.setOf(f.tuple(1, ordered.max())));
				// remove the ordered partition from the set of symmetric partitions
				for(Iterator<IntSet> partIter = parts.iterator(); partIter.hasNext(); ) {
					if (partIter.next().contains(ordered.min())) {
						partIter.remove();
						break;
					}			
				}
//				System.out.println("breaking: " + total + ", " + bounds);				
			}
		}
	}
	
	/**
	 * If all columns of the upper bound of r are symmetric partitions, 
	 * those partitions are returned.  Otherwise null is returned.
	 * @return (all i: [0..r.arity) | some s: parts[int] |
	 *          bounds.upperBound[r].project(i).indexView() = s) =>
	 *         {colParts: [0..r.arity)->IntSet | 
	 *          all i: [0..r.arity()) | colParts[i] = bounds.upperBound[r].project(i).indexView() },
	 *         null
	 */
	private final IntSet[] symmetricColumnPartitions(Relation r) {
		final IntSet upper = bounds.upperBound(r).indexView();
		if (upper.isEmpty()) return null;
		
		final IntSet[] colParts = new IntSet[r.arity()];
		for(int i = r.arity()-1, min = upper.min(); i >= 0; i--, min /= usize) {
			for(IntSet part : parts) {
				if (part.contains(min%usize)) {
					colParts[i] = part; 
					break;
				}
			}
			if (colParts[i]==null) 
				return null;
		}
		for(IntIterator tuples = upper.iterator(); tuples.hasNext(); ) {
			for(int i = r.arity()-1, tuple = tuples.nextInt(); i >= 0; i--, tuple /= usize) {
				if (!colParts[i].contains(tuple%usize))
					return null;
			}		
		}
		return colParts;	
	}
	
	/*---------------UNIVERSE PARTITIONING METHODS----------------------*/
	/**
	 * Partitions this.bounds.universe into sets of equivalent atoms.
	 * @requires no parts[int]
	 * @effects all disj s, q: this.parts'[int] | 
	 *           some s.ints && some q.ints && (no s.ints & q.ints) &&
	 *           this.parts'[int].ints = [0..this.bounds.universe.size()) &&
	 *           (all ts: this.bounds.lowerBound[Relation] + this.bounds.upperBound[Relation] | 
	 *             all s: this.parts'[int] | all a1, a2: this.bounds.universe.atoms[s.ints] |
	 *              all t1, t2: ts.tuples | t1.atoms[0] = a1 && t2.atoms[0] = a2 =>
	 *                t1.atoms[1..ts.arity) = t1.atoms[1..ts.arity) || 
	 *                t1.atoms[1..ts.arity) = a1 && t1.atomos[1..ts.arity) = a2)
	 */
	private void computePartitions() {
		//	start with the maximum partition -- the whole universe.
		assert parts.size() == 0;
		parts.add(allOf(usize));
		if (usize==1) return; // nothing more to do 
		
		// refine the partitions based on the upper/lower bounds
		// of each relation
		final Map<IntSet, IntSet> range2domain = new HashMap<IntSet, IntSet>((usize*2) / 3);
		for(Relation r : bounds.relations()) {
			TupleSet lower = bounds.lowerBound(r);
			TupleSet upper = bounds.upperBound(r);
			refinePartitions(lower.indexView(), lower.arity(), range2domain);
			if (!lower.equals(upper))
				refinePartitions(upper.indexView(), upper.arity(), range2domain);
		
		}
		
		// refine the partitions based on the bounds of each integer
		for(IntIterator iter = bounds.ints().iterator(); iter.hasNext();) {
			TupleSet exact = bounds.exactBound(iter.nextInt());
			refinePartitions(exact.indexView(), 1, range2domain);
		}
	}
	
	/**
	 * Refines the atomic partitions in this.parts based on the contents of the given tupleset, 
	 * decomposed into its constituent IntSet and arity.  The range2domain map is used for 
	 * intermediate computations for efficiency (to avoid allocating it in each recursive call). 
	 * @requires all disj s, q: this.parts[int] | 
	 *            some s.ints && some q.ints && (no s.ints & q.ints) &&
	 *            this.parts[int].ints = [0..this.bounds.universe.size())
	 * @effects  let usize = this.bounds.universe.size(), firstColFactor = usize^(arit-1) |
	 *            all disj s, q: this.parts'[int] | 
	 *             some s.ints && some q.ints && (no s.ints & q.ints) &&
	 *             this.parts'[int].ints = [0..usize) &&
	 *             all s: this.parts'[int] | all a1, a2: this.bounds.universe.atoms[s.ints] |
	 *               all t1, t2: set.ints | t1 / firstColFactor = a1 && t2 / firstColFactor = a2 =>
	 *                 t1 % firstColFactor = t2 % firstColFactor  || 
	 *                 t1 = a1*((1 - firstColFactor) / (1 - usize)) && 
	 *                 t2 = a2*((1 - firstColFactor) / (1 - usize)))
	 */
	private void refinePartitions(IntSet set, int arity, Map<IntSet, IntSet> range2domain) {
		if (arity==1) {
			refinePartitions(set);
			return;
		}
		
		final List<IntSet> otherColumns = new LinkedList<IntSet>();
		int firstColFactor = (int) StrictMath.pow(usize, arity-1);
		IntSet firstCol = Ints.bestSet(usize);
		for(IntIterator rbIter = set.iterator(); rbIter.hasNext(); ) {
			firstCol.add(rbIter.nextInt() / firstColFactor);
		}
		refinePartitions(firstCol);
		
		int idenFactor = (1 - firstColFactor) / (1 - usize);
		for(ListIterator<IntSet> partsIter = parts.listIterator(); partsIter.hasNext(); ) {
			IntSet part = partsIter.next();
			if (firstCol.contains(part.min())) { // contains one, contains them all
				range2domain.clear();
				for(IntIterator atoms = part.iterator(); atoms.hasNext(); ) {
					int atom = atoms.nextInt();
					IntSet atomRange = Ints.bestSet(firstColFactor);
					for(IntIterator rbIter = set.iterator(atom*firstColFactor, (atom+1)*firstColFactor - 1); 
					rbIter.hasNext(); ) {
						atomRange.add(rbIter.nextInt() % firstColFactor);
					}
					IntSet atomDomain = range2domain.get(atomRange);
					if (atomDomain != null) atomDomain.add(atom);
					else range2domain.put(atomRange, oneOf(usize, atom));
				}
				partsIter.remove();
				IntSet idenPartition = Ints.bestSet(usize);
				for(Map.Entry<IntSet, IntSet> entry : range2domain.entrySet()) {
					if (entry.getValue().size()==1 && entry.getKey().size()==1 &&
						entry.getKey().min() == entry.getValue().min() * idenFactor) {
						idenPartition.add(entry.getValue().min());
					} else {
						partsIter.add(entry.getValue());
						otherColumns.add(entry.getKey());
					}
				}
				if (!idenPartition.isEmpty())
					partsIter.add(idenPartition);			
			}
		}
		
		// refine based on the remaining columns
		for(IntSet otherCol : otherColumns) {
			refinePartitions(otherCol, arity-1, range2domain);
		}
	}
	
	/**
	 * Refines the atomic partitions this.parts based on the contents of the given set.
	 * @requires all disj s, q: this.parts[int] | 
	 *            some s.ints && some q.ints && (no s.ints & q.ints) &&
	 *            this.parts[int].ints = [0..this.bounds.universe.size())
	 * @effects  all disj s, q: this.parts'[int] | 
	 *            some s.ints && some q.ints && (no s.ints & q.ints) &&
	 *            this.parts'[int].ints = [0..this.bounds.universe.size()) &&
	 *            (all i: [0..this.parts'.size()) | 
	 *             this.parts'[i].ints in set.ints || no this.parts'[i].ints & set.ints)
	 */
	private void refinePartitions(IntSet set) {
		for(ListIterator<IntSet> partsIter = parts.listIterator(); partsIter.hasNext(); ) {
			IntSet part = partsIter.next();
			IntSet intersection = Ints.bestSet(part.min(), part.max());
			intersection.addAll(part);
			intersection.retainAll(set);
			if (!intersection.isEmpty() && intersection.size() < part.size()) {
				part.removeAll(intersection);
				partsIter.add(intersection);
			}
		}
	}
	
	/**
	 * Returns an int set that contains all elements between 
	 * 0, inclusive, and size, exlusive.
	 * @requires size >= 0
	 * @return {s: IntSet | s.ints = [0..size) }
	 */
	private static final IntSet allOf(int size) {
		final IntSet set = Ints.bestSet(size);
		for(int i = 0; i < size; i++) {
			set.add(i);
		}
		return set;
	}
	
	/**
	 * Returns an IntSet that can store elements
	 * in the range [0..size), and that holds
	 * the given number.
	 * @requries 0 <= num < size
	 * @return {s: IntSet | s.ints = num } 
	 */
	private static final IntSet oneOf(int size, int num) {
		final IntSet set = Ints.bestSet(size);
		set.add(num);
		return set;
	}

}
