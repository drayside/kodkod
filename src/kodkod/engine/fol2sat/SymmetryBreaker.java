package kodkod.engine.fol2sat;

import static kodkod.ast.RelationPredicate.Name.ACYCLIC;
import static kodkod.ast.RelationPredicate.Name.TOTAL_ORDERING;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import kodkod.ast.Relation;
import kodkod.ast.RelationPredicate;
import kodkod.engine.bool.BooleanAccumulator;
import kodkod.engine.bool.BooleanConstant;
import kodkod.engine.bool.BooleanFactory;
import kodkod.engine.bool.BooleanMatrix;
import kodkod.engine.bool.BooleanValue;
import kodkod.engine.bool.Operator;
import kodkod.engine.config.Options;
import kodkod.instance.Bounds;
import kodkod.instance.TupleFactory;
import kodkod.util.collections.IdentityHashSet;
import kodkod.util.ints.IndexedEntry;
import kodkod.util.ints.IntIterator;
import kodkod.util.ints.IntSet;
import kodkod.util.ints.Ints;

/**
 * Breaks symmetries for a given problem.  Symmetries
 * are broken for total orders, acyclic relations, and 
 * via a generic lex-leader predicate.
 * 
 * @specfield bounds: Bounds // problem bounds
 * @specfield options: Options
 * @specfield symmetries: set IntSet 
 * @specfield broken: set RelationPredicate 
 * @author Emina Torlak
 */
final class SymmetryBreaker {
	private final Bounds bounds;
	private final Options options;
	private final Set<IntSet> symmetries;
	private final int usize;
	private final Set<RelationPredicate> broken;
	
	/**
	 * Constructs a  new symmetry breaker for the given bounds and options.
	 * @effects this.bounds' = bounds && this.options' = options && 
	 * this.symmetries' = SymmetryDetector.partition(bounds) && no this.broken'
	 */
	SymmetryBreaker(Bounds bounds, Options options) {
		this.bounds = bounds;
		this.options = options;
		this.usize = bounds.universe().size();
		options.reporter().detectingSymmetries();
		this.symmetries = SymmetryDetector.partition(bounds);
		this.broken = new IdentityHashSet<RelationPredicate>();
	}
	
	/**
	 * Breaks predicate symmetries on this.bounds based on the given map from
	 * RelationPredicate.Name to sets of RelationPredicates.
	 * @requires RelationPredicate.Name.preds.*children & Relation in this.bounds.relations
	 * @requires all n: preds.RelationPredicate | preds[n].name() = n
	 * @effects this.bounds is modified, if possible, to have exact bounds for some of the total
	 * orderings in broken and to have halved upper bounds for some of the acyclic relations in broken
	 * @effects this.symmetries is modified to no longer contain the partitions on which symmetries have been broken
	 * @effects this.broken is modified to contain the predicates on which symmetries have been broken
	 */
	final void breakPredicateSymmetries(Map<RelationPredicate.Name, Set<RelationPredicate>> preds) {
		final Set<RelationPredicate> totals = preds.get(TOTAL_ORDERING);
		final Set<RelationPredicate> acyclics = preds.get(ACYCLIC);
		
		for(RelationPredicate.TotalOrdering pred : sort(totals.toArray(new RelationPredicate.TotalOrdering[totals.size()]))) {
			if (breakTotalOrder(pred))
				broken.add(pred);
		}
		
		for(RelationPredicate.Acyclic pred : sort(acyclics.toArray(new RelationPredicate.Acyclic[acyclics.size()]))) {
			if (breakAcyclic(pred))
				broken.add(pred);
		}
	}
	
	/**
	 * Returns the set of all predicates for which symmetries have been broken so far.
	 * @return this.broken
	 */
	final Set<RelationPredicate> broken() {
		return broken;
	}
	
	/**
	 * Generates a lex leader symmetry breaking predicate for this.symmetries 
	 * (if any), using the specified leaf interpreter and this.options.symmetryBreaking().
	 * @requires interpreter.relations in this.bounds.relations
	 * @effects no this.symmetries'
	 * @return a symmetry breaking predicate for this.symmetries
	 */
	final BooleanValue breakLexSymmetries(LeafInterpreter interpreter) {
		final int predLength = options.symmetryBreaking();
		if (symmetries.isEmpty() || predLength==0) return BooleanConstant.TRUE;
		
		final List<RelationParts> relParts = relParts();
		final BooleanFactory factory = interpreter.factory();
		final BooleanAccumulator sbp = BooleanAccumulator.treeGate(Operator.AND);
		final List<BooleanValue> original = new ArrayList<BooleanValue>(predLength);
		final List<BooleanValue> permuted = new ArrayList<BooleanValue>(predLength);
		
		for(IntSet sym : symmetries) {
		
			IntIterator indeces = sym.iterator();
			for(int prevIndex = indeces.nextInt(); indeces.hasNext(); ) {
				int curIndex = indeces.nextInt();
				for(Iterator<RelationParts> rIter = relParts.iterator(); rIter.hasNext() && original.size() < predLength;) {
					
					RelationParts rparts = rIter.next();
					Relation r = rparts.relation;
					
					if (!rparts.representatives.contains(sym.min())) continue;  // r does not range over sym
					
					BooleanMatrix m = interpreter.interpret(r);
					for(IndexedEntry<BooleanValue> entry : m) {
						int permIndex = permutation(r.arity(), entry.index(), prevIndex, curIndex);
						BooleanValue permValue = m.get(permIndex);
						if (permIndex==entry.index() || atSameIndex(original, permValue, permuted, entry.value()))
							continue;
						
						original.add(entry.value());
						permuted.add(permValue);			
					}
				}
								
				sbp.add(leq(factory, original, permuted));
				original.clear();
				permuted.clear();
				prevIndex = curIndex;
			}
		}
		
		symmetries.clear();
		return factory.accumulate(sbp);
	}
	
	/**
	 * Returns a list of RelationParts that map each non-constant r in this.bounds.relations to
	 * the representatives of the sets from this.symmetries contained in the upper bound of r.  
	 * The entries are sorted by relations' arities and names.
	 * @return a list of RelationParts that contains an entry for each non-constant r in this.bounds.relations and
	 * the representatives of sets from this.symmetries contained in the upper bound of r.
	 */
	private List<RelationParts> relParts() {
		final List<RelationParts> relParts = new ArrayList<RelationParts>(bounds.relations().size());
		for(Relation r: bounds.relations()) {		
			IntSet upper = bounds.upperBound(r).indexView();
			if (upper.size()==bounds.lowerBound(r).size()) continue; // skip constant relation
			IntSet reps = Ints.bestSet(usize);
			for(IntIterator tuples = upper.iterator(); tuples.hasNext(); ) {
				for(int tIndex = tuples.nextInt(), i = r.arity(); i > 0; i--, tIndex /= usize) {
					for(IntSet symm : symmetries) {
						if (symm.contains(tIndex%usize)) {
							reps.add(symm.min());
							break;
						}
					}
				}
			}
			relParts.add(new RelationParts(r, reps));
		}
		final Comparator<RelationParts> cmp = new Comparator<RelationParts>() {
			public int compare(RelationParts o1, RelationParts o2) {
				final int acmp = o1.relation.arity() - o2.relation.arity();
				return acmp!=0 ? acmp : String.valueOf(o1.relation.name()).compareTo(String.valueOf(o2.relation.name()));
			}
		};
		Collections.sort(relParts, cmp);
		return relParts;
	}
	
	/**
	 * Returns a BooleanValue that is true iff the string of bits
	 * represented by l0 is lexicographically less than or equal
	 * to the string of bits reprented by l1.
	 * @requires l0.size()==l1.size()
	 * @return a circuit that compares l0 and l1
	 */
	private final BooleanValue leq(BooleanFactory f, List<BooleanValue> l0, List<BooleanValue> l1) {
		final BooleanAccumulator cmp = BooleanAccumulator.treeGate(Operator.AND);
		BooleanValue prevEquals = BooleanConstant.TRUE;
		for(int i = 0; i < l0.size(); i++) {
			cmp.add(f.implies(prevEquals, f.implies(l0.get(i), l1.get(i))));
			prevEquals = f.and(prevEquals, f.iff(l0.get(i), l1.get(i)));
		}
		return f.accumulate(cmp);
	}
	
	/**
	 * Let t be the tuple represent by the given arity and tupleIndex.
	 * This method returns the tuple index of the tuple t' such t'
	 * is equal to t with each occurence of atomIndex0
	 * replaced by atomIndex1 and vice versa.
	 * @return the index of the tuple to which the given symmetry
	 * maps the tuple specified by arith and tupleIndex
	 */
	private final int permutation(int arity, int tupleIndex, int atomIndex0, int atomIndex1) {
		int permIndex = 0;
		for(int u = 1; arity > 0; arity--, tupleIndex /= usize, u *= usize ) {
			int atomIndex = tupleIndex%usize;
			if (atomIndex==atomIndex0)
				permIndex += atomIndex1 * u;
			else if (atomIndex==atomIndex1) {
				permIndex += atomIndex0 * u;
			} else {
				permIndex += atomIndex * u;
			}
		}
		return permIndex;
	}
	
	/**
	 * Returns true if there is some index i such that l0[i] = v0 and l1[i] = v1.
	 * @requires l0.size()=l1.size()
	 * @return some i: int | l0[i] = v0 && l1[i] = v1
	 */
	private static boolean atSameIndex(List<BooleanValue> l0, BooleanValue v0, List<BooleanValue> l1, BooleanValue v1) {
		for(int i = 0; i < l0.size(); i++) {
			if (l0.get(i).equals(v0) && l1.get(i).equals(v1))
				return true;
		}
		return false;
	}
	
	/**
	 * Sorts the predicates in the given array in the ascending order of 
	 * the names of the predicates' relations, and returns it.
	 * @return broken'
	 * @effects all i: [0..preds.size()) | all j: [0..i) | 
	 *            broken[j].relation.name <= broken[i].relation.name 
	 */
	private static final <P extends RelationPredicate> P[] sort(final P[] preds) {
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
	 * from this.symmetries.
	 * @return true if the bounds for the acyclic.relation were reduced, otherwise returns false
	 * @effects (some s: this.parts[int] | 
	 *            this.bounds.upperBound[acyclic.relation].project([0..1]).indexView() = s) =>
	 *          (this.bounds.upperBound' = this.bounds.upperBound ++ 
	 *           acyclic.relation -> {t: Tuple | t.arity = 2 && t.atoms[0] < t.atoms[1] }) &&
	 *          (this.parts'[int] = 
	 *           this.parts[int] - this.bounds.upperBound[acyclic.relation].project(0).indexView())
	 */
	private final boolean breakAcyclic(RelationPredicate.Acyclic acyclic) {
		final IntSet[] colParts = symmetricColumnPartitions(acyclic.relation());
		if (colParts!=null) {
			final IntSet upper = bounds.upperBound(acyclic.relation()).indexView();
			final IntSet reduced = Ints.bestSet(usize*usize);
			for(IntIterator tuples = upper.iterator(); tuples.hasNext(); ) {
				int tuple = tuples.nextInt();
				int mirror = (tuple / usize) + (tuple % usize)*usize;
				if (tuple != mirror) {
					if (!upper.contains(mirror)) return false;
					if (!reduced.contains(mirror))
						reduced.add(tuple);	
				}
			}
			bounds.bound(acyclic.relation(), bounds.universe().factory().setOf(2, reduced));
			// remove the column partitions from the set of symmetric partitions
			for(Iterator<IntSet> symIter = symmetries.iterator(); symIter.hasNext(); ) {
				IntSet part = symIter.next();
				if (part.contains(colParts[0].min()) || symmetries.contains(colParts[1].min())) {
					symIter.remove();
				}			
			}
			return true;
		}
		return false;
	}
	
	/**
	 * If possible, reduces the upper bounds of total.relation, total.ordered, total.first,
	 * and total.last, to exact bounds and removes the partition comprising the upper bound 
	 * of total.ordered from this.symmetries.
	 * @return true if the bounds for total.(relation+ordered+first+last) were reduced, otherwise returns false.
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
	private final boolean breakTotalOrder(RelationPredicate.TotalOrdering total) {
		final IntSet ordered = bounds.upperBound(total.ordered()).indexView();
				
		if (symmetricColumnPartitions(total.ordered())!=null && 
			bounds.upperBound(total.first()).indexView().contains(ordered.min()) && 
			bounds.upperBound(total.last()).indexView().contains(ordered.max())) {
			// construct the natural ordering that corresponds to the ordering of the atoms in the universe
			final IntSet ordering = Ints.bestSet(usize*usize);
			int prev = ordered.min();
			for(IntIterator atoms = ordered.iterator(prev+1, usize); atoms.hasNext(); ) {
				int next = atoms.nextInt();
				ordering.add(prev*usize + next);
				prev = next;
			}
			if (ordering.containsAll(bounds.lowerBound(total.relation()).indexView()) &&
				bounds.upperBound(total.relation()).indexView().containsAll(ordering)) {
				final TupleFactory f = bounds.universe().factory();
				bounds.boundExactly(total.relation(), f.setOf(2, ordering));
				bounds.boundExactly(total.ordered(), bounds.upperBound(total.ordered()));
				bounds.boundExactly(total.first(), f.setOf(f.tuple(1, ordered.min())));
				bounds.boundExactly(total.last(), f.setOf(f.tuple(1, ordered.max())));
				// remove the ordered partition from the set of symmetric partitions
				for(Iterator<IntSet> symIter = symmetries.iterator(); symIter.hasNext(); ) {
					if (symIter.next().contains(ordered.min())) {
						symIter.remove();
						break;
					}			
				}
//				System.out.println("breaking: " + total + ", " + bounds);				
				return true;
			}
		}
		
		return false;
	}
	
	/**
	 * If all columns of the upper bound of r are symmetric partitions, 
	 * those partitions are returned.  Otherwise null is returned.
	 * @return (all i: [0..r.arity) | some s: symmetries[int] |
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
			for(IntSet part : symmetries) {
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
	
	/**
	 * An entry for a relation and the representative (least atom) for each
	 * symmetry class in the relation's upper bound.
	 */
	private static final class RelationParts {
		final Relation relation;
		final IntSet representatives;
		
		RelationParts(Relation relation, IntSet representatives) {
			this.relation = relation;
			this.representatives = representatives;
		}
	}
}
