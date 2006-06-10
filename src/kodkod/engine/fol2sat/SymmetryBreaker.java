package kodkod.engine.fol2sat;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import kodkod.ast.Relation;
import kodkod.engine.Options;
import kodkod.engine.bool.BooleanConstant;
import kodkod.engine.bool.BooleanFactory;
import kodkod.engine.bool.BooleanMatrix;
import kodkod.engine.bool.BooleanValue;
import kodkod.engine.bool.BooleanAccumulator;
import kodkod.engine.bool.Operator;
import kodkod.instance.Bounds;
import kodkod.util.ints.IndexedEntry;
import kodkod.util.ints.IntIterator;
import kodkod.util.ints.IntSet;

/**
 * Generates a symmetry breaking predicate
 * using given allocator and symmetry information.
 * 
 * @author Emina Torlak
 */
final class SymmetryBreaker {
	/**
	 * Generates a symmetry breaking predicate for the given
	 * symmetries, using the specified allocator and options.
	 * @return a symmetry breaking predicate for the given symmetries
	 * @throws NullPointerException - any of the arguments are null
	 */
	static BooleanValue generateSBP(Set<IntSet> symmetries, BooleanVariableAllocator allocator, Options options) {
		return symmetries.isEmpty() || options.symmetryBreaking()==0 ? 
			   BooleanConstant.TRUE : 
			   (new SymmetryBreaker(symmetries,allocator,options)).lexLeaderPredicates();
	}
	
	
	private final int MAX_CMP_LENGTH;
	/* allocator used to obtain SAT encodings of relations */
	private final BooleanVariableAllocator allocator;
	/* available symmetries */
	private final List<IntSet> symmetries;
	/* stores a mapping from each Relation r to the symmetries
	 * over which its upper bound ranges. */
	private final Map<Relation, Set<IntSet>> relSymms;
	/* non-constant abv.bounds.relations sorted in the ascending of arity and name. */
	private final Relation[] sortedRels;
	/* usize = abv.bounds.universe.size */
	private final int usize;

	/**
	 * Constructs a predicate generator using the given allocator
	 * symmetry information, and Options.
	 */
	private SymmetryBreaker(Set<IntSet> symmetries, BooleanVariableAllocator allocator, Options options) {	
		this.allocator = allocator;
		this.usize = allocator.bounds().universe().size();
	
		this.MAX_CMP_LENGTH = options.symmetryBreaking();
		
		this.symmetries = new LinkedList<IntSet>();
		this.symmetries.addAll(symmetries);
		
		final Set<Relation> relations = allocator.bounds().relations();
		this.sortedRels = relations.toArray(new Relation[relations.size()]);
		this.relSymms = new IdentityHashMap<Relation, Set<IntSet>>(relations.size());
		
		initSortedRels();
		initRelParts();	
	}
	
	
	/**
	 * Initializes this.sortedRels
	 */
	private void initSortedRels() {
		final Comparator<Relation> cmp = new Comparator<Relation>() {
			public int compare(Relation o1, Relation o2) {
				final int acmp = o1.arity() - o2.arity();
				return acmp!=0 ? acmp : String.valueOf(o1.name()).compareTo(String.valueOf(o2.name()));
			}
		};
		Arrays.sort(sortedRels, cmp);
	}
	
	/**
	 * Initializes this.relParts.
	 */
	private void initRelParts() {
		final Bounds bounds = allocator.bounds();
		for(Relation r : bounds.relations()) {
			IntSet upper = bounds.upperBound(r).indexView();
			Set<IntSet> rsymms = new HashSet<IntSet>(symmetries.size()/2);
			for(IntIterator tuples = upper.iterator(); tuples.hasNext(); ) {
				for(int tIndex = tuples.nextInt(), i = r.arity(); i > 0; i--, tIndex /= usize) {
					for(IntSet symm : symmetries) {
						if (symm.contains(tIndex%usize)) {
							rsymms.add(symm);
							break;
						}
					}
				}
			}
			relSymms.put(r, rsymms);
		}
	}
	
	/**
	 * Constructs a conjunction of lex-leader predicates for 
	 * all atom sets in this.symmetries.
	 * @return a conjunction of lex-leader predicates for all 
	 * atom sets in this.symmetries
	 */
	private BooleanValue lexLeaderPredicates() {
		final BooleanAccumulator sbp = BooleanAccumulator.treeGate(Operator.AND);
		for(Iterator<IntSet> symIter = symmetries.iterator(); symIter.hasNext(); ) {
			sbp.add(lexLeaderPredicateFor(symIter.next()));
			symIter.remove();
		}
		return allocator.factory().accumulate(sbp);
	}
	
	/**
	 * Constructs a lex-leader SBP that breaks symmetries on the given 
	 * symmetric partition.  The code below is adapted from alloy.symm.SBM_LexLeader class.   
	 * @requires symmPart in this.symmetries[int]
	 * @return a lex-leader SBP that breaks symmetries on the given symmetric partition.
	 */
	private BooleanValue lexLeaderPredicateFor(IntSet symmPart) {
		if (symmPart.size()<2) return BooleanConstant.TRUE;
		
		final BooleanAccumulator lexLeader = BooleanAccumulator.treeGate(Operator.AND);
		final Bounds bounds = allocator.bounds();
		final List<BooleanValue> originalBits = new ArrayList<BooleanValue>(MAX_CMP_LENGTH);
		final List<BooleanValue> permutedBits = new ArrayList<BooleanValue>(MAX_CMP_LENGTH);
		
//		System.out.println(symmPart);
		final IntIterator indeces = symmPart.iterator();
		for(int prevIndex = indeces.nextInt(); indeces.hasNext(); ) {
			int curIndex = indeces.nextInt();
			for(int i = 0; i < sortedRels.length && originalBits.size() < MAX_CMP_LENGTH; i++) {
				
				Relation r = sortedRels[i];
				
				if (bounds.upperBound(r).size()==bounds.lowerBound(r).size() || 
					!relSymms.get(r).contains(symmPart))
					continue;  // r is constant or upper bound of r does not range over the atoms in symmPart
				
				BooleanMatrix m = allocator.allocate(r);
//				System.out.println(r + ": " + m);
				for(IndexedEntry<BooleanValue> entry : m) {
					int permIndex = permutation(r.arity(), entry.index(), prevIndex, curIndex);
					BooleanValue permValue = m.get(permIndex);
					if (permIndex==entry.index() ||
						atSameIndex(originalBits, permValue, permutedBits, entry.value()))
						continue;
					
					originalBits.add(entry.value());
					permutedBits.add(permValue);			
				}
			}
			
//			System.out.println(originalBits + " <= ");
//			System.out.println(permutedBits);
			
			lexLeader.add(leq(originalBits, permutedBits));
			originalBits.clear();
			permutedBits.clear();
			prevIndex = curIndex;
		}
		
		return allocator.factory().accumulate(lexLeader);
	}
	
	/**
	 * Returns a BooleanValue that is true iff the string of bits
	 * represented by l0 is lexicographically less than or equal
	 * to the string of bits reprented by l1.
	 * @requires l0.size()==l1.size()
	 * @return a circuit that compares l0 and l1
	 */
	private final BooleanValue leq(List<BooleanValue> l0, List<BooleanValue> l1) {
		final BooleanFactory f = allocator.factory();
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
	 * Returns true if there is some index i such that 
	 * l0[i] = v0 and l1[i] = v1.
	 * @requires l0.size()=l1.size()
	 * @return some i: int | l0[i] = v0 && l1[i] = v1
	 */
	private static boolean atSameIndex(List<BooleanValue> l0, BooleanValue v0, 
			                           List<BooleanValue> l1, BooleanValue v1) {
		for(int i = 0; i < l0.size(); i++) {
			if (l0.get(i).equals(v0) && l1.get(i).equals(v1))
				return true;
		}
		return false;
	}
}
