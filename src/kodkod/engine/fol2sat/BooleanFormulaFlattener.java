package kodkod.engine.fol2sat;

import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.Map;

import kodkod.engine.bool.BooleanConstant;
import kodkod.engine.bool.BooleanFactory;
import kodkod.engine.bool.BooleanValue;
import kodkod.engine.bool.BooleanVariable;
import kodkod.engine.bool.BooleanVisitor;
import kodkod.engine.bool.MultiGate;
import kodkod.engine.bool.MutableMultiGate;
import kodkod.engine.bool.NotGate;
import kodkod.engine.bool.MultiGate.Operator;
import kodkod.util.IntSet;
import kodkod.util.Ints;

/**
 * <p>Given a {@link kodkod.engine.bool.BooleanValue boolean value}, v, and a 
 * {@link kodkod.engine.bool.BooleanFactory factory}, F, 
 * a BooleanFormulaFlattener eliminates as many 
 * intermediate gates as possible from v, and stores the flattened tree of v in F.  
 * An intermediate gate's inputs are absorbed into the parent's iff the 
 * the gate's fanout is 1 and the gate and its parent are MultiGates with the same operator.  
 * For example, suppose that the root corresponds to the formula
 * ((1 || 2) || 3 || !(4 || 5) || (6 & (7 & 8))), and that the components of this formula are
 * assigned the following literals:  (1 || 2) ---> 9, (4 || 5) ---> 10, !(4 || 5) ---> -10, (7 & 8) ---> 11, 
 * (6 & (7 & 8)) ---> 12, and ((1 || 2) || 3 || !(4 || 5) || (6 & 7 & 8)) ---> 13.  
 * Calling this.flatten(root) will flatten the root to (1 || 2 || 3 || !(4 || 5) || (6 & 7 & 8)), 
 * re-assigning the literals as follows: (4 || 5) ---> 9, !(4 || 5) ---> -9, (6 & 7 & 8) ---> 10, and 
 * (1 || 2 || 3 || !(4 || 5) || (6 & 7 & 8)) ---> 11.  
 * 
 * @author Emina Torlak
 */
final class BooleanFormulaFlattener {

	private BooleanFormulaFlattener() {}
	
	/**
	 * Flattens the given value using f and returns it.
	 * The method assumes that all variables at the leaves of
	 * the root are components of f.
	 * @requires (root.*inputs & Variable) in f.components
	 * @effects f.components = f.components' + flatRoot.*inputs
	 * @return {flatRoot : BooleanValue | [[flatRoot]] = [[root]] && 
	 *           no d, p: flatRoot.^inputs & MultiGate | d in p.inputs && d.op = p.op && inputs.d != p }  
	 */
	static final BooleanValue flatten(BooleanValue root, BooleanFactory f) {
		final int oldCompDepth = f.comparisonDepth();
		f.setComparisonDepth(1);
		final FlatteningVisitor flattener = new FlatteningVisitor(root, f);
		final BooleanValue flatRoot = root.accept(flattener, null);
		f.setComparisonDepth(oldCompDepth);
		return flatRoot;
	}
	
	/**
	 * The visitor that flattens a given formula, as described in BooleanFactory.flatten(BooleanValue)
	 */
	private static final class FlatteningVisitor implements BooleanVisitor<BooleanValue, MutableMultiGate> {
		private final BooleanFactory factory;
		private final IntSet flattenable;
		private final Map<MultiGate,BooleanValue> cache;
		
		/**
		 * Constructs a new FlatteningVisitor.  The returned visitor can only be applied to the specified 
		 * root value.  All the variables at the leaves of the given root must have been created by the 
		 * given factory.
		 * @requires (root.*inputs & Variable) in factory.components
		 */
		FlatteningVisitor(BooleanValue root, BooleanFactory factory) {
			this.factory = factory;
			final FlatteningDataGatherer dataGatherer = new FlatteningDataGatherer(root);
			root.accept(dataGatherer, null);
			this.flattenable = dataGatherer.flattenable;
			dataGatherer.visited.removeAll(flattenable); 
			this.cache = new IdentityHashMap<MultiGate,BooleanValue>(dataGatherer.visited.size());
		}
		
		/**
		 * If p is null, returns v.  Otherwise, adds v to p and
		 * returns the result.
		 */
		private final BooleanValue addToParent(BooleanValue v, MutableMultiGate parent) {
			return parent==null ? v : parent.addInput(v);
		}
		
		public BooleanValue visit(MultiGate multigate, MutableMultiGate parent) {
			final MultiGate.Operator op = multigate.op();
			if (flattenable.contains(multigate.literal())) { // multigate's inputs are absorbed into its parent's inputs
//				System.out.println("Flattenable: " + multigate);
				for(Iterator<BooleanValue> inputs = multigate.inputs(); inputs.hasNext();) {
					if (inputs.next().accept(this, parent)==op.shortCircuit())
						return op.shortCircuit();
				}
				return parent;
			} else { // construct a gate that corresponds to the multigate
//				System.out.println("Unflattenable: " + multigate);
				BooleanValue replacement = cache.get(multigate);
			
				if (replacement == null) {
					final MutableMultiGate newGate = MutableMultiGate.treeGate(op);
					for(Iterator<BooleanValue> inputs = multigate.inputs(); inputs.hasNext();) {
						if (inputs.next().accept(this,newGate)==op.shortCircuit()) {
							return op.shortCircuit();
						}
					}
					replacement = factory.toImmutableValue(newGate);
					cache.put(multigate, replacement);
				}
				
				return addToParent(replacement, parent);
			}
		}

		public BooleanValue visit(NotGate negation, MutableMultiGate parent) {
			return addToParent(factory.not(negation.input().accept(this,null)), parent);
		}

		public BooleanValue visit(BooleanVariable variable, MutableMultiGate parent) {
			return addToParent(variable, parent);
		}

		public BooleanValue visit(BooleanConstant constant, MutableMultiGate parent) {
			assert parent == null;
			return constant;
		}
		
	}
	
	/**
	 * A visitor that determins which gates can be flattened.  Specifically, when 
	 * applied to a given root, the flattenable field of the visitor contains the
	 * literals of all m such that m is a MultiGate descendent of the root and
	 * #inputs.m = 1 && (inputs.m).op = m.op => s.contains(m.literal).  That is,
	 * flattenable = {i: int | some m: root.^inputs & MultiGate | #inputs.m = 1 && (inputs.m).op = m.op && m.literal = i}
	 */
	private static final class FlatteningDataGatherer implements BooleanVisitor<Object, Operator> {
		/* contains the literals of all the flattenable multi gates */
		final IntSet flattenable;
		/* contains the literals of all the visited multi gates */
		final IntSet visited;
		
		/**
		 * Constructs a new flattenning data gatherer.   The returned visitor can only be
		 * applied to the specified root value.
		 */
		private FlatteningDataGatherer(BooleanValue root) {
			final int maxLit = StrictMath.abs(root.literal());
			this.flattenable = Ints.bestSet(maxLit+1);
			this.visited = Ints.bestSet(maxLit+1);
		}
		
		public Object visit(MultiGate multigate, Operator parentOp) {
			final int literal = multigate.literal();
			if (visited.contains(literal)) { // we've seen this node already
				flattenable.remove(literal);
			} else { // haven't seen it yet
				visited.add(literal);
				if (parentOp == multigate.op()) flattenable.add(literal);
				// visit children
				for(Iterator<BooleanValue> inputs = multigate.inputs(); inputs.hasNext();) {
					inputs.next().accept(this, multigate.op());
				}
			}
			
			return null;
		}
		
		public Object visit(NotGate negation, Operator parentOp) {
			negation.input().accept(this,null);
			return null;
		}

		public Object visit(BooleanVariable variable, Operator arg) {
			return null;
		}

		public Object visit(BooleanConstant constant, Operator arg) {
			return null;
		}
		
	}
}
