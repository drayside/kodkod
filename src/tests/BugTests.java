package tests;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import junit.framework.TestCase;
import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.Relation;
import kodkod.ast.Variable;
import kodkod.engine.Evaluator;
import kodkod.engine.Solver;
import kodkod.engine.TimeoutException;
import kodkod.instance.Bounds;
import kodkod.instance.Instance;
import kodkod.instance.Tuple;
import kodkod.instance.TupleFactory;
import kodkod.instance.TupleSet;
import kodkod.instance.Universe;
import kodkod.util.IntBitSet;
import kodkod.util.IntIterator;
import kodkod.util.IntSet;
import kodkod.util.IntTreeSet;

/**
 * Test cases that record reported bugs. 
 * 
 * @author Emina Torlak
 */
public class BugTests extends TestCase {
	private final Solver solver = new Solver();
	
	public final void testEmina_02162006() {
		final IntTreeSet s = new IntTreeSet();
		for (int i=0; i<5; i++) {
            s.add(i);
        }
		final IntTreeSet s1 = new IntTreeSet();
		s1.add(0);
		
		final IntSet intersection = s.copy();
		intersection.retainAll(s1);
		s.removeAll(intersection);
		
		assertSameContents(s, 1, 2, 3, 4);
		assertSameContents(s1, 0);
		assertSameContents(intersection, 0);		
	}
	
	public final void testVincent_02162006() {
	    // set ups universe of atoms [1..257]
        final List<Integer> atoms = new ArrayList<Integer>();

        // change this to 256, and the program works
        for (int i=0; i<257; i++) {
            atoms.add(i+1);
        }

        final Universe universe = new Universe(atoms);
        final Bounds bounds = new Bounds(universe);
        final TupleFactory factory = universe.factory();

        // oneRel is bounded to the Integer 1
        final Relation oneRel = Relation.unary("oneRel");

        // rel can contain anything
        final Relation rel = Relation.unary("rel");

        bounds.bound(oneRel, factory.setOf(factory.tuple(atoms.get(0))),
        				factory.setOf(factory.tuple(atoms.get(0))));
        bounds.bound(rel, factory.allOf(1));

        // constraint: oneRel in rel
        Formula formula = oneRel.in(rel);

        // solve      
		try {
			final Instance instance = solver.solve(formula, bounds);
			assertNotNull(instance);
			//System.out.println(instance);
		} catch (TimeoutException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}        
      

	}
	
	private final void assertSameContents(IntSet s, int... elts) {
		assertEquals(elts.length, s.size());
		for(int i: elts)
			assertTrue(s.contains(i));
	}
	
	public final void testVincent_02132006() {
		IntTreeSet set = new IntTreeSet();        
        for (int i=0; i<2;i++) {
            set.add(i);
        }

        IntTreeSet set2 = new IntTreeSet();        
        for (int i=0; i<2;i++) {
            set2.add(i);
        }

        set.removeAll(set2);
        IntIterator setIterator = set.iterator();
        assertFalse(setIterator.hasNext());
        assertFalse(setIterator.hasNext());
      
        set.addAll(set2);
        assertSameContents(set, 0, 1);
        
        set2.clear();
        for (int i=3; i<5;i++) {
            set2.add(i);
        }
        
        set.addAll(set2);
        assertSameContents(set, 0, 1, 3, 4);
        
        set2.clear();
        for (int i=1; i<4;i++) {
            set2.add(i);
        }
        
        set.addAll(set2);
        assertSameContents(set, 0, 1, 2, 3, 4);
	}
	
	public final void testEmina_01232006() {
		final List<String> atoms = new ArrayList<String>(5);
		for(int i = 0; i < 5; i++) 
			atoms.add("a"+i);
		final Universe u = new Universe(atoms);
		final TupleFactory tf = u.factory();
		
		final Relation r1 = Relation.unary("r1"), 
		               r2 = Relation.binary("r2"), 
		               r3 = Relation.ternary("r3");
		final Bounds b = new Bounds(u);
		final TupleSet r2Bound = tf.noneOf(2);
		for(int i = 0; i < 4; i++)
			r2Bound.add(tf.tuple(atoms.get(i), atoms.get(i)));
		r2Bound.add(tf.tuple("a4", "a1"));
		r2Bound.add(tf.tuple("a4", "a2"));
		b.bound(r2, r2Bound);
		b.bound(r1, tf.setOf("a0", "a3"), tf.setOf("a0", "a3", "a4"));
		b.bound(r3, tf.setOf(tf.tuple("a0","a0","a0"), tf.tuple("a3","a3","a3")));
		final Formula f = r1.product(r2).in(r3);
		
		try {
			final Instance instance = solver.solve(f, b);
			assertTrue((new Evaluator(instance)).evaluate(f));
//			System.out.println(instance);
//			System.out.println((new Evaluator(instance)).evaluate(f  ));
		} catch (TimeoutException te) {
			fail("Timed out solving " + f);
		}
	}
	
	public final void testEmina_01192006() {
		IntBitSet s = new IntBitSet(64);
		for(int i = 4; i < 8; i++) {
			for(int j = 0; j < 4; j++) {
				s.add(i*8+j);
			}
		}
//		System.out.println(s);
		for(int i = 4; i < 8; i++) {
			assertTrue(s.iterator(i*8, (i+1)*8-1).hasNext());
			int count = 0;
			for(IntIterator iter = s.iterator(i*8, (i+1)*8-1); iter.hasNext(); ) {
				count += iter.nextInt() % 8;
			}
			assertEquals(count, 6);
		}
		for(int i = 4; i < 8; i++) {
			assertTrue(s.iterator((i+1)*8-1, i*8).hasNext());
			int count = 0;
			for(IntIterator iter = s.iterator((i+1)*8-1, i*8); iter.hasNext(); ) {
				count += iter.nextInt() % 8;
			}
			assertEquals(count, 6);
		}
	}
	
	public final void testGreg_01192006() {
//		 circular linked list
		Relation Entry = Relation.unary("Entry");
		Relation head = Relation.unary("head");
		Relation next = Relation.binary("next");
		Formula nextFun = next.function(Entry, Entry);

//		 bijection between indices and entries in linked list
		Relation Index = Relation.unary("Index");
		Relation index2Entry = Relation.binary("index2Entry");
		Expression entries = head.join(next.closure());
		Variable e = Variable.unary("e");
		Expression preImage = index2Entry.join(e);
		Formula index2EntryBij = e.in(entries).implies(preImage.one()).and(
				e.in(entries).not().implies(preImage.no())).forAll(e.oneOf(Entry));

//		 try to force list to have three distinct entries
		Variable e1 = Variable.unary("e1");
		Variable e2 = Variable.unary("e2");
		Variable e3 = Variable.unary("e3");
		Formula threeEntries =
		e1.eq(e2).not().and(e1.eq(e3).not()).and(e2.eq(e3).not()).
			forSome(e1.oneOf(entries).and(e2.oneOf(entries).and(e3.oneOf(entries))));
		Formula simulate = head.one().and(nextFun).and(index2EntryBij).and(threeEntries);

		Object Entry0 = "Entry0";
		Object Entry1 = "Entry1";
		Object Entry2 = "Entry2";
		Object Entry3 = "Entry3";
		Object Index0 = "Index0";
		Object Index1 = "Index1";
		Object Index2 = "Index2";
		Object Index3 = "Index3";

		Universe univ = new Universe(
				Arrays.asList(Entry0, Entry1, Entry2, Entry3,
						Index0, Index1, Index2, Index3));
		TupleFactory factory = univ.factory();
		TupleSet entryTuples = factory.setOf(Entry0, Entry1, Entry2, Entry3);
		TupleSet indexTuples = factory.setOf(Index0, Index1, Index2, Index3);

		Instance instance = new Instance(univ);
		instance.add(Entry, entryTuples);
		instance.add(head, factory.setOf(Entry0));
		instance.add(Index, indexTuples);

		Tuple next0 = factory.tuple(Entry0, Entry1);
		Tuple next1 = factory.tuple(Entry1, Entry2);
		Tuple next2 = factory.tuple(Entry2, Entry3);
		Tuple next3 = factory.tuple(Entry3, Entry0);
		instance.add(next, factory.setOf(next0, next1, next2, next3));

		Tuple i2e0 = factory.tuple(Index0, Entry0);
		Tuple i2e1 = factory.tuple(Index1, Entry1);
		Tuple i2e2 = factory.tuple(Index2, Entry2);
		Tuple i2e3 = factory.tuple(Index3, Entry3);
		instance.add(index2Entry, factory.setOf(i2e0, i2e1, i2e2, i2e3));

		Evaluator eval = new Evaluator(instance);
		assertTrue(eval.evaluate(simulate));

		Bounds bounds = new Bounds(univ);
		bounds.boundExactly(Entry, entryTuples);
		bounds.bound(head, entryTuples);
		bounds.bound(next, entryTuples.product(entryTuples));
		bounds.bound(Index, indexTuples);
		bounds.bound(index2Entry, indexTuples.product(entryTuples));
//		Solver solver = new Solver(SATSolverName.Default);
		try {
//			System.out.println(simulate);
//			System.out.println(bounds);
//			System.out.println(instance);
			instance = solver.solve(simulate, bounds);
//			System.out.println(instance);
			assertNotNull(instance);
		} catch (TimeoutException e4) {
			// TODO Auto-generated catch block
			e4.printStackTrace();
		}
	}
	
	public final void testMana_01132006() {
//		r0=[[], [[null], [DblLinkedList0]]], 
//		null=[[[null]], [[null]]], 
//		head=[[], [[DblLinkedList0, null], [DblLinkedList0, DblLinkedListElem0]]], 
//		next=[[], [[DblLinkedListElem0, null], [DblLinkedListElem0, DblLinkedListElem0]]],
//		univ=[[[null], [DblLinkedList0], [1], [DblLinkedListElem0], [0]], [[null], [DblLinkedList0], [1], [DblLinkedListElem0], [0]]]
//		r1=[[], [[null], [DblLinkedListElem0]]],
		final List<String> atoms = new ArrayList<String>(5);
		atoms.add("null"); atoms.add("DblLinkedList0"); atoms.add("1");
		atoms.add("DblLinkedListElem0"); atoms.add("0");
		final Universe u = new Universe(atoms);
		final TupleFactory t = u.factory();
		
		//!((head . univ) in ((if (r1 in null) then (head ++ (r0 -> (r1 . next))) else head) . univ))

		final Relation head = Relation.binary("head"), univ = Relation.unary("univ"),
		               r0 = Relation.unary("r0"), r1 = Relation.unary("r1"),
		               next = Relation.binary("next"), nil = Relation.unary("null"),
		               none = Relation.unary("none");
		
		final Expression override = head.override(r0.product(r1.join(next)));
		final Expression ifElse = r1.in(nil).thenElse(override, head);
		final Formula f = head.join(univ).in(ifElse.join(univ)).not();
		
		final Bounds b = new Bounds(u);
		b.bound(r0, t.setOf("null", "DblLinkedList0"));
		b.bound(r1, t.setOf("null", "DblLinkedListElem0"));
		b.bound(head, t.setOf("DblLinkedList0").product(b.upperBound(r1)));
		
		b.bound(next, t.setOf(t.tuple("DblLinkedListElem0","null"), t.tuple("DblLinkedListElem0","DblLinkedListElem0")));
		b.boundExactly(univ, t.allOf(1));
		b.boundExactly(nil, t.setOf("null"));
		b.boundExactly(none, t.noneOf(1));
		
//		System.out.println(f);
//		System.out.println(b);
		
		try {
			final Instance instance = solver.solve(f, b);
			assertNull(instance);
		} catch (TimeoutException te) {
			fail("Timed out solving " + f);
		}
	}
	
	
	public final void testGreg_11232005() {
		final List<String> atoms = new ArrayList<String>(3);
		atoms.add("-1"); atoms.add("0"); atoms.add("1");
		final Universe u = new Universe(atoms);
		final TupleFactory t = u.factory();
		
		final Relation inc = Relation.binary("inc"), add = Relation.ternary("add"), 
		               one = Relation.unary("1"), param0 = Relation.unary("param0"), 
		               ints = Relation.unary("int");
		
		// (one param0 && ((1 . (param0 . add)) in (param0 . ^inc)))
		final Formula f = param0.one().and((one.join(param0.join(add))).in(param0.join(inc.closure())));
		
		final Bounds b = new Bounds(u);
		
		b.bound(param0, t.allOf(1));
		b.boundExactly(one, t.setOf(t.tuple("1")));
		b.boundExactly(ints, t.allOf(1));
		b.boundExactly(inc, t.setOf(t.tuple("-1","0"), t.tuple("0","1")));
		// [1, 1, -1], [1, -1, 0], [1, 0, 1], [-1, 1, 0], [-1, -1, 1],
		// [-1, 0, -1], [0, 1, 1], [0, -1, -1], [0, 0, 0]]
		b.boundExactly(add, t.setOf(t.tuple("1","1","-1"), t.tuple("1","-1","0"), t.tuple("1","0","1"), 
				                  t.tuple("-1","1","0"), t.tuple("-1","-1","1"), t.tuple("-1","0","-1"), 
				                  t.tuple("0","1","1"), t.tuple("0","-1","-1"), t.tuple("0","0","0")));
		
//		System.out.println(f);
//		System.out.println(b);
		
		try {
			final Instance instance = solver.solve(f, b);
			assertTrue((new Evaluator(instance)).evaluate(f));
//			System.out.println(instance);
//			System.out.println((new Evaluator(instance)).evaluate(f  ));
		} catch (TimeoutException te) {
			fail("Timed out solving " + f);
		}
	}
	
	public final void testGreg_01052006() {
		
//		final TreeSequence<Integer> t = new TreeSequence<Integer>();
//		final int[] elts = {107, 31, 86, 72, 6, 119, 23, 131};
//		for(int i = 0; i < elts.length; i++) {
//			t.put(elts[i], 0);
//		}
//		final int[] sorted = new int[elts.length];
//		System.arraycopy(elts, 0, sorted, 0, elts.length);
//		Arrays.sort(sorted);
//		int count = 0;
//		for(IndexedEntry<Integer> e : t) {
//			assertEquals(sorted[count++], e.index());
//		}
//		t.remove(72);
//		t.put(72,0);
//		count = 0;
//		for(IndexedEntry<Integer> e : t) {
//			assertEquals(sorted[count++], e.index());
//		}
		
		final List<Object> atoms = new ArrayList<Object>(12);
		atoms.add(2);
		atoms.add(4);
		atoms.add(-2);
		atoms.add("null"); 
		atoms.add("array0");
		atoms.add(6);
		atoms.add(1);
		atoms.add(-1);
		atoms.add(-3);
		atoms.add(3);
		atoms.add(5);
		atoms.add(0);
		
		final Universe u = new Universe(atoms);
		final TupleFactory f = u.factory();
		
		final TupleSet tdiv = f.noneOf(3);
		
		for(int i = -3; i <=6; i ++) {
			for(int j = -3; j <=6; j++) {
				if (j != 0) {
					int divij = i/j;
					if (-3 <= divij && divij <= 6 )
						tdiv.add(f.tuple(i,j,divij));
					else 
						tdiv.add(f.tuple(i,j,(10+divij)%10));
				}
			}
		}
		
		final TupleSet tdivcopy = tdiv.copy();
		for(int i = -3; i <=6; i ++) {
			for(int j = -3; j <=6; j++) {
				if (j != 0) {
					int divij = i/j;
					if (-3 <= divij && divij <= 6 )
						assertTrue(tdivcopy.contains(f.tuple(i,j,divij)));
					else 
						assertTrue(tdivcopy.contains(f.tuple(i,j,(10+divij)%10)));
				}
			}
		}	
	}
	
	
}
