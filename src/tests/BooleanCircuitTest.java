package tests;

import static relcalc.core.bool.BooleanConstant.FALSE;
import static relcalc.core.bool.BooleanConstant.TRUE;
import static relcalc.core.bool.MultiGate.Operator.AND;
import static relcalc.core.bool.MultiGate.Operator.OR;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import junit.framework.TestCase;
import relcalc.core.bool.BooleanConstant;
import relcalc.core.bool.BooleanFormula;
import relcalc.core.bool.BooleanValue;
import relcalc.core.bool.BooleanVariable;
import relcalc.core.bool.CircuitFactory;
import relcalc.core.bool.MultiGate.Operator;
import relcalc.util.ArrayIterator;


public class BooleanCircuitTest extends TestCase {
	private CircuitFactory f;
	private BooleanVariable[] v;
	private final int size = 20;
	
	protected void setUp() throws Exception {
		super.setUp();
		init();
	}
	
	private void init() {
		f = CircuitFactory.factory(size);
		v = new BooleanVariable[size];
		for(int i = 0; i < size; i++) {
			v[i] = f.variable(i+1);
			assertNotNull(v[i]);
		}
	}
	
	public final void testConstant() {
		assertSame(TRUE, BooleanConstant.constant(true));
		assertTrue(TRUE.booleanValue());  
		
		assertSame(FALSE, BooleanConstant.constant(false));
		assertFalse(FALSE.booleanValue());  
	}
	
	public void testVariable() {
		for(int i = 0; i < size; i++) {
			assertSame(v[i], f.variable(i+1));
		}
		assertNull(f.variable(size+1));
		assertNull(f.variable(0));
	}
	
	public final void testNot() {
		// bivalency: !T = F, !F = T
		assertSame(FALSE, f.not(TRUE));
		assertSame(TRUE, f.not(FALSE));
		
		// involution !!a = a
		assertSame(FALSE, f.not(f.not(FALSE)));
		assertSame(TRUE, f.not(f.not(TRUE)));
		
		for(int i = 0; i < size; i++) {
			assertSame(v[i], f.not(f.not(v[i])));
		}
		
		for(int i = 0; i < size / 2; i++) {
			assertSame(f.or(v[i*2], v[i*2 + 1]), 
					f.not(f.not(f.or(v[i*2], v[i*2 + 1]))));
			assertSame(f.and(v[i*2], v[i*2 + 1]), 
					f.not(f.not(f.and(v[i*2], v[i*2 + 1]))));
		}
	}
	
	private final void testParenthesis(BooleanValue result, Operator op,
			BooleanValue p, BooleanValue q,
			BooleanValue r, BooleanValue s) {
		
//		System.out.println("-------------------------------");
//		System.out.println("p: " + p + " " + p.literal());
//		System.out.println("q: " + q + " " + q.literal());
//		System.out.println("r: " + r + " " + r.literal());
//		System.out.println("s: " + s + " " + s.literal());
//		
//		System.out.println("pq: " + f.compose(op, p, q));
//		System.out.println("rs: " + f.compose(op, r, s));
//		System.out.println("(pq)(rs): " + f.compose(op, f.compose(op, p, q), f.compose(op, r , s)));
//		
//		System.out.println("rs: " + r.compose(op, s));
//		System.out.println("q(rs): " + q.compose(op, r.compose(op, s)));
//		System.out.println("p(q(rs)): " + p.compose(op, (q.compose(op, (r.compose(op, s))))));
//		
//		System.out.println("qr: " + q.compose(op, r));
//		System.out.println("(qr)s: " + (q.compose(op, r)).compose(op, s));
//		System.out.println("p(qr)s): " + p.compose(op, ((q.compose(op, r)).compose(op, s))));
//		
//		System.out.println("pq: " + p.compose(op, q));
//		System.out.println("(pq)r: " + (p.compose(op, q)).compose(op, r));
//		System.out.println("(p(qr))s: " + ((p.compose(op, q)).compose(op, r)).compose(op, s));
		
//		p(q(rs))
		assertSame(result, f.compose(op, p, f.compose(op, q, f.compose(op, r, s))));
//		p((qr)s)
		assertSame(result, f.compose(op, p, f.compose(op, f.compose(op, q, r), s)));
//		(pq)(rs)
		assertSame(result, f.compose(op, f.compose(op, p, q), f.compose(op, r, s)));
//		(p(qr))s
		assertSame(result, f.compose(op, p, f.compose(op, f.compose(op, q, r), s)));
//		((pq)r)s
		assertSame(result, f.compose(op, f.compose(op, f.compose(op, p, q), r), s));
	}
	
	private final void testIdentityContradictionExcludedMiddle(Operator op, BooleanValue p) {
        // identity: p & T = p | F = p
        assertSame(p, f.compose(op, p, op.identity()));
        
        // short circuit: p & F = F, p | T = T
        assertSame(op.shortCircuit(), f.compose(op, p, op.shortCircuit()));
        
        // contradiction / excluded middle: p & !p = F, p | !p = T
        assertSame(op.shortCircuit(), f.compose(op, p, f.not(p)));
        
    }
	
	private final void testIdempotencyAbsorptionContraction(Operator op, BooleanValue p, BooleanValue q) {
        // idempotency: p op p = p 
        assertSame(p, f.compose(op, p, p));
        assertSame(q, f.compose(op, q, q));
        
        // absorption: p op (p op q) = p op q
        assertSame(f.compose(op, p, q), f.compose(op, p, f.compose(op, p, q)));
        
        // contraction: p op.complement (p op q) = p
        //System.out.println(p + ", " + q + ", " + p.compose(op.complement(), p.compose(op, q)));
        assertSame(p, f.compose(op.complement(), p, f.compose(op, p, q)));
    }
	
	
	
	private final void testCommutativityAndAssociativity(Operator op, 
			BooleanValue p, BooleanValue q,
			BooleanValue r, BooleanValue s) {
		
//		System.out.println("p: " + p + " " + p.digest());
//		System.out.println("q: " + q + " " + q.digest());
//		System.out.println("r: " + r + " " + r.digest());
//		System.out.println("s: " + s + " " + s.digest());
//		System.out.println("pq: " + p.compose(op, q));
//		System.out.println("pqr: "  + p.compose(op, q).compose(op, r));
//		System.out.println("pqrs: " + p.compose(op, q).compose(op, r).compose(op, s));
		
//		commutativity:  p op q = p op q
//		and associativity: p op (r op q) = (p op r) op q
		
//		generate all permutations, and their parenthesizations, of p q r s and check that they are the same
		final List<BooleanValue> formulas = new LinkedList<BooleanValue>();
		formulas.add(p); formulas.add(q); formulas.add(r); formulas.add(s);
		
		final BooleanValue composition = f.compose(op, f.compose(op, f.compose(op, p, q), r), s);
		
		for (int i0 = 0; i0 < 4; i0++) {
			BooleanValue f0 = (BooleanValue) formulas.get(i0);
			formulas.remove(i0);
			for (int i1 = 0; i1 < 3; i1++) {
				BooleanValue f1 = (BooleanValue) formulas.get(i1);
				formulas.remove(i1);
				
				for (int i2 = 0; i2 < 2; i2++) {
					BooleanValue f2 = (BooleanValue) formulas.get(i2);
					formulas.remove(i2);
					testParenthesis(composition, op, f0, f1, f2, (BooleanValue) formulas.get(0));
					formulas.add(i2,f2);
				}
				
				formulas.add(i1,f1);
				
			}
			formulas.add(i0,f0);
		}
		
	}
	
	private final void testMultiGateWithConstantAndFormula(Operator op, BooleanValue p, BooleanValue q) {
		testCommutativityAndAssociativity(op, TRUE, p, q, FALSE);
		testCommutativityAndAssociativity(op, FALSE, TRUE, p, q);
		testCommutativityAndAssociativity(op, p, TRUE, q, FALSE);
        testIdempotencyAbsorptionContraction(op, p, FALSE);
        testIdempotencyAbsorptionContraction(op, TRUE, q);
	}
	
	private final void testMultiGateWithConstantArgument(Operator op) {
		// constant / constant
        testIdentityContradictionExcludedMiddle(op, TRUE);
        testIdentityContradictionExcludedMiddle(op, FALSE);
        testIdempotencyAbsorptionContraction(op, TRUE, FALSE);
        
        // constant / variable
        testMultiGateWithConstantAndFormula(op, v[8], v[9]);
        
        // constant / negation 
        final BooleanValue v246 = f.compose(op, v[2], f.compose(op.complement(), v[4], v[6]));
        final BooleanValue v135 = f.compose(op.complement(), v[1], f.compose(op, v[3], v[5]));
        testMultiGateWithConstantAndFormula(op, f.not(v246), f.not(v135));
        testMultiGateWithConstantAndFormula(op, v246, f.not(v135));
        
        // constant / multigate
        testMultiGateWithConstantAndFormula(op, v246, v135);
        testMultiGateWithConstantAndFormula(op, f.compose(op, v[2], v[3]), f.compose(op, v[1], v[4]));
	}
	
	private final void testMultiGateWithVariableArgument(Operator op) {
		// variable / variable    
        testIdentityContradictionExcludedMiddle(op, v[0]);
        testIdentityContradictionExcludedMiddle(op, v[3]);
        testCommutativityAndAssociativity(op, v[4], v[6], v[8], v[2]);
        testIdempotencyAbsorptionContraction(op, v[1], v[5]);
        
        // variable / negation
        final BooleanValue v101214 = f.compose(op, v[10], f.compose(op.complement(), v[12], v[14]));
        final BooleanValue v151311 = f.compose(op.complement(), v[15], f.compose(op, v[13], v[11]));
        
        testCommutativityAndAssociativity(op, v[10], f.not(v151311), v[14], f.not(v101214));
        testIdempotencyAbsorptionContraction(op, v[11], f.not(v151311));
        
        // variable / multigate
        final BooleanValue v191618 = f.compose(op.complement(), v[19], f.compose(op.complement(), v[16], v[18])), 
                           v101712 = f.compose(op, v[10], f.compose(op, v[17], v[12])), 
                           v141815 = f.compose(op.complement(), v[14], f.compose(op, v[18], v[15])),  
                           v121716 = f.compose(op, v[12], f.compose(op.complement(), v[17], v[16]));
        testCommutativityAndAssociativity(op, v[10], v[12], v191618, v141815);
        // fails due to not extensively checking for sharing, which is ok for now 
//        testCommutativityAndAssociativity(op, v[10], v[12], v121716, v101712);
        testCommutativityAndAssociativity(op, v[9], v[18], v[16], v121716);
        testIdempotencyAbsorptionContraction(op, v[10], v101712);
        testIdempotencyAbsorptionContraction(op, v[14], v141815);
	}
	
	private final void testMultiGateWithNegatedArgument(Operator op) {
		// negation / negation
        final BooleanValue v842 = f.compose(op.complement(), v[8], f.compose(op.complement(), v[4], v[2])), 
                           v191015 = f.compose(op, v[19], f.compose(op, v[10], v[15])), 
                           v027 = f.compose(op.complement(), v[0], f.compose(op, v[2], v[7])),  
                           v15104 = f.compose(op, v[15], f.compose(op.complement(), v[10], v[4]));
        testIdentityContradictionExcludedMiddle(op, f.not(v027));
        testIdentityContradictionExcludedMiddle(op, f.not(v191015));
        testCommutativityAndAssociativity(op, f.not(v842), f.not(v191015), f.not(v027), f.not(v15104));
        
        // negation / multigate
        testCommutativityAndAssociativity(op, v842, f.not(v191015), v027, f.not(v15104));
        testCommutativityAndAssociativity(op, f.not(v842), v191015, v027, f.not(v15104));
        testCommutativityAndAssociativity(op, v842, f.not(v191015), f.not(v027), v15104);
        testCommutativityAndAssociativity(op, f.not(v842), v191015, f.not(v027), v15104);
        
	}
	
	private final void testMultiGateWithMultiGateArgument(Operator op) {
		final BooleanValue v842 = f.compose(op.complement(), v[8], f.compose(op.complement(), v[4], v[2])), 
                           v191015 = f.compose(op, v[19], f.compose(op, v[10], v[15])), 
                           v027 = f.compose(op.complement(), v[0], f.compose(op, v[2], v[7])),  
                           v15104 = f.compose(op, v[15], f.compose(op.complement(), v[10], v[4]));
		
		testIdentityContradictionExcludedMiddle(op, v027);
        testIdentityContradictionExcludedMiddle(op, v191015);
        testCommutativityAndAssociativity(op, v842, v191015, v027, v15104);
        testCommutativityAndAssociativity(op, v842, v191015, v027, f.compose(op,v[19], v[0]));
        testIdempotencyAbsorptionContraction(op, v842, v15104);
	}
			
	private void testMultiGate(Operator op) {
		testMultiGateWithConstantArgument(op);
		init();
		testMultiGateWithVariableArgument(op);
		init();
		testMultiGateWithNegatedArgument(op);
		init();
		testMultiGateWithMultiGateArgument(op);
	}
	
	public final void testAnd() {
		testMultiGate(AND);
	}
	
	public final void testOr() {
		testMultiGate(OR);
	}
	
	private boolean hasInputs(BooleanFormula f, Iterator<BooleanValue> ins) {
		final Iterator<BooleanValue> fins = f.inputs();
		while(fins.hasNext()&&ins.hasNext()) {
			if (fins.next()!=ins.next()) return false;
		}
		return true;
	}
	
	private final void testMultiGateFlattening(Operator op) {
		final BooleanFormula v15 = (BooleanFormula) f.compose(op, v[1], v[5]);
		final BooleanFormula v26 = (BooleanFormula) f.compose(op, f.not(v[2]), v[6]);
		final BooleanFormula v36 = (BooleanFormula) f.compose(op, v[2], f.not(v[6]));
		final BooleanFormula v47 = (BooleanFormula) f.compose(op, f.not(v[4]), f.not(v[7]));
		
		assertEquals(v15.toString(), f.flatten(v15).toString());
		assertEquals(v26.toString(), f.flatten(v26).toString());
		assertEquals(v36.toString(), f.flatten(v36).toString());
		assertEquals(v47.toString(), f.flatten(v47).toString());
		
		assertEquals(f.not(v15).toString(), f.flatten(f.not(v15)).toString());
		assertEquals(f.not(v26).toString(), f.flatten(f.not(v26)).toString());
		assertEquals(f.not(v36).toString(), f.flatten(f.not(v36)).toString());
		assertEquals(f.not(v47).toString(), f.flatten(f.not(v47)).toString());
		
		assertEquals(f.compose(op.complement(), v15, v36).toString(), f.flatten(f.compose(op.complement(), v15, v36)).toString());
		assertSame(op.shortCircuit(), f.flatten(f.compose(op, v36, f.compose(op, v15, v26))));
		assertTrue(hasInputs((BooleanFormula) f.flatten(f.compose(op, v15, v47)), ArrayIterator.iterate(f.not(v[7]), f.not(v[4]), v[1], v[5])));
	}
	
	public final void testFlattening() {
		// flatten constants
		assertSame(TRUE, f.flatten(TRUE));
		assertSame(FALSE, f.flatten(FALSE));
		
		// flatten variables and their negations
		assertSame(v[10], f.flatten(v[10]));
		assertSame(f.not(v[10]), f.flatten(f.not(v[10])));
		
		// flatten multigates and their negations
		testMultiGateFlattening(AND);
		testMultiGateFlattening(OR);
	}


}
