/*
 * BooleanMatrixTest.java
 * Created on Jul 12, 2005
 */
package tests;

import static relcalc.core.bool.BooleanConstant.FALSE;
import static relcalc.core.bool.BooleanConstant.TRUE;
import static relcalc.core.bool.MultiGate.Operator.AND;
import static relcalc.core.bool.MultiGate.Operator.OR;

import java.util.Arrays;
import java.util.Iterator;

import junit.framework.TestCase;
import relcalc.core.bool.BooleanMatrix;
import relcalc.core.bool.BooleanValue;
import relcalc.core.bool.BooleanVariable;
import relcalc.core.bool.BooleanFactory;
import relcalc.core.bool.Dimensions;
import relcalc.util.IndexedEntry;
import relcalc.util.IntRange;
import relcalc.util.Ints;

/** 
 * Tests {@link relcalc.core.bool.BooleanMatrix BooleanMatrix}.
 *
 * @author Emina Torlak 
 */
public class BooleanMatrixTest extends TestCase {
    private static final int NUM_VARS = 30;
    private final BooleanFactory f;
    private final BooleanVariable[] vars;
    private final Dimensions dim324, dim43, dim4; 
    private final IntRange[] mR;
    private final BooleanValue[] mCells;
    private BooleanMatrix mT324, mF324, mF43, mF4;
    
  
    
    public BooleanMatrixTest(String arg0) {
        super(arg0);
        f = BooleanFactory.factory(NUM_VARS);
        vars = new BooleanVariable[NUM_VARS];
        for (int i = 0; i < NUM_VARS; i++) { vars[i] = f.variable(i+1); }
        final int[] dims324 = { 3, 2, 4 }, dims43 = {4, 3}, dims4 = {4};
        dim324 = Dimensions.rectangular(dims324);
        dim43 = Dimensions.rectangular(dims43);
        dim4 = Dimensions.rectangular(dims4);
      
        mR = new IntRange[6];
        mR[0] = range(0,3); mR[1] = range(4,8);
        mR[2] = range(9,10); mR[3] = range(11,14);
        mR[4] = range(15,15); mR[5] = range(16,23);
        mCells = new BooleanValue[dim324.capacity()];
    }
    
    /*
     * @see TestCase#setUp()
     */
    protected void setUp() throws Exception {
        super.setUp();   
        mT324 = f.matrix(dim324, TRUE);
        mF324 = f.matrix(dim324, FALSE); 
        mF43 = f.matrix(dim43, FALSE);
        mF4 = f.matrix(dim4, FALSE);
        Arrays.fill(mCells, FALSE);
    }

    /*
     * @see TestCase#tearDown()
     */
    protected void tearDown() throws Exception {
        super.tearDown();
    }

    /**
     * @return true if all cells in m have the same value as the corresponding
     * cell in the given array of formulas.  The method assumes that m.dimensions.capacity==cells.length.
     */
    private static final boolean equivalent(BooleanMatrix m, BooleanValue[] cells) {
        for (int i = 0; i < cells.length; i++) {
            if (m.get(i)!=cells[i]) {
                System.out.println(i + ": " + m.get(i) + " vs. " + cells[i]);
                return false;
            }
        }
        return true;
    }
    
    /**
     * @return true if all cells in m have the same value as the corresponding
     * cell in bs.  The method assumes that m.dimensions==b.dimensions.
     */
    private static final boolean equivalent(BooleanMatrix m, BooleanMatrix b) {
        if (!(equivalent(m.dimensions(), b.dimensions()) && m.density()==b.density() &&
              m.zero().equals(b.zero()))) return false;
        int max = m.dimensions().capacity();
        for (int i = 0; i < max; i++) {
            if (m.get(i)!=b.get(i)) return false;
        }
        return true;
    }
    
    /**
     * @return true if dim0 and dim1 represent the same dimensions
     */
    private static final boolean equivalent(Dimensions dim0, Dimensions dim1) {
        if (dim0.numDimensions()==dim1.numDimensions() && dim0.capacity()==dim1.capacity()) {
            for (int i= 0; i < dim0.numDimensions(); i++) {
                if (dim0.dimension(i)!=dim1.dimension(i)) return false;
            }
            return true;
        }
        return false;
        
    }
    
    /**
     * @effects sets the cells in m and the array, from range.min() to range.max(), to the corresponding values in vars.
     */
    private final void fill(BooleanMatrix m, BooleanValue[] cells, IntRange range) {
        for (int i = range.min(); i <= range.max(); i++) { 
            m.set(i, vars[i]); 
            cells[i] = vars[i]; 
        }
    }
    
    /**
     * @effects sets the cells in m, from range.min() to range.max(), to the corresponding values in vars.
     */
    private final void fill(BooleanMatrix m, IntRange range) {
        for (int i = range.min(); i <= range.max(); i++) { 
            m.set(i, vars[i]); 
        }
    }
    
    /**
     * @effects m'.elements = m.elements ++ index->m.zero && cells'[index] = m.zero
     */
    private static final void blank(BooleanMatrix m, BooleanValue[] cells, int index) {
        m.set(index, m.zero());
        cells[index] = m.zero();
    }
    
    private static final IntRange range(int min, int max) {
        return Ints.range(min, max);
    }
   
    private void assertEquals(IntRange range, Iterator<IndexedEntry<BooleanValue>> indexIter) {
        for(int i = range.min(); i <= range.max(); i++) {
            assertEquals(i, indexIter.next().index());
        }
    }
    public final void testSetAndGet() {
        // set regions [4..8], [11..14], [16..23], [9..10] to variables
        fill(mF324, mCells, mR[1]); fill(mF324, mCells, mR[3]); 
        fill(mF324, mCells, mR[5]); fill(mF324, mCells, mR[2]);
        assertTrue(equivalent(mF324, mCells));
        
        // check that the dense regions in the matrix are [4..14] and [16..23]
        Iterator<IndexedEntry<BooleanValue>> indeces = mF324.iterator();
        assertEquals(Ints.merge(Ints.merge(mR[1], mR[2]), mR[3]), indeces);
        assertEquals(mR[5], indeces);
                
        // wipe out 4, 14, 23, and 10
        blank(mF324, mCells,mR[1].min()); blank(mF324, mCells, mR[3].max()); 
        blank(mF324, mCells, mR[5].max()); blank(mF324, mCells, mR[2].max());
        assertTrue(equivalent(mF324, mCells));
        // add 4 again
        fill(mF324, mCells, range(mR[1].min(),mR[1].min()));
        indeces = mF324.iterator();
        assertEquals(Ints.merge(range(mR[1].min(), mR[1].max()), range(mR[2].min(), mR[2].max()-1)), indeces);
        assertEquals(range(mR[2].max()+1, mR[3].max()-1), indeces);
        assertEquals(range(mR[5].min(), mR[5].max()-1), indeces);
        
        //System.out.println(mF324);
    }

    public final void testNot() {
        
        fill(mT324, mCells, mR[1]); fill(mT324,mCells,mR[3]);
        for (int i = mR[1].min(); i<= mR[1].max(); i++) { mCells[i] = f.not(mCells[i]); }
        for (int i = mR[3].min(); i<= mR[3].max(); i++) { mCells[i] = f.not(mCells[i]); }
        mT324.set(mR[1].max()+1,FALSE); mCells[mR[1].max()+1] = TRUE;
        
        BooleanMatrix mNot = mT324.not();
        
        assertEquals(mT324.density(), mNot.density());
        assertTrue(equivalent(mT324.dimensions(), mNot.dimensions()));
        assertTrue(equivalent(mNot,mCells));
        Iterator<IndexedEntry<BooleanValue>> miter = mT324.iterator(), mniter = mNot.iterator();
        while (miter.hasNext()) { assertEquals(miter.next().index(),mniter.next().index()); }
        
        //System.out.println(mT324);
        //System.out.println(mNot);
    }

    public final void testAnd() {
        assertTrue(equivalent(mT324, mT324.compose(AND, mT324))); 
        assertTrue(equivalent(mF324, mF324.compose(AND, mF324)));
        assertTrue(equivalent(mF324, mF324.compose(AND, mT324)));
        assertTrue(equivalent(mT324.compose(AND, mF324), mF324.compose(AND, mT324)));
        
          
        for (int i = mR[2].min(); i <= mR[2].max(); i++) {
            mT324.set(i, vars[i]);
            mF324.set(i, vars[2*i % vars.length]);
            mCells[i] = f.compose(AND, vars[i], vars[2*i % vars.length]);
        }
        assertTrue(equivalent(mT324.compose(AND, mF324), mCells));
        
        mT324.set(mR[4].min(), vars[mR[4].min()]);
        mF324.set(mR[3].min(), vars[mR[3].min()]);
        mCells[mR[3].min()] = vars[mR[3].min()];
        assertTrue(equivalent(mT324.compose(AND, mF324), mCells));
        
        //System.out.println(mT324);
        //System.out.println(mF324);
        
    }

    public final void testOr() {
        assertTrue(equivalent(mT324, mT324.compose(OR,  mT324))); 
        assertTrue(equivalent(mF324, mF324.compose(OR, mF324)));
        assertTrue(equivalent(mT324, mT324.compose(OR, mF324)));
        assertTrue(equivalent(mT324.compose(OR, mF324), mF324.compose(OR, mT324)));
        
        Arrays.fill(mCells, TRUE);
                
        for (int i = mR[1].min(); i <= mR[1].max(); i++) {
            mT324.set(i, vars[i]);
            mF324.set(i, vars[2*i % vars.length]);
            mCells[i] = f.compose(OR, vars[i], vars[2*i % vars.length]);
        }
        assertTrue(equivalent(mT324.compose(OR, mF324), mCells));
        
        mT324.set(mR[0].max(), vars[mR[0].max()]);
        mF324.set(mR[2].min(), vars[mR[2].min()]);
        mCells[mR[0].max()] = vars[mR[0].max()];
        assertTrue(equivalent(mT324.compose(OR, mF324), mCells));
        
        //System.out.println(mT324);
        //System.out.println(mF324);
        //System.out.println(Arrays.asList(mCells));
        
    }
    
    public final void testFold() {
        assertEquals(FALSE, mF324.andFold());
        assertEquals(FALSE, mF324.orFold());
        assertEquals(TRUE, mT324.andFold());
        assertEquals(TRUE, mT324.orFold());
        
        mF324.set(mR[0].max(), vars[mR[0].max()]);
        mF324.set(mR[5].max(), vars[mR[5].max()]);
        assertEquals(FALSE, mF324.andFold());
        assertTrue(f.compose(OR, vars[mR[0].max()], vars[mR[5].max()])==mF324.orFold());
        
        mT324.set(mR[2].max(), vars[mR[2].max()]);
        mT324.set(mR[3].max(), vars[mR[3].max()]);
        assertEquals(TRUE, mT324.orFold());
        assertTrue(f.compose(AND, vars[mR[2].max()], vars[mR[3].max()])==mT324.andFold());
        
    }
    
    private  final void assertDotProductFalse(BooleanMatrix mF, BooleanMatrix m) {
        BooleanMatrix product = mF.dot(m); 
        assertEquals(FALSE, product.zero());
        assertEquals(0, product.density());
        assertTrue(equivalent(mF.dimensions().dot(m.dimensions()), product.dimensions()));
    }
    
    public final void testDotProduct() {
        fill(mF43, range(0, dim43.capacity()-1));
        fill(mF4, range(0, dim4.capacity()-1));
        
        assertDotProductFalse(mF324, mF43);
        assertDotProductFalse(mF324, mF4);
                
        fill(mF324, range(0, dim324.capacity()-1));
           
        
        BooleanValue[] result = new BooleanValue[dim324.dot(dim43).capacity()];
        Arrays.fill(result, FALSE);
        int rows43 = dim324.dimension(dim324.numDimensions()-1);
        int rows324 = dim324.capacity() / rows43;
        int cols43 = dim43.capacity() / rows43;
        for (int i = 0; i < rows324; i++) {
            for (int j = 0; j < cols43; j++) {
                for (int k = 0; k < rows43; k++) {
                    int index324 = i*rows43 + k;
                    int index43 = j+ k*cols43;
                    int indexR = cols43*i + j;
                    result[indexR] = f.compose(OR, result[indexR], f.compose(AND, mF324.get(index324), mF43.get(index43)));
                }
            }
        }
        
        assertTrue(equivalent(mF324.dot(mF43), result));
        
        for (int i = 0; i < dim324.capacity(); i += 2) { mF324.set(i, FALSE); }
        for (int i = 1; i < dim4.capacity(); i += 2) { mF4.set(i, FALSE); }
        
        assertDotProductFalse(mF324, mF4);
        
    }

    private  final void assertCrossProductFalse(BooleanMatrix mF, BooleanMatrix m) {
        BooleanMatrix product = mF.cross(m); 
        assertEquals(FALSE, product.zero());
        assertEquals(0, product.density());
        assertTrue(equivalent(mF.dimensions().cross(m.dimensions()), product.dimensions()));
    }
    
    public final void testCrossProduct() {
        final BooleanMatrix mT43 = f.matrix(dim43, TRUE);
        
        fill(mT43, range(0, dim43.capacity()-1));
        fill(mF4, range(0, dim4.capacity()-1));
        
        assertCrossProductFalse(mF324, mT43);
        assertCrossProductFalse(mF324, mF4);
           
        fill(mT324, range(0, dim324.capacity()-1));
             
        BooleanValue[] result = new BooleanValue[dim324.cross(dim43).capacity()];
        Arrays.fill(result, TRUE);
        
        final int c324 = dim324.capacity(), c43 = dim43.capacity();
        final int c32443 = c324*c43;
        
        for (int i = 0; i < c32443; i++) {
            result[i] = f.compose(AND, mT324.get(i / c43), mT43.get(i % c43));
        }
        assertTrue(equivalent(mT324.cross(mT43), result));
        
        mT324.set(1, TRUE);
        for (int i = c43; i < c43*2; i++) {
            result[i] = mT43.get(i % c43);
        }
     
        assertTrue(equivalent(mT324.cross(mT43), result));
        
        mT43.set(5, TRUE);
        for (int i = 0; i < c324; i++) {
            result[i*c43 + 5] = mT324.get(i);
        }

        assertTrue(equivalent(mT324.cross(mT43), result));
        
        mT324.set(10, FALSE);
        for (int i = c43*10; i < c43*11; i++) {
            result[i] = FALSE;
        }

        assertTrue(equivalent(mT324.cross(mT43), result));
        //System.out.println(Arrays.asList(result));
        //System.out.println(mT324.crossProduct(mT43));
     
    }
    
    public final void testTranspose() {
        BooleanMatrix mF43t = mF43.transpose();
        assertEquals(mF43.density(), mF43t.density());
        assertTrue(equivalent(dim43.transpose(), mF43t.dimensions()));
        
        fill(mF43, range(0, dim43.capacity()-1));
        BooleanValue[] result = new BooleanValue[dim43.capacity()];
        final int a = dim43.dimension(0), b = dim43.dimension(1);
        for (int i = 0; i < a; i++) {
            for (int j = 0; j < b; j++) {
                result[j*a+i] = vars[i*b+j];
            }
        }
        
        mF43t = mF43.transpose();
        assertEquals(mF43.density(), mF43t.density());
        assertTrue(equivalent(dim43.transpose(), mF43t.dimensions()));
        assertTrue(equivalent(mF43t,result));
      
    }
    
    public final void testClosure() {
        BooleanMatrix mF44 =  f.matrix(Dimensions.square(2,4), FALSE);
        assertTrue(equivalent(mF44, mF44.closure()));
        
        mF44.set(0, vars[0]);
        mF44.set(9, vars[9]);
        assertTrue(equivalent(mF44, mF44.closure()));
        
        mF44.set(2, vars[2]);
        
        BooleanValue[] result = new BooleanValue[mF44.dimensions().capacity()];
        for (int i = 0 ; i < result.length; i++) { result[i] = FALSE; }
        result[0] = vars[0];
        result[1] = f.compose(AND, vars[2], vars[9]);
        result[1] = f.compose(OR, result[1], f.compose(AND, vars[0], result[1]));
        result[2] = vars[2];
        result[9] = vars[9];
       
        assertTrue(equivalent(mF44.closure(), result));
        
        mF44.set(7, vars[7]);
        result[7] = vars[7];
        result[3] = f.compose(AND, vars[2], f.compose(AND, vars[9], vars[7]));
        result[11] = f.compose(AND,vars[7], vars[9]);
        
        
        assertTrue(equivalent(mF44.closure(), result));
        
        //System.out.println(mF44.closure());
        
        
    }
    
  
}
