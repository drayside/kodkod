/*
 * GroundValueGenerator.java
 * Created on Aug 26, 2005
 */
package kodkod.engine.fol2sat;

import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

import kodkod.ast.Decl;
import kodkod.ast.Decls;
import kodkod.ast.Variable;
import kodkod.engine.bool.BooleanConstant;
import kodkod.engine.bool.BooleanFactory;
import kodkod.engine.bool.BooleanMatrix;
import kodkod.engine.bool.BooleanValue;
import kodkod.util.ints.IndexedEntry;


/** 
 * Generates all possible combinations of values that the quantified
 * variables in a given {@link kodkod.ast.Decls declarations}
 * object can take.  For example, suppose that we have the following
 * declarations--a: A, b: B, c: C--and that the sets A, B, C are comprised
 * of atoms {A0, A1, A2}, {B0, B1}, and {C0}.  Then, a GroundValueGenerator
 * for these declarations will generate the following variable-value mappings
 * for a, b, and c:  { (a, A0), (b, B0), (c, C0) }, { (a, A0), (b, B1), (c, C0) },
 * ..., { (a, A2), (b, B1), (c, C0) }. 
 * 
 * @specfield env: Environment<BooleanMatrix>
 * @specfield decls: Declarations
 * @specfield transls: [0..decls.size) ->one BooleanMatrix
 * @invariant all i: [0..decls.size) | translate(decls.declarations[i].expression) = transls[i]
 * @invariant env.map.BooleanMatrix = decls.declarations[int]
 * @author Emina Torlak 
 */
final class GroundValueGenerator {
    // FIXME:  clean this up
	private final Environment<BooleanMatrix> env;
    private final List<Decl> decls;
    private final BooleanMatrix[] transls;
    private final Iterator<IndexedEntry<BooleanValue>>[] translIters;
    private final int[] index;
    private int count, maxCount;
    
    /**  
     * Constructs a new GroundValueGenerator for the given declarations and 
     * their translations.  The  environment in which the variables will be
     * bound when next() is called will have the given environment as its parent.
     * 
     * @requires all i: [0..decls.size) | translate(decls.declarations[i].expression) = transls[i]
     * @effects this.decls' = decls && this.transls' = translations && this.env'.parent = baseEnvironment
     */
    @SuppressWarnings("unchecked") 
    GroundValueGenerator(Environment<BooleanMatrix> baseEnvironment, Decls decls, List<BooleanMatrix> translations) {
        this.env = baseEnvironment.extend(new IdentityHashMap<Variable, BooleanMatrix>(decls.size()));
        this.decls = decls.declarations();
        this.transls = new BooleanMatrix[decls.size()];
        this.translIters = new Iterator[transls.length];
        this.index = new int[transls.length];
        this.count = 1;
        for(int i = 0; i < transls.length; i++) {
        		transls[i] = translations.get(i);
        		count *= transls[i].density(); 
        	}
        this.maxCount = 2 * count;
        
    }
    
    
    /**
     * @return true if another mapping of variables to matrices can be generated,
     * otherwise returns false.
     */
    public boolean hasNext() { return count < maxCount; }
    
    /**
     * Returns the next variable-to-value mapping.
     * The factory is used to construct the 
     * matrices to which the variables in this.decls are mapped.  The returned environment is backed by
     * this GroundValueGenerator, and its mappings for the variables in this.decls
     * should not be modified.  If they are, subsequent calls to next may return wrong mappings.
     * @effects modifies this.env so that it contains a mapping from each 
     * {@link kodkod.ast.Variable variable} declared in this.decls to a
     * {@link kodkod.engine.bool.BooleanMatrix matrix} that represents the next 
     * value that variable can take.  
     * @return this.env'
     * @throws java.util.NoSuchElementException - !hasNext()
     */
    public Environment<BooleanMatrix> next(BooleanFactory factory) {
        if (!hasNext()) throw new NoSuchElementException("!hasNext()");
        
        for (int i = transls.length - 1, prevIndex = count - 1, curIndex = count; 
             i >= 0 ; i--) {
            
            int density = transls[i].density();
            Variable var = decls.get(i).variable();
            
            if (curIndex % density != prevIndex % density || env.get(var)==null) {
                if (curIndex % density == 0) { translIters[i] = transls[i].iterator(); }
                index[i] = translIters[i].next().index();
                BooleanMatrix varValue = factory.matrix(transls[i].dimensions(), BooleanConstant.FALSE);
                varValue.set(index[i], BooleanConstant.TRUE);
                env.bind(var, varValue);
            }
            
            curIndex = curIndex / density;
            prevIndex = prevIndex / density;
        }
        
        count++;
//        System.out.println(varsToValues + ", " + count + ", " + maxCount);
        return env;
    }
    
    /**
     * Let m be a matrix with the dimensions of the matrix transls[0]x...xtransls[decls.size], 
     * where 'x' stands for cross product.  Suppose that the previous call to 
     * {@link kodkod.engine.fol2sat.GroundValueGenerator#next next} returned the mappings 
     * { (a, A0), (b, B1), (c, C0) } for this.decls = a: A, b: B, and c: C.
     * Then, the returned integer array is an index into m which locates the tuple A0->B1->C0.
     * The return value is unspecified if no prior calls were made to next().  Modifying
     * the entries in the returned array may cause the subsequent calls to the methods
     * of this to return incorrect results. 
     */
    public int[] index() { return index; }
    
    /**
     * Returns the parent of this.environment.
     * @return this.env.parent
     */
    public Environment<BooleanMatrix> baseEnvironment() {
		return env.parent();
    }
    
}
