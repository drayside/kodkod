package kodkod.engine.satlab;

import java.util.Arrays;

/**
 * Provides a java native interface to the 
 * zchaff solver from Princeton.  
 * @author Emina Torlak
 */
final class ZChaffJNI {
	private ZChaffJNI() { }
	
	static {
	    System.load("/Users/emina/Documents/workspace3.1/relations/zchaff_withcore/zchaffJNI2.jnilib");
	}
	
	static final int	 UNDETERMINED = 0, UNSATISFIABLE = 1, SATISFIABLE = 2,
					 TIME_OUT = 3, MEM_OUT = 4, ABORTED = 5;
	
	static final int TRUE = 1, FALSE = 0, UNDECIDED = -1;
	
	/**
	 * Creates an instance of zchaff and returns 
	 * its address in memory.  The returned instance
	 * of zchaff will have its unsat core functionality
	 * enabled or disabled depending on the value of the
	 * enableCoreExtraction flag.
	 * @return the memory address of an instance
	 * of the zchaff solver that provides core extraction
	 * functionality if the enableCoreExtraction flag is true.
	 */
	static native long make(boolean enableCoreExtraction);
	
	/**
	 * Releases the resources associated with
	 * the given instance of zchaff.  This method
	 * must be called when the object holding the
	 * given reference goes out of scope to avoid
	 * memory leaks.
	 * @effects releases the resources associated
	 * with the given instance of zchaff
	 */
	static native void free(long zchaff);
	
	/**
	 * Sets the timeout of the given instance of zchaff
	 * to the specified value.
	 * @effects sets the timeout to the given number of seconds
	 */
	static native void setTimeout(long zchaff, int timeout);
	
	/**
	 * Adds the given number of variables
	 * to the instance of zchaff referenced
	 * by the first argument.
	 * @effects increases the vocabulary of the given instance
	 * by the specified number of variables
	 */
	static native void addVariables(long zchaff, int numVariables);
	
	/**
	 * Returns the number of variables in the vocabulary of 
	 * the given instance of zchaff.
	 * @return the number of variables
	 */
	static native int numVariables(long zchaff);
	
	/**
	 * Adds the specified clause to the instance
	 * of zchaff referenced by the first argument.
	 * @requires all i: lits[int] | some l: [1..numVariables(zchaff) | 
	 *            i = l || i = -l
	 * @effects adds the given clause to the specified instance of zchaff.
	 */
	static native void addClause(long zchaff, int[] lits);
	
	/**
	 * Returns the number of clauses in the pool of clauses
	 * of the given instance of zchaff.
	 * @return the number of clauses
	 */
	static native int numClauses(long zchaff);
	
	/**
	 * Calls the solve method on the instance of 
	 * zchaff referenced by the given long.
	 * @return an integer in the range [0..5], indicating the solver's
	 * status as follows: UNDETERMINED = 0, UNSATISFIABLE = 1, SATISFIABLE = 2,
	 * TIME_OUT = 3, MEM_OUT = 4, ABORTED = 5.
	 */
	static native int solve(long zchaff);
	
	/**
	 * Returns the assignment for the given literal
	 * by the specified instance of zchaff:  1 means
	 * the variable is TRUE, 0 that it's FALSE, and 
	 * -1 that it is UNDECIDED.
	 * @requires the last call to {@link #solve(long) solve(zchaff)} returned SATISFIABLE
	 * @return the assignment for the given literal
	 */
	static native int valueOf(long zchaff, int literal);

	/**
	 * Returns the size of the unsatisfiable core produced by the
	 * given instance of zchaff.  
	 * @requires the given instance of zchaff was created using {@link #make(boolean) make(true)}
	 * and the last call to {@link #solve(long) solve(zchaff)} returned UNSATISFIABLE  
	 * @return the size of the unsat core produced by the given instance
	 * of zchaff.
	 */
	static native int coreSize(long zchaff);
	
	/**
	 * Returns the ith clause in the unsatisfiable core produced by the 
	 * given instance of zchaff. 
	 * @requires the given instance of zchaff was created using {@link #make(boolean) make(true)}
	 * and the last call to {@link #solve(long) solve(zchaff)} returned UNSATISFIABLE 
	 * @requires 0 <= i < {@link #coreSize(long) coreSize(zchaff) } 
	 * @return the ith clause in the unsatisfiable core of the given instance of zchaff
	 */
	static native int[] coreClause(long zchaff, int i);
	
	/**
	 * Removes all clauses from the given instance of zchaff that are not
	 * in the unsat core.
	 * @requires the given instance of zchaff was created using {@link #make(boolean) make(true)}
	 * and the last call to {@link #solve(long) solve(zchaff)} returned UNSATISFIABLE  
	 * @effects removes all clauses from the given instance of zchaff that are not
	 * in the unsat core.
	 */
	static native void retainCore(long zchaff);

	private static int[] pack(int... lits) {
		return lits;
	}
	
	public static void main(String[] args) {
		final long z = make(true);
		addVariables(z, 5);
		addClause(z, pack(1));
		addClause(z, pack(2));
		addClause(z, pack(3,4));
		addClause(z, pack(1, -5));
		addClause(z, pack(2, -5));
		addClause(z, pack(-1,-2,5));
		addClause(z, pack(-5));
		System.out.println(numVariables(z) + " " + numClauses(z));
		System.out.println(solve(z));
		System.out.println(coreSize(z));
		for(int i = coreSize(z)-1; i >= 0; i--) {
			System.out.println(Arrays.toString(coreClause(z,i)));
		}
		System.out.println(numClauses(z));
		retainCore(z);
		System.out.println("retained core...");
		System.out.println(numVariables(z) + " " + numClauses(z));
		System.out.println(solve(z));
		System.out.println(coreSize(z));
		for(int i = coreSize(z)-1; i >= 0; i--) {
			System.out.println(Arrays.toString(coreClause(z,i)));
		}
	}
}
