/* 
 * Kodkod -- Copyright (c) 2005-2007, Emina Torlak
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package kodkod.engine.satlab;

/**
 * Wrapper for an instance of MinCostZChaff.
 * 
 * @author Emina Torlak
 */
final class ZChaffMincost extends NativeSolver implements SATMinSolver {

	/**
	 * Constructs an instance of ZChaffMincost.
	 */
	ZChaffMincost() {
		super(make());
	}

	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.satlab.SATMinSolver#setCost(int, int)
	 */
	public void setCost(int variable, int cost) {
		validateVariable(variable);
		if (cost < 0)
			throw new IllegalArgumentException("invalid cost: " + cost);
		setCost(peer(), variable, cost);
	}

	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.satlab.SATMinSolver#costOf(int)
	 */
	public int costOf(int variable) {
		validateVariable(variable);
		return costOf(peer(), variable);
	}
	
	/**
	 * {@inheritDoc}
	 * @see java.lang.Object#toString()
	 */
	public String toString() { 
		return "ZChaffMincost";
	}
	
	static {
		loadLibrary("zchaffmincost");
	}

	/**
	 * Creates an instance of zchaff and returns 
	 * its address in memory.  
	 * @return the memory address of an instance
	 * of the zchaff solver 
	 */
	private static native long make();
	
	/**
	 * Sets the cost of the given variable to the specified value in the 
	 * native zchaff instance at the given address.
	 * @requires  variable is a valid variable identifier && cost >= 0
	 * @effects sets the cost of the given variable to the specified value in the 
	 * native zchaff instance at the given address.
	 */
	private native void setCost(long peer, int variable, int cost);
	
	/**
	 * Retrieves the cost of the given variable in the native zchaff instance at the 
	 * given address.
	 * @requires variable is a valid variable identifier
	 * @return the cost of the given variable in the native zchaff instance at the 
	 * given address.
	 */
	private native int costOf(long peer, int variable);
	
	/**
	 * Releases the resources associated with
	 * the given instance of zchaff.  This method
	 * must be called when the object holding the
	 * given reference goes out of scope to avoid
	 * memory leaks.
	 * @effects releases the resources associated
	 * with the given instance of zchaff
	 */
	native void free(long peer);
	
	/**
	 * Adds the given number of variables
	 * to the instance of zchaff referenced
	 * by the first argument.
	 * @effects increases the vocabulary of the given instance
	 * by the specified number of variables
	 */
	native void addVariables(long peer, int numVariables);
	
	/**
	 * Adds the specified clause to the instance of zchaff referenced by the first argument.
	 * @requires all i: lits[int] | some l: [1..numVariables(p) | i = l || i = -l
	 * @effects adds the given clause to the specified instance of zchaff.
	 * @return clause index of the given clause, if it was added to the peer;
	 * a negative integer if not
	 */
	native int addClause(long peer, int[] lits);
	
	/**
	 * Calls the solve method on the instance of 
	 * zchaff referenced by the given long.
	 * @return true if sat, false otherwise
	 */
	native boolean solve(long peer);
	
	/**
	 * Returns the assignment for the given literal
	 * by the specified instance of zchaff:  1 means
	 * the variable is TRUE, 0 that it's FALSE, and 
	 * -1 that it is UNDECIDED.
	 * @requires the last call to {@link #solve(long) solve(peer)} returned SATISFIABLE
	 * @return the assignment for the given literal
	 */
	native boolean valueOf(long peer, int literal);
	
}
