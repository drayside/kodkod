/* 
 * Kodkod -- Copyright (c) 2005-present, Emina Torlak
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

import java.util.Stack;

/**
 * Java wrapper for the MiniSat solver by Niklas E&eacute;n and Niklas S&ouml;rensson.
 * @author Emina Torlak
 */
final class MiniSat extends NativeSolver implements CheckpointableSolver {
  Stack<Long> solverCheckpoints;

	/**
	 * Constructs a new MiniSAT wrapper.
	 */
	public MiniSat() {
		super(make());
    solverCheckpoints = new Stack<Long>();
	}
	
	static {
		loadLibrary(MiniSat.class);
	}

	
	/**
	 * {@inheritDoc}
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return "MiniSat";
	}
	
	/**
	 * Returns a pointer to an instance of  MiniSAT.
	 * @return a pointer to an instance of minisat.
	 */
	private static native long make();

  /**
   * Returns a pointer to a copy of a MiniSAT instance.
   * @return a pointer to a copy of a MiniSAT instance.
   */
  private static native long make_copy(long original);
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.satlab.NativeSolver#free(long)
	 */
	native void free(long peer);
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.satlab.NativeSolver#addVariables(long, int)
	 */
	native void addVariables(long peer, int numVariables);

	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.satlab.NativeSolver#addClause(long, int[])
	 */
	native boolean addClause(long peer, int[] lits);
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.satlab.NativeSolver#solve(long)
	 */
	native boolean solve(long peer);
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.satlab.NativeSolver#valueOf(long, int)
	 */
	native boolean valueOf(long peer, int literal);

  public void checkpoint() {
    long copy = make_copy(this.peer());
    System.out.println("Checkpointing, made copy at " + copy);
    solverCheckpoints.push(this.peer());
    this.setPeer(copy);
  }

  public void rollback() {
    long newPeer = solverCheckpoints.pop();
    System.out.println("Rollingback, new peer is at " + newPeer);
    System.out.println("Freeing peer at " + this.peer());
    free(this.peer());
    this.setPeer(newPeer);
    System.out.println("Finished Rollback, peer is at " + this.peer());
  }

  public int numberOfCheckpoints() {
    return solverCheckpoints.size();
  }

  @Override
  public synchronized void free() {
    super.free();
    while (solverCheckpoints.size() > 0) {
      free(solverCheckpoints.pop());
    } 
  }
}
