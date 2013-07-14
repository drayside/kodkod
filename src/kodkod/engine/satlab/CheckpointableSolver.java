package kodkod.engine.satlab;

/**
 * Provides an interface to a SAT solver with checkpointing capability.
 *
 * @specfield variables: set [1..)
 * @specfield clauses: set Clause
 * @specfield checkpoints: set Checkpoint
 * @invariant all i: [2..) | i in variables => i-1 in variables
 * @invariant all c: clauses | all lit: c.literals | lit in variables || -lit in variables
 * @invariant all c: clauses | all disj i,j: c.literals | abs(i) != abs(j)
 */
interface CheckpointableSolver extends SATSolver {

    /**
     * Creates a checkpoint at the solver's current state and pushes it onto
     * the checkpoint stack.
     */
    public void checkpoint();

    /**
     * Pops a checkpoint off the checkpoint stack and reverts the solver state
     * to that of the checkpoint.
     * @throws IllegalStateException  #this.checkpoints <= 0
     */
    public void rollback();

    /**
     * Returns the number of checkpoints on the checkpoint stack.
     * @return #this.checkpoints
     */
    public int numberOfCheckpoints();
}