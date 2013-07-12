package kodkod.engine.satlab;

// TODO: Need to add documentation.
interface CheckpointableSolver extends SATSolver {
    public void checkpoint();
    public void rollback();
    public int numberOfCheckpoints();
}