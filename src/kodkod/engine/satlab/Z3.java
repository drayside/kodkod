package kodkod.engine.satlab;

import java.lang.RuntimeException;
import java.util.Stack;
import com.microsoft.z3.Solver;
import com.microsoft.z3.Context;
import com.microsoft.z3.Z3Exception;
import com.microsoft.z3.Expr;
import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Symbol;
import com.microsoft.z3.Status;
import com.microsoft.z3.Model;

/**
* A wrapper class that provides access to the basic functionality
* of the Z3 solver from Microsoft Research.
*/
final class Z3 implements CheckpointableSolver {
    private Solver solver;
    private Context context;
    
    private int vars;
    private int clauses;
    private Status last_status;

    private Stack<Checkpoint> checkpoints;

    private class Checkpoint {
        private int vars;
        private int clauses;
        private Status status;

        public Checkpoint(int vars, int clauses, Status status) {
            this.vars = vars;
            this.clauses = clauses;
            this.status = status;
        }

        public int getVars() {
            return vars;
        }

        public int getClauses() {
            return clauses;
        }

        public Status getStatus() {
            return status;
        }
    }

    Z3() {
        try {
            context = new Context();
            solver = context.MkSimpleSolver();
        } catch (Z3Exception e) {
            throw new RuntimeException(e);
        }

        this.vars = 0;
        this.clauses = 0;
        this.last_status = null;
        checkpoints = new Stack<Checkpoint>();
    }

    /**
     * {@inheritDoc}
     * @see kodkod.engine.satlab.SATSolver#numberOfVariables()
     */
    public int numberOfVariables() {
        return vars;
    }

    /**
     * {@inheritDoc}
     * @see kodkod.engine.satlab.SATSolver#numberOfClauses()
     */
    public int numberOfClauses() {
        return clauses;
    }

    /**
     * {@inheritDoc}
     * @see kodkod.engine.satlab.CheckpointableSolver#numberOfCheckpoints()
     */
    public int numberOfCheckpoints() {
        try {
            return solver.NumScopes();
        } catch (Z3Exception e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * {@inheritDoc}
     * @see kodkod.engine.satlab.SATSolver#addVariables(int)
     */
    public void addVariables(int numVars) {
        if (numVars < 0) {
            throw new IllegalArgumentException("numVars < 0: " + numVars);
        } else if (numVars > 0) {
            vars += numVars;
        }
    }

    /**
     * {@inheritDoc}
     * @see kodkod.engine.satlab.SATSolver#addClause(int[])
     */
    public boolean addClause(int[] lits) {
        if (lits == null) {
            throw new NullPointerException();
        }
        try {
            BoolExpr[] literals = new BoolExpr[lits.length];

            for (int i = 0; i < lits.length; i += 1) {
                int lit = Math.abs(lits[i]);
                if (lit == 0 || lit > vars) {
                    throw new IllegalArgumentException("Illegal variable: " + lit);
                }
                Symbol sym = context.MkSymbol(lit);
                BoolExpr expr = context.MkBoolConst(sym);
                boolean negated = (lits[i] < 0);
                if (negated) {
                    literals[i] = context.MkNot(expr);
                } else {
                    literals[i] = expr;
                }
            }

            BoolExpr clause = context.MkOr(literals);

            solver.Assert(clause);

            clauses += 1;
        } catch (Z3Exception e) {
            throw new RuntimeException(e);
        }
        return true;
    }

    /**
     * {@inheritDoc}
     * @see kodkod.engine.satlab.SATSolver#solve()
     */
    public boolean solve() {
        try {
            last_status = solver.Check();

            if (last_status == Status.SATISFIABLE) {
                return true;
            } else if (last_status == Status.UNSATISFIABLE) {
                return false;
            } else {
                throw new RuntimeException("Result was UNKNOWN. " + solver.ReasonUnknown());
            }
        } catch (Z3Exception e) {
            throw new RuntimeException(e);
        }

    }

    /**
     * {@inheritDoc}
     * @see kodkod.engine.satlab.SATSolver#valueOf(int)
     */
    public final boolean valueOf(int variable) {
        if (variable > vars || variable <= 0) {
            throw new IllegalArgumentException("Unknown variable: " + variable);
        }
        if (last_status != Status.SATISFIABLE) {
            throw new IllegalStateException();
        }
        try {
            Model model = solver.Model();
            Symbol sym = context.MkSymbol(variable);
            BoolExpr variable_expression = context.MkBoolConst(sym);

            Expr value_expression = model.ConstInterp(variable_expression);

            if (value_expression.IsTrue()) {
                return true;
            } else if (value_expression.IsFalse()) {
                return false;
            } else {
                throw new RuntimeException("Variable " + variable + " has no constant assignment.");
            }
        } catch (Z3Exception e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * {@inheritDoc}
     * @see kodkod.engine.satlab.CheckpointableSolver#checkpoint()
     */
    public void checkpoint() {
        try {
            solver.Push();
            checkpoints.push(new Checkpoint(vars, clauses, last_status));
        } catch (Z3Exception e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * {@inheritDoc}
     * @see kodkod.engine.satlab.CheckpointableSolver#rollback()
     */
    public void rollback() {
        try {
            solver.Pop();
            Checkpoint checkpoint = checkpoints.pop();
            this.vars = checkpoint.getVars();
            this.clauses = checkpoint.getClauses();
            this.last_status = checkpoint.getStatus();
        } catch (Z3Exception e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * {@inheritDoc}
     * @see kodkod.engine.satlab.SATSolver#free()
     */
    public synchronized final void free() {
        solver = null;
        context.Dispose();
        context = null;
    }
}