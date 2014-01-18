package kodkod.engine.satlab;

import java.lang.RuntimeException;
import java.util.Stack;
import java.util.HashMap;
import com.microsoft.z3.Solver;
import com.microsoft.z3.Context;
import com.microsoft.z3.Z3Exception;
import com.microsoft.z3.Expr;
import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Symbol;
import com.microsoft.z3.Status;
import com.microsoft.z3.Model;
import com.microsoft.z3.Tactic;

/**
* A wrapper class that provides access to the basic functionality
* of the Z3 solver from Microsoft Research.
*/
final class Z3 implements CheckpointableSolver {
    private Solver solver;
    private Context context;
    private Tactic satTactic;

    private int vars;
    private int clauses;
    private Status last_status;
    private Model last_model;

    private HashMap<Integer, BoolExpr> expressionCache;
    private Stack<Checkpoint> checkpoints;

    private class Checkpoint {
        private int vars;
        private int clauses;
        private Status status;
        private Model model;

        public Checkpoint(int vars, int clauses, Status status, Model model) {
            this.vars = vars;
            this.clauses = clauses;
            this.status = status;
            this.model = model;
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

        public Model getModel() {
            return model; 
        }
    }

    Z3() {
        expressionCache = new HashMap<Integer, BoolExpr>();
        try {
            context = new Context();
            satTactic = context.ParAndThen(context.MkTactic("sat-preprocess"),
                                           context.MkTactic("sat"));
            solver = context.MkSolver(satTactic);
        } catch (Z3Exception e) {
            throw new RuntimeException(e);
        }

        this.vars = 0;
        this.clauses = 0;
        this.last_status = null;
        this.last_model = null;
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
                literals[i] = getExpressionForLiteral(lits[i]);
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
                last_model = solver.Model();
                return true;
            } else if (last_status == Status.UNSATISFIABLE) {
                last_model = null;
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
            Model model = last_model;
            BoolExpr variable_expression = getExpressionForLiteral(variable);

            Expr value_expression = model.Eval(variable_expression, true);

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
            checkpoints.push(new Checkpoint(vars, clauses, last_status, last_model));
        } catch (Z3Exception e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * {@inheritDoc}
     * @see kodkod.engine.satlab.CheckpointableSolver#rollback()
     */
    public void rollback() {
        if (numberOfCheckpoints() <= 0) {
            throw new IllegalStateException("No checkpoints to rollback to.");
        }
        try {
            solver.Pop();
            Checkpoint checkpoint = checkpoints.pop();
            this.vars = checkpoint.getVars();
            this.clauses = checkpoint.getClauses();
            this.last_status = checkpoint.getStatus();
            this.last_model = checkpoint.getModel();
        } catch (Z3Exception e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * {@inheritDoc}
     * @see kodkod.engine.satlab.SATSolver#free()
     */
    public synchronized final void free() {
        checkpoints = null;
        solver = null;
        context.Dispose();
        context = null;
        expressionCache = null;
    }

    private BoolExpr getExpressionForLiteral(int literal) throws Z3Exception {
      if (literal == 0) {
        throw new IllegalArgumentException("Parameter must be nonzero.");
      }

      BoolExpr expr = expressionCache.get(literal);
      if (expr == null) {
        if (literal > 0) {
          Symbol sym = context.MkSymbol(literal);
          expr = context.MkBoolConst(sym);
        } else {
          expr = context.MkNot(getExpressionForLiteral(-literal)); 
        }
        expressionCache.put(literal, expr);
      }

      return expr;
    }
}
