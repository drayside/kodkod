package kodkod.engine.satlab;

import org.junit.*;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;
import static org.junit.Assume.*;
import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import kodkod.engine.satlab.SATFactory;
import kodkod.engine.satlab.SATSolver;

@RunWith(JUnit4.class)
public class Z3Test {

    SATSolver solver;

    @Before
    public void setUp() {
        assumeTrue(SATFactory.available(SATFactory.Z3));
        solver = SATFactory.Z3.instance();
    }

    @After
    public void cleanup() {
        if (solver != null) {
            solver.free();
            solver = null;
        }
    }

    @Test
    public void singleVariableFormula() {
        assertThat(solver.numberOfVariables(), is(0));
        assertThat(solver.numberOfClauses(), is(0));

        // Add a single variable.
        solver.addVariables(1);

        assertThat(solver.numberOfVariables(), is(1));
        assertThat(solver.numberOfClauses(), is(0));

        // Add a single clause with that variable.
        solver.addClause(new int[]{1});

        assertThat(solver.numberOfVariables(), is(1));
        assertThat(solver.numberOfClauses(), is(1));

        boolean satisfiable = solver.solve();

        assertTrue(satisfiable);
        assertThat(solver.valueOf(1), is(true));
        assertThat(solver.numberOfVariables(), is(1));
        assertThat(solver.numberOfClauses(), is(1));
    }

    @Test
    public void singleNegatedVariableFormula() {
        assertThat(solver.numberOfVariables(), is(0));
        assertThat(solver.numberOfClauses(), is(0));

        // Add a single variable.
        solver.addVariables(1);

        assertThat(solver.numberOfVariables(), is(1));
        assertThat(solver.numberOfClauses(), is(0));

        // Add a single clause with the negation of that variable.
        solver.addClause(new int[]{-1});

        assertThat(solver.numberOfVariables(), is(1));
        assertThat(solver.numberOfClauses(), is(1));

        boolean satisfiable = solver.solve();

        assertTrue(satisfiable);
        assertThat(solver.valueOf(1), is(false));
        assertThat(solver.numberOfVariables(), is(1));
        assertThat(solver.numberOfClauses(), is(1));
    }

    @Test
    public void simpleContradiction() {
        assertThat(solver.numberOfVariables(), is(0));
        assertThat(solver.numberOfClauses(), is(0));

        // Add a single variable.
        solver.addVariables(1);

        assertThat(solver.numberOfVariables(), is(1));
        assertThat(solver.numberOfClauses(), is(0));

        // Add a clause with the variable.
        solver.addClause(new int[]{1});

        // Add a clause with the negation of the variable.
        solver.addClause(new int[]{-1});

        assertThat(solver.numberOfVariables(), is(1));
        assertThat(solver.numberOfClauses(), is(2));

        boolean satisfiable = solver.solve();

        assertFalse(satisfiable);
        assertThat(solver.numberOfVariables(), is(1));
        assertThat(solver.numberOfClauses(), is(2));
    }

    @Test
    public void solve3Variables() {
        assertThat(solver.numberOfVariables(), is(0));
        assertThat(solver.numberOfClauses(), is(0));

        // Add 3 variables.
        solver.addVariables(3);

        assertThat(solver.numberOfVariables(), is(3));
        assertThat(solver.numberOfClauses(), is(0));

        // Add clause (1 or 2 or 3).
        solver.addClause(new int[]{1, 2, 3});

        // Add clause (~1 or ~2 or ~3).
        solver.addClause(new int[]{-1, -2, -3});

        // Add clause (~1 or ~2).
        solver.addClause(new int[]{-1, -2});

        // Add clause (~3).
        solver.addClause(new int[]{-3});

        assertThat(solver.numberOfVariables(), is(3));
        assertThat(solver.numberOfClauses(), is(4));

        boolean satisfiable = solver.solve();

        assertTrue(satisfiable);
        assertThat(solver.numberOfVariables(), is(3));
        assertThat(solver.numberOfClauses(), is(4));
    }
}