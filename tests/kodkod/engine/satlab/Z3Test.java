package kodkod.engine.satlab;

import org.junit.*;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;
import org.junit.rules.ExpectedException;
import static org.junit.Assume.*;
import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import kodkod.engine.satlab.SATFactory;
import kodkod.engine.satlab.CheckpointableSolver;

@RunWith(JUnit4.class)
public class Z3Test {

    CheckpointableSolver solver;

    @Before
    public void setUp() {
        assumeTrue(SATFactory.available(SATFactory.Z3));
        assumeTrue(SATFactory.Z3.checkpointable());
        solver = (CheckpointableSolver)SATFactory.Z3.instance();
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

        // Add clause (~1).
        solver.addClause(new int[]{-1});

        assertThat(solver.numberOfVariables(), is(3));
        assertThat(solver.numberOfClauses(), is(5));

        boolean satisfiable = solver.solve();

        assertTrue(satisfiable);

        assertThat(solver.valueOf(1), is(false));
        assertThat(solver.valueOf(2), is(true));
        assertThat(solver.valueOf(3), is(false));

        assertThat(solver.numberOfVariables(), is(3));
        assertThat(solver.numberOfClauses(), is(5));
    }

    @Test
    public void incremental3Variables() {
        assertThat(solver.numberOfVariables(), is(0));
        assertThat(solver.numberOfClauses(), is(0));

        // Add 3 variables.
        solver.addVariables(3);

        assertThat(solver.numberOfVariables(), is(3));
        assertThat(solver.numberOfClauses(), is(0));

        // Add clause (1 or 2 or 3).
        solver.addClause(new int[]{1, 2, 3});

        assertThat(solver.numberOfVariables(), is(3));
        assertThat(solver.numberOfClauses(), is(1));

        boolean satisfiable = solver.solve();

        assertTrue(satisfiable);

        // Add clause (~1).
        solver.addClause(new int[]{-1});

        assertThat(solver.numberOfVariables(), is(3));
        assertThat(solver.numberOfClauses(), is(2));

        satisfiable = solver.solve();

        assertTrue(satisfiable);

        // Add clause (~3).
        solver.addClause(new int[]{-3});

        assertThat(solver.numberOfVariables(), is(3));
        assertThat(solver.numberOfClauses(), is(3));

        satisfiable = solver.solve();

        assertTrue(satisfiable);
        assertThat(solver.valueOf(1), is(false));
        assertThat(solver.valueOf(2), is(true));
        assertThat(solver.valueOf(3), is(false));

        // Add clause (~2).
        solver.addClause(new int[]{-2});

        assertThat(solver.numberOfVariables(), is(3));
        assertThat(solver.numberOfClauses(), is(4));

        satisfiable = solver.solve();

        assertFalse(satisfiable);
    }

    @Test
    public void addingNegativeVariablesShouldThrowException() {
        try {
            solver.addVariables(-1);

            // Should never reach here.
            fail();
        } catch (IllegalArgumentException e) {

        }
    }

    @Test
    public void nullClauseShouldThrowException() {
        assertThat(solver.numberOfVariables(), is(0));
        assertThat(solver.numberOfClauses(), is(0));

        solver.addVariables(3);

        try {
            solver.addClause(null);

            // Should never reach here.
            fail();
        } catch (NullPointerException e) {

        }
    }

    @Test
    public void illegalVariableInClauseShouldThrowException() {
        solver.addVariables(3);

        try {
            solver.addClause(new int[] {1,2,4});

            // Should never reach here.
            fail();
        } catch (IllegalArgumentException e) {

        }
    }

    @Test
    public void illegalNegativeVariableInClauseShouldThrowException() {
        solver.addVariables(3);

        try {
            solver.addClause(new int[] {1,2,-4});

            // Should never reach here.
            fail();
        } catch (IllegalArgumentException e) {

        }
    }

    @Test
    public void illegalZeroVariableInClauseShouldThrowException() {
        solver.addVariables(3);

        try {
            solver.addClause(new int[] {1,2,0});

            // Should never reach here.
            fail();
        } catch (IllegalArgumentException e) {

        }
    }

    @Test
    public void illegalVariableInValueOfShouldThrowException() {
        solver.addVariables(1);
        solver.addClause(new int[] {1});
        solver.solve();
        try {
            solver.valueOf(2);

            // Should never reach here.
            fail();
        } catch (IllegalArgumentException e) {

        }

        try {
            solver.valueOf(0);

            // Should never reach here.
            fail();
        } catch (IllegalArgumentException e) {

        }

        try {
            solver.valueOf(-1);

            // Should never reach here.
            fail();
        } catch (IllegalArgumentException e) {

        }
    }

    @Test
    public void valueOfAfterUnsatShouldThrowException() {
        solver.addVariables(1);
        solver.addClause(new int[] {1});
        solver.addClause(new int[] {-1});
        boolean result = solver.solve();
        assertFalse(result);

        try {
            solver.valueOf(1);

            // Should never reach here.
            fail();
        } catch (IllegalStateException e) {

        }
    }

    @Test
    public void valueOfBeforeSolveShouldThrowException() {
        solver.addVariables(1);
        solver.addClause(new int[] {1});

        try {
            solver.valueOf(1);

            // Should never reach here.
            fail();
        } catch (IllegalStateException e) {

        }
    }

    @Test
    public void basicCheckpointing() {

    }
}