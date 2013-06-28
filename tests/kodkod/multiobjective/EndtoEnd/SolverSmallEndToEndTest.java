package kodkod.multiobjective.EndtoEnd;

import org.junit.*;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;
import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.assertThat;

import java.util.Arrays;
import java.util.List;
import java.util.TreeSet;
import java.util.Vector;
import java.util.Iterator;
import java.io.PrintWriter;
import java.io.FileWriter;
import kodkod.ast.*;
import kodkod.ast.operator.*;
import kodkod.instance.*;
import kodkod.engine.*;
import kodkod.engine.satlab.SATFactory;
import kodkod.engine.config.Options;

import kodkod.multiobjective.Testmodels.MooProblem;
import kodkod.multiobjective.Testmodels.rooks_3_metrics_2;
import kodkod.multiobjective.*;

@RunWith(JUnit4.class)
public class SolverSmallEndToEndTest {
	private MooProblem moo_problem;

	/*
	 * This test is a translation of rooks_3_metrics_2.als
	 */

	@Before
	public void setUp() {
		moo_problem = new rooks_3_metrics_2();
	}

	@Test
	public void TestEndToEnd() {
		MultiObjectiveSolver solver = new MultiObjectiveSolver();
		solver.options().setSolver(SATFactory.DefaultSAT4J);
		solver.options().setSymmetryBreaking(1000);

		Iterator<Solution> solutions = solver.solveAll(moo_problem.getFormula(), 
													   moo_problem.getBounds(),
													   moo_problem.getObjectives());
		
		int solutionCount = 0;
		while (solutions.hasNext()) {
			Solution solution = solutions.next();

			solutionCount += 1;
			assertThat(solution.sat(), is(true));
		}
		assertThat(solutionCount, is(1));
	}
}
