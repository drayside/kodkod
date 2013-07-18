package kodkod.multiobjective.EndtoEnd;

import org.junit.*;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;
import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;
import static org.junit.Assume.*;

import java.util.Arrays;
import java.util.List;
import java.util.TreeSet;
import java.util.Vector;
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
import kodkod.multiobjective.algorithms.*;
import kodkod.multiobjective.concurrency.*;

@RunWith(JUnit4.class)
public class CGIASmallEndToEndTest {
	/*Fields*/
	private MooProblem moo_problem;
	
	/*
	 * This test is a translation of rooks_3_metrics_2.als
	 */

	@Before
	public void setUp() {
		// Need to have Z3 available since Z3 is the only checkpointed solver.
		assumeTrue(SATFactory.available(SATFactory.MiniSat));
		moo_problem = new rooks_3_metrics_2();
	}

	@Test
	public void WithSymmetryBreaking() {
		MultiObjectiveProblem problem = moo_problem.getProblem();
		CheckpointedGuidedImprovementAlgorithm igia = new CheckpointedGuidedImprovementAlgorithm("asdf", new MultiObjectiveOptions());
		igia.getOptions().setSolver(SATFactory.MiniSat);
		igia.getOptions().setSymmetryBreaking(1000);

		SolutionNotifier notifier = new SolutionNotifier() {
			List<MeasuredSolution> solutions = new Vector<MeasuredSolution>();

			public void tell(final MeasuredSolution s) {
				solutions.add(s);
			}

			public void tell(Solution s, MetricPoint values) {
				tell(new MeasuredSolution(s, values));
			}

			public void done() {
				// There should be a single solution.
				assertThat(solutions.size(), is(1));

				MeasuredSolution solution = solutions.get(0);
				MetricPoint mp = solution.getValues();

				// objective 0 should have value 6
				assertThat(mp.getValue((Objective)moo_problem.getObjectives().toArray()[0]), is(6));

				// objective 1 should have value 7
				assertThat(mp.getValue((Objective)moo_problem.getObjectives().toArray()[1]), is(7));
			}
		};

		igia.multiObjectiveSolve(problem, notifier);
	}

	@Test
	public void WithoutSymmetryBreaking() {
		MultiObjectiveProblem problem = moo_problem.getProblem();
		CheckpointedGuidedImprovementAlgorithm igia = new CheckpointedGuidedImprovementAlgorithm("asdf", new MultiObjectiveOptions());
		igia.getOptions().setSolver(SATFactory.MiniSat);
		igia.getOptions().setSymmetryBreaking(0);

		SolutionNotifier notifier = new SolutionNotifier() {
			List<MeasuredSolution> solutions = new Vector<MeasuredSolution>();

			public void tell(final MeasuredSolution s) {
				solutions.add(s);
			}

			public void tell(Solution s, MetricPoint values) {
				tell(new MeasuredSolution(s, values));
			}

			public void done() {
				// There should be 6 solutions, 1 for each permutation of rook
				// positions.
				assertThat(solutions.size(), is(6));

				// Each solution should have the same metric values.
				for (MeasuredSolution solution : solutions) {
					MetricPoint mp = solution.getValues();

					// objective 0 should have value 6
					assertThat(mp.getValue((Objective)moo_problem.getObjectives().toArray()[0]), is(6));

					// objective 1 should have value 7
					assertThat(mp.getValue((Objective)moo_problem.getObjectives().toArray()[1]), is(7));
				}
			}
		};

		igia.multiObjectiveSolve(problem, notifier);
	}
}
