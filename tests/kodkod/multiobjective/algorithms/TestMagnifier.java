package kodkod.multiobjective.algorithms;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.*;

import java.util.Collection;
import java.util.List;
import java.util.Vector;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import kodkod.ast.Formula;
import kodkod.engine.Solution;
import kodkod.engine.satlab.SATFactory;
import kodkod.multiobjective.Testmodels.MooProblem;
import kodkod.multiobjective.Testmodels.rooks_3_metrics_2;
import kodkod.multiobjective.concurrency.SolutionNotifier;
import kodkod.multiobjective.concurrency.TranslatingBlockingQueueSolutionNotifier;
import kodkod.multiobjective.*;

import org.junit.BeforeClass;
import org.junit.Before;
import org.junit.Test;

public class TestMagnifier {
	/* Fields */
	static MooProblem problem;
	Formula improvementConstraints;
	GuidedImprovementAlgorithm gia;

	@BeforeClass
	public static void SetUpProblem(){
		problem = new rooks_3_metrics_2();
	}
	
	@Before
	public void SetUpParameters(){
		BlockingQueue<Solution> solutionQueue = new LinkedBlockingQueue<Solution>();
		SolutionNotifier solutionNotifier = new TranslatingBlockingQueueSolutionNotifier(solutionQueue);

		
		MultiObjectiveSolver solver = new MultiObjectiveSolver();
		gia = new GuidedImprovementAlgorithm("asdf", new MultiObjectiveOptions());
		gia.setBitWidth(32);
		gia.getOptions().setSolver(SATFactory.DefaultSAT4J);
		gia.getOptions().setSymmetryBreaking(1000);
		gia.getOptions().setBitwidth(problem.getBitWidth());
		
		
		Solution s = gia.solveFirst(problem.getProblem().getConstraints(), problem.getBounds(), problem.getProblem(), null);
		
			MetricPoint currentValues = null; // is re-assigned around the inner loop
			Solution sprev = null;
			
		while(gia.isSat(s)){
			currentValues = MetricPoint.measure(s, problem.getObjectives(), gia.getOptions());
			final Formula improvementConstraints = currentValues.parametrizedImprovementConstraints();

			s =  gia.solveOne(problem.getProblem().getConstraints().and(improvementConstraints), problem.getBounds(),  problem.getProblem(), improvementConstraints);
		}
		
		final Collection<Formula> assignmentsConstraints = currentValues.assignmentConstraints();
		assignmentsConstraints.add(problem.getProblem().getConstraints());
		gia.magnifier(Formula.and(assignmentsConstraints), problem.getBounds(), currentValues, solutionNotifier);
		
	}
	
	@Test
	public void testMagnifier() {
		assertNotNull("Object:"+improvementConstraints+" is null", improvementConstraints);
	}
}