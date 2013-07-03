package kodkod.multiobjective.algorithms;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.*;
import junit.framework.Assert;
import kodkod.ast.Formula;
import kodkod.engine.Solution;
import kodkod.engine.satlab.SATFactory;
import kodkod.multiobjective.Testmodels.MooProblem;
import kodkod.multiobjective.Testmodels.rooks_3_metrics_2;
import kodkod.multiobjective.*;

import org.junit.BeforeClass;
import org.junit.Before;
import org.junit.Test;

public class TestImprovementConstraint {
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
		gia = new GuidedImprovementAlgorithm("asdf", new MultiObjectiveOptions());
		gia.setBitWidth(32);
		gia.getOptions().setSolver(SATFactory.DefaultSAT4J);
		gia.getOptions().setSymmetryBreaking(1000);
		gia.getOptions().setBitwidth(problem.getBitWidth());
		
		Solution s = gia.solveFirst(problem.getProblem().getConstraints(), problem.getBounds(), problem.getProblem(), null);
		MetricPoint currentValues = MetricPoint.measure(s, problem.getObjectives(), gia.getOptions());
		improvementConstraints = currentValues.parametrizedImprovementConstraints();
		System.out.println(improvementConstraints);
	}
	
	@Test
	public void testImprovementConstraintsIsNull() {
		assertNotNull("Object:"+improvementConstraints+" is null", improvementConstraints);
	}
	
	@Test
	public void testImprovementConstraintsHasImproved(){
		Solution s1 =  gia.solveOne(problem.getProblem().getConstraints().and(improvementConstraints), problem.getBounds(),  problem.getProblem(), improvementConstraints);
		MetricPoint mp = MetricPoint.measure(s1, problem.getObjectives(), gia.getOptions());
		
		assertThat(mp.getValue((Objective)problem.getObjectives().toArray()[0]), is(6));
		assertThat(mp.getValue((Objective)problem.getObjectives().toArray()[1]), is(7));
	}
}
