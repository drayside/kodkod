package kodkod.multiobjective.algorithms;

import static org.junit.Assert.*;
import junit.framework.Assert;
import kodkod.engine.Solution;
import kodkod.engine.satlab.SATFactory;
import kodkod.multiobjective.Testmodels.MooProblem;
import kodkod.multiobjective.Testmodels.rooks_3_metrics_2;
import kodkod.multiobjective.*;
import kodkod.multiobjective.algorithms.*;
import org.junit.Before;
import org.junit.Test;

public class GIAUnitTest {
	/* Fields */
	MooProblem problem;

	@Before
	public void SetUp(){
		problem = new rooks_3_metrics_2();
	}
	
	@Test
	public void testSolveFirst() {
		GuidedImprovementAlgorithm gia = new GuidedImprovementAlgorithm("asdf", new MultiObjectiveOptions());
		gia.setBitWidth(32);
		gia.getOptions().setSolver(SATFactory.DefaultSAT4J);
		gia.getOptions().setSymmetryBreaking(1000);
		gia.getOptions().setBitwidth(problem.getBitWidth());
		
		Solution s = gia.solveFirst(problem.getProblem().getConstraints(), problem.getProblem(), null);
		assertTrue(s.sat());
	}

}
