package kodkod.multiobjective.UnitTests;

import static org.junit.Assert.*;
import kodkod.multiobjective.Testmodels.MooProblem;
import kodkod.multiobjective.Testmodels.rooks_3_metrics_2;
import kodkod.multiobjective.api.MultiObjectiveProblem;

import org.junit.Before;
import org.junit.Test;

public class GIAUnitTest {
	/* Fields */
	MooProblem problem;

	@Before
	private void SetUp(){
		problem = new rooks_3_metrics_2();
	}
	
	@Test
	public void test() {
		fail("Not yet implemented");
	}

}
