package tests;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite("Test for tests");
		//$JUnit-BEGIN$
		suite.addTestSuite(BooleanMatrixTest.class);
		suite.addTestSuite(TranslatorTest.class);
		suite.addTestSuite(EvaluatorTest.class);
		suite.addTestSuite(BooleanCircuitTest.class);
		suite.addTestSuite(SymmetryBreakingTest.class);
		suite.addTestSuite(SkolemizationTest.class);
		suite.addTestSuite(ModelsTest.class);
		suite.addTestSuite(BugTests.class);
		suite.addTestSuite(ReductionAndProofTest.class);
		//$JUnit-END$
		return suite;
	}

}
