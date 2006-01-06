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
		suite.addTestSuite(ModelsTest.class);
		suite.addTestSuite(CrashesTest.class);
		//$JUnit-END$
		return suite;
	}

}
