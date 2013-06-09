package kodkod.multiobjective;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

@RunWith(Suite.class)
@Suite.SuiteClasses({
  kodkod.multiobjective.GIASmallEndToEndTest.class,
  kodkod.multiobjective.SolverSmallEndToEndTest.class
})
public class TestSuite {
}
