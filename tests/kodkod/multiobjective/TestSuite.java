package kodkod.multiobjective;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

@RunWith(Suite.class)
@Suite.SuiteClasses({
  kodkod.multiobjective.EndtoEnd.TestSuite.class,
  kodkod.multiobjective.algorithms.TestSuite.class
})
public class TestSuite {
}
