package kodkod;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

@RunWith(Suite.class)
@Suite.SuiteClasses({
  kodkod.multiobjective.TestSuite.class,
  kodkod.engine.TestSuite.class
})
public class TestSuite {
}
