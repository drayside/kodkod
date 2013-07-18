package kodkod.multiobjective.EndtoEnd;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

@RunWith(Suite.class)
@Suite.SuiteClasses({
  kodkod.multiobjective.EndtoEnd.GIASmallEndToEndTest.class,
  kodkod.multiobjective.EndtoEnd.IGIASmallEndToEndTest.class,
  kodkod.multiobjective.EndtoEnd.PartitionedGIASmallEndToEndTest.class,
  kodkod.multiobjective.EndtoEnd.SolverSmallEndToEndTest.class,
  kodkod.multiobjective.EndtoEnd.OGIASmallEndToEndTest.class
})
public class TestSuite {
}
