/**
 * 
 */
package kodkod.multiobjective.concurrency;

import kodkod.engine.Solution;
import kodkod.multiobjective.MeasuredSolution;
import kodkod.multiobjective.MetricPoint;

public interface SolutionNotifier {
	public void tell(final MeasuredSolution s);
	public void tell(final Solution s, final MetricPoint values);
	public void done();
}