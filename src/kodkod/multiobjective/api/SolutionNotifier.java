/**
 * 
 */
package kodkod.multiobjective.api;

import kodkod.engine.Solution;

public interface SolutionNotifier {
	public void tell(final MeasuredSolution s);
	public void tell(final Solution s, final MetricPoint values);
	public void done();
}