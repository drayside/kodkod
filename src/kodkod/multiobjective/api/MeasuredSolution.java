package kodkod.multiobjective.api;

import kodkod.engine.Solution;

public final class MeasuredSolution {

	public final Solution solution;
	public final MetricPoint values;
	
	public MeasuredSolution(final Solution solution, final MetricPoint values) {
		this.solution = solution;
		this.values = values;
	}
	
}
