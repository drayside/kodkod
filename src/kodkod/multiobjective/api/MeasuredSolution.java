package kodkod.multiobjective.api;

import kodkod.engine.Solution;

public final class MeasuredSolution {

	public final Solution solution;
	public final MetricPoint values;
	
	public MeasuredSolution(final Solution solution, final MetricPoint values) {
		super();
		this.solution = solution;
		this.values = values;
	}

	public boolean dominates(final MeasuredSolution that) {
		return this.values.dominates(that.values);
	}
	
}
