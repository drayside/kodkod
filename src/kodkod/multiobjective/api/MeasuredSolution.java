package kodkod.multiobjective.api;

import kodkod.engine.Solution;

public final class MeasuredSolution {

	private final Solution solution;
	private final MetricPoint values;
	
	public MeasuredSolution(final Solution solution, final MetricPoint values) {
		this.solution = solution;
		this.values = values;
	}
	
	public Solution getSolution() {
		return solution;
	}

	public MetricPoint getValues() {
		return values;
	}

}
