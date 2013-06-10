package kodkod.multiobjective;

import kodkod.engine.Solution;

public final class MeasuredSolution {

	private final Solution solution;
	private final MetricPoint values;
	
	public MeasuredSolution(final Solution solution, final MetricPoint values) {
		this.solution = solution;
		this.values = values;
	}
	
	@Override
	public String toString() {
		return "MeasuredSolution [solution=" + solution + ", values=" + values
				+ "]";
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result
				+ ((solution == null) ? 0 : solution.hashCode());
		result = prime * result + ((values == null) ? 0 : values.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		MeasuredSolution other = (MeasuredSolution) obj;
		if (solution == null) {
			if (other.solution != null)
				return false;
		} else if (!solution.equals(other.solution))
			return false;
		if (values == null) {
			if (other.values != null)
				return false;
		} else if (!values.equals(other.values))
			return false;
		return true;
	}

	public Solution getSolution() {
		return solution;
	}

	public MetricPoint getValues() {
		return values;
	}

}
