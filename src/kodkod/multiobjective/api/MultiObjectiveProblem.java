package kodkod.multiobjective.api;

import java.util.SortedSet;

import kodkod.ast.Formula;
import kodkod.instance.Bounds;

public final class MultiObjectiveProblem {

	final Formula constraints;
	final Bounds bounds;
	final int bitWidth;
	final SortedSet<Objective> objectives;

	public MultiObjectiveProblem(final Bounds bounds, final Formula constraints, final SortedSet<Objective> objectives) {
		this(bounds, 32, constraints, objectives);
	}
	
	public MultiObjectiveProblem(final Bounds bounds, final int bitWidth, final Formula constraints, final SortedSet<Objective> objectives) {
		this.bounds = bounds;
		this.bitWidth = bitWidth;
		this.constraints = constraints;
		this.objectives = objectives;
	}

	@Override
	public String toString() {
		return "MultiObjectiveProblem [constraints=" + constraints
				+ ", bounds=" + bounds + ", bitWidth=" + bitWidth
				+ ", objectives=" + objectives + "]";
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + bitWidth;
		result = prime * result + ((bounds == null) ? 0 : bounds.hashCode());
		result = prime * result
				+ ((constraints == null) ? 0 : constraints.hashCode());
		result = prime * result
				+ ((objectives == null) ? 0 : objectives.hashCode());
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
		MultiObjectiveProblem other = (MultiObjectiveProblem) obj;
		if (bitWidth != other.bitWidth)
			return false;
		if (bounds == null) {
			if (other.bounds != null)
				return false;
		} else if (!bounds.equals(other.bounds))
			return false;
		if (constraints == null) {
			if (other.constraints != null)
				return false;
		} else if (!constraints.equals(other.constraints))
			return false;
		if (objectives == null) {
			if (other.objectives != null)
				return false;
		} else if (!objectives.equals(other.objectives))
			return false;
		return true;
	}
}
