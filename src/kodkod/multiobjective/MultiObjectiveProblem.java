package kodkod.multiobjective;

import java.util.SortedSet;

import kodkod.ast.Formula;
import kodkod.instance.Bounds;

public final class MultiObjectiveProblem {

	private final Formula constraints;
	private final Bounds bounds;
	private final int bitWidth;
	private final SortedSet<Objective> objectives;

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
		return "MultiObjectiveProblem [constraints=" + getConstraints()
				+ ", bounds=" + getBounds() + ", bitWidth=" + getBitWidth()
				+ ", objectives=" + getObjectives() + "]";
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + getBitWidth();
		result = prime * result + ((getBounds() == null) ? 0 : getBounds().hashCode());
		result = prime * result
				+ ((getConstraints() == null) ? 0 : getConstraints().hashCode());
		result = prime * result
				+ ((getObjectives() == null) ? 0 : getObjectives().hashCode());
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
		if (getBitWidth() != other.getBitWidth())
			return false;
		if (getBounds() == null) {
			if (other.getBounds() != null)
				return false;
		} else if (!getBounds().equals(other.getBounds()))
			return false;
		if (getConstraints() == null) {
			if (other.getConstraints() != null)
				return false;
		} else if (!getConstraints().equals(other.getConstraints()))
			return false;
		if (getObjectives() == null) {
			if (other.getObjectives() != null)
				return false;
		} else if (!getObjectives().equals(other.getObjectives()))
			return false;
		return true;
	}

	public int getBitWidth() {
		return bitWidth;
	}

	public Formula getConstraints() {
		return constraints;
	}

	public Bounds getBounds() {
		return bounds;
	}

	public SortedSet<Objective> getObjectives() {
		return objectives;
	}
}
