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
		super();
		this.bounds = bounds;
		this.bitWidth = bitWidth;
		this.constraints = constraints;
		this.objectives = objectives;
	}
}
