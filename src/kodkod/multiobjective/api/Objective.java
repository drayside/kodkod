/**
 * 
 */
package kodkod.multiobjective.api;

import kodkod.ast.Formula;
import kodkod.ast.IntConstant;
import kodkod.ast.IntExpression;

public abstract class Objective implements Comparable<Objective> {

	public final String desc;
	
	public final IntExpression expr;
	
	private Objective(final String desc, final IntExpression expr) {
		this.desc = desc;
		this.expr = expr;
	}
	
	@Override
	public int compareTo(final Objective other) {
		return this.desc.compareTo(other.desc);
	}
	
	public final Formula assignmentConstraint(final int value) {
		return expr.eq(IntConstant.constant(value));
	}
	
	public abstract Formula betterThan(final int value);

	public abstract Formula betterThanOrEqual(final int value);

	public abstract Formula worseThan(final int value);
	
	/**
	 * Indicates which, if either, value is preferred by this metric.
	 * @param d1
	 * @param d2
	 * @return 0 if neither is preferred; -1 if d1 is preferred; +1 if d2 is preferred.
	 */
	public abstract int prefer(final int v1, final int v2);
	
	public static Objective newMinObjective(final String desc, final IntExpression expr) {
		return new MinObjective(desc, expr);
	}

	public static Objective newMaxObjective(final String desc, final IntExpression expr) {
		return new MaxObjective(desc, expr);
	}

	
	private static final class MinObjective extends Objective {
		private MinObjective(final String desc, final IntExpression expr) {
			super(desc, expr);
		}
	
		@Override
		public Formula betterThan(final int value) {
			return expr.lt(IntConstant.constant(value));
		}

		@Override
		public Formula betterThanOrEqual(int value) {
			return expr.lte(IntConstant.constant(value));
		}

		@Override
		public Formula worseThan(int value) {
			return expr.gt(IntConstant.constant(value));
		}

		@Override
		public int prefer(final int v1, final int v2) {
			if (v1 == v2) {
				return 0;
			} else if (v1 < v2) {
				return -1; // prefer v1
			} else {
				return +1; // prefer v2
			}
		}

	}

	
	private static final class MaxObjective extends Objective {
		private MaxObjective(final String desc, final IntExpression expr) {
			super(desc, expr);
		}
	
		@Override
		public Formula betterThan(final int value) {
			return expr.gt(IntConstant.constant(value));
		}

		@Override
		public Formula betterThanOrEqual(int value) {
			return expr.gte(IntConstant.constant(value));
		}

		@Override
		public Formula worseThan(final int value) {
			return expr.lt(IntConstant.constant(value));
		}

		@Override
		public int prefer(final int v1, final int v2) {
			if (v1 == v2) {
				return 0;
			} else if (v1 > v2) {
				return -1; // prefer v1
			} else {
				return +1; // prefer v2
			}
		}

	}


}