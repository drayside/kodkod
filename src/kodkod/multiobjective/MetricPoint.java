/**
 * 
 */
package kodkod.multiobjective;

import java.util.ArrayList;
import java.util.BitSet;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.logging.Level;
import java.util.logging.Logger;

import kodkod.ast.Formula;
import kodkod.engine.Evaluator;
import kodkod.engine.Solution;
import kodkod.engine.config.Options;

public final class MetricPoint {

	private final SortedMap<Objective,Integer> values;
	
	private final static Logger logger = Logger.getLogger(MetricPoint.class.toString());

	private MetricPoint(final SortedMap<Objective,Integer> values) {
		this.values = values;
	}
	
	public List<Integer> values() {
		final List<Integer> list = new ArrayList<Integer>(values.size() + 1);
		for (final Integer v : values.values()) {
			list.add(v);
		}
		return Collections.unmodifiableList(list);
	}

	public Integer getValue(final Objective objective) {
		return values.get(objective);
	}

	public static MetricPoint measure(final Solution s, final SortedSet<Objective> objectives, final Options options) {
		final SortedMap<Objective,Integer> values = new TreeMap<Objective,Integer>();
		final Evaluator ev = new Evaluator(s.instance(), options);
		for (final Objective objective : objectives) {
			final Integer value = ev.evaluate(objective.expr);
			values.put(objective, value);
		}
		return new MetricPoint(Collections.unmodifiableSortedMap(values));
	}
	
	public Collection<Formula> assignmentConstraints() {
		final List<Formula> conjuncts = new ArrayList<Formula>(values.size() + 1);
		for (final Map.Entry<Objective, Integer> e : values.entrySet()) {
			final Formula f = e.getKey().assignmentConstraint(e.getValue().intValue());
			conjuncts.add(f);
		}
		return conjuncts;
	}

	public Formula parametrizedImprovementConstraints() {
		final List<Formula> conjuncts = new ArrayList<Formula>(values.size() + 1);

		// All of the metrics must be at least as good.
		for (final Map.Entry<Objective,Integer> metricEntry : values.entrySet()) {
			final Objective metric = metricEntry.getKey();
			conjuncts.add(metric.betterThanOrEqual(metricEntry.getValue().intValue()));
		}

		final List<Formula> improvement_disjuncts = new ArrayList<Formula>(values.size() + 1);

		// At least one of the metrics must improve.
		for (final Map.Entry<Objective,Integer> metricEntry : values.entrySet()) {
			final Objective metric = metricEntry.getKey();
			improvement_disjuncts.add(metric.betterThan(metricEntry.getValue().intValue()));
		}
		conjuncts.add(Formula.or(improvement_disjuncts));

		StringBuilder sb = new StringBuilder("Possible Improvements are conjunction of ");
		for (Formula aConjunction : conjuncts) {
			sb.append("\n\t");
			sb.append(aConjunction);
		}
		logger.log(Level.FINE, sb.toString());

		return Formula.and(conjuncts);
	}

	public Formula objectiveImprovementConstraint(Objective objective) {
		if (!values.containsKey(objective)) {
			throw new RuntimeException();
		}
		int value = values.get(objective).intValue();
		Formula constraint = objective.betterThan(value);

		logger.log(Level.FINE, "Improving on {0}", constraint.toString());

		return constraint;
	}

	public Formula boundaryConstraint(Objective objective) {
		if (!values.containsKey(objective)) {
			throw new RuntimeException();
		}
		int value = values.get(objective).intValue();

		return objective.worseThanOrEqual(value);
	}

	public Formula exclusionConstraint() {
		final List<Formula> disjuncts = new ArrayList<Formula>(values.size() + 1);
		for (final Map.Entry<Objective, Integer> e : values.entrySet()) {
			final Formula f = e.getKey().betterThan(e.getValue().intValue());
			disjuncts.add(f);
		}
		return Formula.or(disjuncts);
	}

	// The BitSet is a mapping of bits to constraints
	// Let bit_i represent obj_i
	// Then if bit_i is 0, we have the constraint "metric_i <  val_i"
	//  and if bit_i is 1, we have the constraint "metric_i >= val_i"
	// We bias the constraints so we don't include the boundary in multiple partitions
	public Formula partitionConstraints(BitSet set) {
		final List<Formula> conjuncts = new ArrayList<Formula>(values.size());
		Set<Objective> objectives = values.keySet();
		int bitIndex = 0;

		// Since objectives is a SortedMap, iterating over the keys is deterministic
		for (final Objective objective : objectives) {
			int value = values.get(objective).intValue();
			if (set.get(bitIndex)) {
				conjuncts.add(objective.betterThanOrEqual(value));
			} else {
				conjuncts.add(objective.worseThan(value));
			}
			bitIndex++;
		}
		
		StringBuilder sb = new StringBuilder("Partition constraints are conjunction of ");
		for (Formula aConjunction : conjuncts) {
			sb.append("\n\t");
			sb.append(aConjunction);
		}
		logger.log(Level.FINE, sb.toString());

		return Formula.and(conjuncts);
	}

	public boolean dominates(final MetricPoint that) {
		if (this.values.size() != that.values.size()) {
			throw new IllegalArgumentException("sizes do not match");
		}
		
		int equalsCount = 0;
		
		for (final Map.Entry<Objective,Integer> e : values.entrySet()) {
			final Objective objective = e.getKey();
			final int thisValue = e.getValue().intValue();
			final int thatValue = that.values.get(objective).intValue();
			final int preference = objective.prefer(thisValue, thatValue);
			if (preference > 0) {
				// thatValue is preferred: thisValue does not dominate
				return false;
			}
			if (preference == 0) {
				// neither is preferred
				equalsCount++;
			}
		}
		
		if (equalsCount == values.size()) {
			// equal on all metrics
			return false;
		} else {
			// some are equal, none are worse, and
			// this is better than that on at least one
			return true;
		}
	}

	@Override
	public String toString() {
		return "MetricPoint [values=" + values + "]";
	}

}
