/**
 * 
 */
package kodkod.multiobjective.api;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeMap;

import kodkod.ast.Formula;
import kodkod.engine.Evaluator;
import kodkod.engine.Solution;
import kodkod.engine.config.Options;

public final class MetricPoint {
	
	private final SortedMap<Objective,Integer> values;
	
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
	
	
	public static MetricPoint measure(final Solution s, final SortedSet<Objective> objectives, final Options options) {
//		System.out.println("Meausuring solution of type " + s.getClass().toString());
		final SortedMap<Objective,Integer> values = new TreeMap<Objective,Integer>();
//		System.out.println("Creating Evaluator with s.instance " + s.instance() + " with options " +  options);
		final Evaluator ev = new Evaluator(s.instance(), options);
		for (final Objective objective : objectives) {
			final Integer value = ev.evaluate(objective.expr);
			values.put(objective, value);
		}
		return new MetricPoint(Collections.unmodifiableSortedMap(values));
	}
	
	Collection<Formula> assignmentConstraints() {
		final List<Formula> conjuncts = new ArrayList<Formula>(values.size() + 1);
		for (final Map.Entry<Objective, Integer> e : values.entrySet()) {
			final Formula f = e.getKey().assignmentConstraint(e.getValue().intValue());
			conjuncts.add(f);
		}
		return conjuncts;
	}

	Formula improvementConstraints() {
		return this.ParametrizedImprovementConstraints();
	}

	Formula ParametrizedImprovementConstraints() {
		final List<Formula> disjuncts = new ArrayList<Formula>(values.size() + 1);

		// every metric gets its turn to be the improver
		for (final Map.Entry<Objective,Integer> improverEntry : values.entrySet()) {
			System.out.println("Using metric " + improverEntry.getKey() + " as improverEntry");
			final Objective improver = improverEntry.getKey();
			final List<Formula> conjuncts = new ArrayList<Formula>(values.size() + 1);
			
			if ( improver.prefer(improverEntry.getValue().intValue(), improverEntry.getValue().intValue()) == 1){
				conjuncts.add(improver.betterThan(improverEntry.getValue().intValue()));
			} else {
				conjuncts.add(improver.betterThan(improverEntry.getValue().intValue()));
			}

	
			// every other metric needs to at least hold the line
			for (final Map.Entry<Objective, Integer> e : values.entrySet()) {
				final Objective o = e.getKey();
				if (!o.equals(improver)) {
					System.out.println("Adding Improvement Constraints");
					final Formula f = o.betterThanOrEqual(e.getValue().intValue());
					conjuncts.add(f);
				}
			}
			
			disjuncts.add(Formula.and(conjuncts));
		}
		
		System.out.println("Possible Improvements are disjunction of ");
		for (Formula aDisjunction : disjuncts){
			System.out.println("\t" + aDisjunction);
		}		
		return Formula.or(disjuncts);
	}

	
	List<Formula> strictImprovementConstraints() {
		// every metric needs to improve
		final List<Formula> conjuncts = new ArrayList<Formula>(values.size() + 1);
		for (final Map.Entry<Objective, Integer> e : values.entrySet()) {
			final Objective o = e.getKey();
			final Formula f = o.betterThan(e.getValue().intValue());
			conjuncts.add(f);
		}

		return conjuncts;
	}		

	
	Formula exclusionConstraint() {
		final List<Formula> disjuncts = new ArrayList<Formula>(values.size() + 1);
		for (final Map.Entry<Objective, Integer> e : values.entrySet()) {
			final Formula f = e.getKey().betterThan(e.getValue().intValue());
			disjuncts.add(f);
		}
		return Formula.or(disjuncts);
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

	public Integer getValue(final Objective o) {
		return values.get(o);
	}

}