package kodkod.multiobjective.api;

import java.util.EnumMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.concurrent.atomic.AtomicLong;

import kodkod.ast.Formula;
import kodkod.instance.Bounds;

/**
 * Stats for solver.  Thread safe.
 */
public final class Stats {
	private final String solverClassName;
	private final String desc;
	
	/**
	 * The map itself is read only:  we mutate the values.
	 * Since we do not mutate the map, it is thread safe.
	 * Thread safety for the values comes from AtomicLong.
	 */
	private final Map<StatKey,AtomicLong> data;
	private final LinkedList<IndividualStats> singleCallData;
	
	
	/* LinkedList of a summary of each iteration. */

	public Stats(final String solverClassName, final String desc) {
		this.solverClassName = solverClassName;
		this.desc = desc;
		
		//initialize Vector about stats of each call to Kodkod
		singleCallData = new LinkedList<IndividualStats>();		
		// initialize the map
		data = new EnumMap<StatKey, AtomicLong>(StatKey.class);
		for (final StatKey key : StatKey.values()) {
			data.put(key, new AtomicLong(0));
		}
		// now the map is read only
		// we avoid the UnmodifiableMap wrapper for performance reasons
	}
	
	public boolean isValidFinalState() {
		final int optimalSolnCount = data.get(StatKey.OPTIMAL_SOLNS).intValue();
		final int metricPoints = data.get(StatKey.OPTIMAL_METRIC_POINTS).intValue();
		final int satCalls = data.get(StatKey.REGULAR_UNSAT_CALL).intValue();
		final int unsatCalls = data.get(StatKey.REGULAR_SAT_CALL).intValue();

		assert optimalSolnCount >= metricPoints : optimalSolnCount + " <?> " + metricPoints;
		assert optimalSolnCount <= unsatCalls;
		assert optimalSolnCount <= satCalls;
//		assert satCalls >= unsatCalls : satCalls + " <?> " + unsatCalls;
		return true;
	}
	
	void increment(final StatKey key) {
		System.out.println("Incrementing " + key.name());
		increment(key, 1);
	}

	void increment(final StatKey key, final long increment) {
		final AtomicLong value = data.get(key);
		value.addAndGet(increment);
	}
	
	void addSummaryIndividualCall(StatKey satCallType, long TranslationTime, long SolvingTime, final Formula f, final Bounds b, final boolean first, MetricPoint ObjectiveValueReceived, final Formula ImprovementConstraints){
		this.singleCallData.addLast(new IndividualStats(satCallType, TranslationTime, SolvingTime, ObjectiveValueReceived, ImprovementConstraints));
	}
	
	public LinkedList<IndividualStats> getIndividualStats(){
		return this.singleCallData;
	}
	void set(final StatKey key, final long val) {
		final AtomicLong value = data.get(key);
		value.set(val);
	}
	
	void begin() {
		final AtomicLong value = data.get(StatKey.BEGIN_TIME);
		value.set(System.currentTimeMillis());
	}

	void end() {
		final AtomicLong endTime = data.get(StatKey.END_TIME);
		endTime.set(System.currentTimeMillis());
		
		final AtomicLong beginTime = data.get(StatKey.BEGIN_TIME);

		final AtomicLong duration = data.get(StatKey.DURATION);
		duration.set(endTime.longValue() - beginTime.longValue());
	}
	
	public long get(final StatKey key) {
		final AtomicLong value = data.get(key);
		return value.longValue();
	}

	public long getTotalCalls() {
		long result = 0;
		result += get(StatKey.REGULAR_SAT_CALL);
		result += get(StatKey.REGULAR_UNSAT_CALL);
		result += get(StatKey.MAGNIFIER_SAT_CALL);
		result += get(StatKey.MAGNIFIER_UNSAT_CALL);
		return result;
	}

	public String getSolverClassName() {
		return solverClassName;
	}

	public String getDesc() {
		return desc;
	}
}