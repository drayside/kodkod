/**
 * 
 */
package kodkod.multiobjective.api;

import java.util.concurrent.BlockingQueue;

import kodkod.engine.Solution;

public final class BlockingQueueSolutionNotifier implements SolutionNotifier {
	
	private final BlockingQueue<MeasuredSolution> q;
	
	public BlockingQueueSolutionNotifier(final BlockingQueue<MeasuredSolution> q) {
		this.q = q;
	}
	
	@Override
	public void tell(final MeasuredSolution s) {
		try {
			q.put(s);
		} catch (final Exception e) {
			throw new RuntimeException(e);
		}
	}

	@Override
	public void tell(Solution s, MetricPoint values) {
		tell(new MeasuredSolution(s, values));
	}

	@Override
	public void done() {
		Poison.poison(q);
	}

	
}