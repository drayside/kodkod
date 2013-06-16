/**
 * 
 */
package kodkod.multiobjective.concurrency;

import java.util.concurrent.BlockingQueue;

import kodkod.engine.Solution;
import kodkod.multiobjective.MeasuredSolution;
import kodkod.multiobjective.MetricPoint;

public final class TranslatingBlockingQueueSolutionNotifier implements SolutionNotifier {
	
	private final BlockingQueue<Solution> queue;
	
	public TranslatingBlockingQueueSolutionNotifier(final BlockingQueue<Solution> queue) {
		this.queue = queue;
	}
	
	@Override
	public void tell(final MeasuredSolution s) {
		try {
			queue.put(s.getSolution());
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
		Poison.poison(queue);
	}
}