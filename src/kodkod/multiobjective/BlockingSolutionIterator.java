package kodkod.multiobjective;

import java.util.Iterator;
import java.util.concurrent.BlockingQueue;
import kodkod.engine.Solution;

/**
 * This class implements an iterator over Kodkod solutions.
 */
public class BlockingSolutionIterator implements Iterator<Solution> {

	private final BlockingQueue<Solution> queue;
	private Solution solution = null;

	public BlockingSolutionIterator(final BlockingQueue<Solution> queue) {
		super();
		this.queue = queue;
	}

	@Override
	public boolean hasNext() {
               final Object probe = queue.peek();
               if (probe == null || Poison.isPoisonPill(probe) || !(probe instanceof Solution)) {
                       return false;
               } else {
                       solution = (Solution) probe;
                       return true;
               }
	}

	@Override
	public Solution next() {
		try {
			solution = queue.take();
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
		final Solution n = solution;
		// Fixed the TODO, by moving "final Solution n = s;" to after the take.:
		// TODO what to do when there are no more pareto points ?
		return n;
	}

	@Override
	public void remove() {
		throw new UnsupportedOperationException();
	}
}