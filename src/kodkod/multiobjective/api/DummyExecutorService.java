/**
 * 
 */
package kodkod.multiobjective.api;

import java.util.Collections;
import java.util.List;
import java.util.concurrent.AbstractExecutorService;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.TimeUnit;

/**
 * An ExecutorService that executes commands in the calling thread
 * (ie, no parallelism whatsoever).
 * 
 * @author drayside
 *
 */
public final class DummyExecutorService extends AbstractExecutorService {

	private boolean isShutdown = false;

	@Override
	public boolean awaitTermination(long timeout, TimeUnit unit)
			throws InterruptedException {
		// FIXME:  this probably isn't a proper implementation of awaitTermination ...
		return isShutdown;
	}

	@Override
	public boolean isShutdown() {
		return isShutdown;
	}

	@Override
	public boolean isTerminated() {
		return isShutdown;
	}

	@Override
	public void shutdown() {
		isShutdown = true;
	}

	@Override
	public List<Runnable> shutdownNow() {
		isShutdown = true;
		return Collections.emptyList();
	}

	/** 
	 * Execute the command in the calling thread.
	 */
	@Override
	public void execute(final Runnable command) {
		if (command == null) {
			throw new NullPointerException("cannot execute null command");
		}
		if (isShutdown) {
			throw new RejectedExecutionException("this executor service is shutdown now.  sorry.");
		}
		// run it.
		command.run();
	}
}