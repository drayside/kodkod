package kodkod.multiobjective.concurrency;

import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.concurrent.BlockingQueue;
import kodkod.engine.Solution;

/**
 * This class implements an iterator over Kodkod solutions.
 */
public class BlockingSolutionIterator implements Iterator<Solution> {

	private final BlockingQueue<Solution> queue;
	private Solution solution = null;
	
	// If we encounter a poison pill we set it to true. Nothing sets to false afterwards.
	private boolean noMoreSolutions = false;

	public BlockingSolutionIterator(final BlockingQueue<Solution> queue) {
		super();
		this.queue = queue;
	}

	/**
	 * hasNext method for SolutionIterator; behaves just like an ordinary iterator for a container.
	 */
	@Override
	public boolean hasNext() {
        Object probe = null;
        
        if (noMoreSolutions){
        	return false;
        } else if ( solution != null ) {
        	// Solution is assigned iff we have a real solution.
        	// We only hit this case if we call hasNext() repeatedly without actually calling next().
        	return true;
        }
        
		try {
			// queue.take() is a blocking call.
			probe = queue.take();
		} catch (InterruptedException e) {
			// We don't handle InterruptedException
			throw new RuntimeException("Unexpected thread interruption.");
		}
       
       if (probe == null) {
    	   throw new NullPointerException();
       } else if (Poison.isPoisonPill(probe)) {
    	   // We are completely done. There's no more solutions available, and it can never be reset to false.
    	   noMoreSolutions = true;
    	   return false;
       } else if (probe instanceof Throwable) {
         throw new RuntimeException((Throwable)(probe));
       } else if (!(probe instanceof Solution )) {
    	   // We don't know what this is.
    	   throw new RuntimeException("Expected Solution. Got " + probe.getClass().toString() + " instead.");
       } else {
    	   // We got a solution, so we are caching it in solution.
    	   solution = (Solution)probe;
    	   return true;
       }
	}

	@Override
	public Solution next() {
		if (noMoreSolutions){
			throw new NoSuchElementException();
		} else if( solution != null ) {
			// We have a cached solution already because hasNext() was previously called.
        	// We set it back to null since we are returning the cached solution.
			final Solution toReturn = solution;
			solution = null;
			return toReturn;
		}
		
		// hasNext() hasn't been called, so we try to take one from the queue.
		try {
			final Object probe = queue.take();
			if ( probe == null ) {
				throw new NullPointerException();
			} else if( Poison.isPoisonPill(probe) ){
				// We are completely done. There's no more solutions available, and it can never be reset to false.
				noMoreSolutions = true;
				throw new NoSuchElementException();
      } else if (probe instanceof Throwable) {
        throw new RuntimeException((Throwable)probe);
			} else if (!(probe instanceof Solution )) {
				// We don't know what this is.
	    	   throw new RuntimeException("Expected Solution. Got " + probe.getClass().toString() + " instead.");
		    } else {
		    	// We got a solution, so we return it.
		    	return (Solution)probe;
		    }
		} catch (InterruptedException e) {
			throw new RuntimeException("Unexpected thread interruption.");
		}
	}

	@Override
	public void remove() {
		throw new UnsupportedOperationException();
	}
}
