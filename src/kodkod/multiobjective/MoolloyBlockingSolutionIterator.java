package kodkod.multiobjective;

import java.util.Iterator;
import java.util.concurrent.BlockingQueue;
import kodkod.engine.Solution;

/**
 * This class implements an iterator over Kodkod solutions.
 */
public class MoolloyBlockingSolutionIterator implements Iterator<Solution> {

	private final BlockingQueue<Solution> q;
	private Solution s = null;

	public MoolloyBlockingSolutionIterator(final BlockingQueue<Solution> q) {
		super();
		this.q = q;
	}

	@Override
	public boolean hasNext() {
               final Object probe = q.peek();
               if (probe == null || Poison.isPoisonPill(probe) || !(probe instanceof Solution)) {
                       return false;
               } else {
                       s = (Solution) probe;
                       return true;
               }
	}

	@Override
	public Solution next() {
		/*
		System.out.println("Getting Next from SolutionIterator");
		System.out.println("Will List Solutions in Queue q");
		for (Object tObj : q.toArray()){
			if (!( tObj instanceof Poison)){
				Solution sObj = (Solution) tObj;
				System.out.println("Solution -- \n" + sObj);				
			}
		}
		System.out.println("End Listing Solutions in Queue q");		
		*/
		
		try {
			s = q.take();
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
		final Solution n = s;
		// Fixed the TODO, by moving "final Solution n = s;" to after the take.:
		// TODO what to do when there are no more pareto points ?
		return n;
	}

	@Override
	public void remove() {
		throw new UnsupportedOperationException();
	}

}