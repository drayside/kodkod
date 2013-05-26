package kodkod.multiobjective.api;

import java.util.concurrent.BlockingQueue;

public final class Poison 
{
	/** Prevent external instantiation. */
	private Poison () {}
	
	public final static Poison PILL = new Poison();

	@SuppressWarnings({ "unchecked", "rawtypes" })
	public static void poison(BlockingQueue<? extends Object> q) {
		try {
			final BlockingQueue q2 = q;
			q2.put( (Object)Poison.PILL );
		} catch (final InterruptedException e) {
			e.printStackTrace();
			throw new RuntimeException(e);
		}
	}
	
	public static boolean isPoisonPill(final Object obj) {
		return (obj == PILL);
	}
	
}
