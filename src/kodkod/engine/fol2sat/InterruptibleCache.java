/**
 * 
 */
package kodkod.engine.fol2sat;

import kodkod.ast.Node;
import kodkod.engine.bool.BooleanMatrix;


/**
 * A TranslationCache that checks for interrupts on each call to 
 * {@link #get(Node, Environment)}.
 * @author Emina Torlak
 */
final class InterruptibleCache extends TranslationCache {

	/**
	 * Constructs a new interruptible cache.
	 */
	InterruptibleCache(AnnotatedNode<? extends Node> annotated) {
		super(annotated);
	}
	
	/**
	 * If the current thread has been interrupted, throws a {@link TranslationAbortedException}.
	 * If the thread is running and translation of the given node, with its free variables
	 * bound as they are in the given environment, has been cached, 
	 * the cached value is returned.  Otherwise, null is returned.
	 * @return this.cache[node][Object] in env.map =>
	 *         this.cache[node].map, null
	 * @throws TranslationAbortedException - Thread.currentTread.isInterrupted()
	 */
	<T> T get(Node node, Environment<BooleanMatrix> env) {
		if (Thread.currentThread().isInterrupted()) 
			throw new TranslationAbortedException();
		return super.get(node, env);
	}

}
