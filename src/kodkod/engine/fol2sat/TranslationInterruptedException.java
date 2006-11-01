/**
 * 
 */
package kodkod.engine.fol2sat;

/**
 * A runtime exception thrown when translation is interrupted by
 * calling Thread.interrupt().
 * @author Emina Torlak
 */
public final class TranslationInterruptedException extends RuntimeException {
	private static final long serialVersionUID = -7923938055171364876L;

	/**
	 * Constructs a new exception with the given message.
	 */
	TranslationInterruptedException(String msg) {
		super(msg);
	}
	
	/**
	 * Constructs a new exception with no message.
	 */
	TranslationInterruptedException() {}
	
}
