/**
 * 
 */
package kodkod.engine.fol2sat;

/**
 * A runtime exception thrown when translation is aborted by calling Thread.interrupt().
 * @author Emina Torlak
 */
public final class TranslationAbortedException extends RuntimeException {
	private static final long serialVersionUID = -7923938055171364876L;

	/**
	 * Constructs a new exception with the given message.
	 */
	TranslationAbortedException(String msg) {
		super(msg);
	}
	
	/**
	 * Constructs a new exception with no message.
	 */
	TranslationAbortedException() {}
	
}
