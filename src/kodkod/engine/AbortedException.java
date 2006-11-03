/**
 * 
 */
package kodkod.engine;

/**
 * Indicates that a solving or evaluation task has been aborted
 * by calling Thread.interrupt on the solving (evaluation) thread.
 * @author Emina Torlak
 */
public final class AbortedException extends RuntimeException {

	private static final long serialVersionUID = 201522560152091247L;

	/**
	 * Constructs an aborted exception with no message.
	 */
	AbortedException() {}

	/**
	 * Constructs an aborted exception with the given message.
	 */
	AbortedException(String message) {
		super(message);
	}

	/**
	 * Constructs an aborted exception with the given cause.
	 */
	AbortedException(Throwable cause) {
		super(cause); 
	}

	/**
	 * Constructs an aborted exception with the given message and cause.
	 */
	AbortedException(String message, Throwable cause) {
		super(message, cause);
	}

}
