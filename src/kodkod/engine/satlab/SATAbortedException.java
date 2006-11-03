/**
 * 
 */
package kodkod.engine.satlab;

/**
* A runtime exception thrown when a sat solver
* terminates abnormally, usually because it was cancelled.
* @author Emina Torlak
*/
public final class SATAbortedException extends RuntimeException {

	private static final long serialVersionUID = 5162235133382021308L;

	/**
	 * Constructs an aborted exception with no message.
	 */
	SATAbortedException() {}

	/**
	 * Constructs an aborted exception with the given message.
	 */
	SATAbortedException(String message) {
		super(message);
	}

	/**
	 * Constructs an aborted exception with the given cause.
	 */
	SATAbortedException(Throwable cause) {
		super(cause);
	}

	/**
	 * Constructs an aborted exception with the given message and cause.
	 */
	SATAbortedException(String message, Throwable cause) {
		super(message, cause);
		// TODO Auto-generated constructor stub
	}

}
