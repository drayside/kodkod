package kodkod.engine;

/**
 * Thrown when a computation is terminated due to running out of time.
 * 
 * @author Emina Torlak
 */
public final class TimeoutException extends Exception {
	private static final long serialVersionUID = 7281832322091650534L;

	public TimeoutException() {
		super();
		// TODO Auto-generated constructor stub
	}

	public TimeoutException(String arg0) {
		super(arg0);
		// TODO Auto-generated constructor stub
	}

	public TimeoutException(String arg0, Throwable arg1) {
		super(arg0, arg1);
		// TODO Auto-generated constructor stub
	}

	public TimeoutException(Throwable arg0) {
		super(arg0);
		// TODO Auto-generated constructor stub
	}

}
