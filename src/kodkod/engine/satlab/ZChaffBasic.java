package kodkod.engine.satlab;

/**
 * Wrapper for an instance of zchaff that provides
 * access to basic functionality.
 */
final class ZChaffBasic extends ZChaff {
	ZChaffBasic() {
		super(make());
	}
	
	static {
	    System.loadLibrary("zchaff_basic");
	}
	
	/**
	 * Creates an instance of zchaff and returns 
	 * its address in memory.  
	 * @return the memory address of an instance
	 * of the zchaff solver 
	 */
	private static native long make();
	
	public String toString() {
		return "ZChaffBasic";
	}
}