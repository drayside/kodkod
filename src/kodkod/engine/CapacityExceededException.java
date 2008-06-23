/* 
 * Kodkod -- Copyright (c) 2005-2008, Emina Torlak
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package kodkod.engine;

/**
 * Indicates that a problem construction or translation task failed because
 * the capacity of the index representation was exceeded.  For
 * @author Emina Torlak
 */
public final class CapacityExceededException extends RuntimeException {

	private static final long serialVersionUID = -8098615204149641969L;

	/**
	 * Constructs a CapacityExceededException.
	 */
	public CapacityExceededException() {}

	/**
	 * Constructs a CapacityExceededException with the given message.
	 */
	public CapacityExceededException(String arg0) {
		super(arg0);
	}

	/**
	 * Constructs a CapacityExceededException with the given cause.
	 */
	public CapacityExceededException(Throwable arg0) {
		super(arg0);
	}

	/**
	 * Constructs a CapacityExceededException with the given message and cause.
	 */
	public CapacityExceededException(String arg0, Throwable arg1) {
		super(arg0, arg1);
	}

}
