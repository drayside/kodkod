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
package kodkod.ast;

/**
 * Defines a generic interface for a binary operator.
 * @specfield op: (D -> D) ->lone R
 * 
 * @author Emina Torlak
 */
public interface BinaryOperator<D, R> {

	/**
	 * Returns true if this binary operator is commutative.
	 * @return all d1, d2: D | this.op[d1,d2] = this.op[d2,d1]
	 */
	public abstract boolean commutative(); 
	
	/**
	 * Returns true if this binary operator is associative.
	 * @return all d1, d2, d3: D | this.op[d1, this.op[d2, d3]] = this.op[this.op[d1,d2], d3]
	 */
	public abstract boolean associative();
	
	/**
	 * Returns the result of applying this operator to the given left and right arguments.
	 * @throws IllegalArgumentException - this operator is not applicable to the given arguments
	 * @return this.op[left, right]
	 */
	public abstract R apply(D left, D right);
	
//	/**
//	 * Returns true if the given object was created by applying this operator 
//	 * to some elements in D.
//	 * @return some op.o
//	 */
//	public abstract boolean decomposable(Object o);
//	
//	/**
//	 * Returns the operator that was used to construct the given composite.
//	 * @requires decomposable(composite)
//	 * @return operator that was used to construct the given composite.
//	 */
//	public abstract BinaryOperator<D, R, C> op(C composite);
//	
//	/**
//	 * Returns the left child of the given composite.
//	 * @requires decomposable(composite)
//	 * @return (this.op.composite).D
//	 */
//	public abstract D left(C composite);
//	
//	/**
//	 * Returns the right child of the given composite.
//	 * @requires decomposable(composite)
//	 * @return D.(this.op.composite)
//	 */
//	public abstract D right(C composite);
	
}
