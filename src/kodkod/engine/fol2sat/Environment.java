/* 
 * Kodkod -- Copyright (c) 2005-2011, Emina Torlak
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
package kodkod.engine.fol2sat;

import kodkod.ast.Variable;
import kodkod.engine.bool.BooleanAccumulator;
import kodkod.engine.bool.BooleanValue;
import kodkod.engine.bool.Operator.Nary;



/** 
 * Represents a  variable binding environment as a 
 * map from a {@link kodkod.ast.Variable variable} to a 
 * {@link java.lang.Object value}.  An environment has
 * a (possibly empty) parent environment to which unsuccessful lookups 
 * are delegated.
 * 
 * @specfield variable: lone Variable
 * @specfield value: lone T
 * @specfield type: lone E
 * @specfield parent: Environment
 * @invariant this = parent => no variable
 * @author Emina Torlak 
 */
final class Environment<T, E> {
	private final Variable variable;
	private final T value;
	private final E type; 
	private final Environment<T, E> parent;
	/** Portion of the overflow circuit not affected by this.variable */ 
	private final BooleanAccumulator overflow; 
	/** Portion of the overflow circuit affected by this.variable (should be used only if this.type is INT) */
	private final BooleanAccumulator varOverflow;
	
	/**
	 * Constructs the empty environment.
	 */
	private Environment() {
		this.variable = null;
		this.value = null;
		this.type = null;
		this.parent = this;
		this.overflow = BooleanAccumulator.treeGate(Nary.OR);
		this.varOverflow = BooleanAccumulator.treeGate(Nary.OR);
	}
	
	/**  
	 * Constructs a new environment with the specified parent
	 * and mapping.
	 * 
	 * @ensures this.parent' = parent && this.variable' = variable && this.value' = value
	 */
	private Environment(Environment<T, E> parent, Variable variable, E type, T value) {
		this.variable = variable;
		this.value = value;
		this.type = type; 
		this.parent = parent;
		this.overflow = BooleanAccumulator.treeGate(Nary.OR);
		this.varOverflow = BooleanAccumulator.treeGate(Nary.OR);
	}
	
	/**
	 * Returns the empty environment.
	 * @return the empty environment.
	 */
	public static <T, E> Environment<T, E> empty() {
	    return new Environment<T, E>();
	}
	
	/**
	 * Returns the parent environment of this, or null if this
	 * does not have a parent.
	 * 
	 * @return this.parent
	 */
	public Environment<T, E> parent()          { return parent; }
	public BooleanAccumulator overflow()       { return overflow; }
    public BooleanAccumulator varOverflow()    { return varOverflow; }

    /**
	 * Returns a new environment that extends this environment with the specified
	 * mapping.
	 * @requires variable != null
	 * @return e : Environment | e.parent = this && e.variable = variable && e.value = value
	 */
	public Environment<T, E> extend(Variable variable, E type, T value) {
		assert variable !=  null;
		return new Environment<T, E>(this, variable, type, value);
	}
	
	public void addOverflowClause(BooleanValue v)    { this.overflow.add(v); }	
	public void addVarOverflowClause(BooleanValue v) { this.varOverflow.add(v); }
	
	/**
	 * Returns this.variable.
	 * @return this.variable
	 */
	public Variable variable() {
		return this.variable;
	}
	
	/**
	 * Return this.isInt. 
	 * @return this.isInt
	 */
	public E type() {
	    return this.type;
	}
	
	/**
	 * Returns this.value.
	 * @return this.value
	 */
	public T value() {
		return this.value;
	}
	
	/**
	 * Returns true if this is the empty (root) environment;
	 * otherwise returns false.
	 * @return this.parent = this
	 */
	public boolean isEmpty() {
		return this==this.parent;
	}
		
	/**
	 * Looks up the given variable in this environment and its
	 * ancestors.  If the variable is not bound in this
	 * environment or any of its ancestors, null is returned.
	 * If the variable is bound in multiple environments,
	 * the first found binding is returned.  Note that null
	 * will also be returned if the variable is bound to null.
	 * @return variable = this.variable => this.value, this.parent.lookup(variable)
	 */
	public T lookup(Variable variable) {
		Environment<T, E> p = this;
		// ok to use == for testing variable equality: 
		// see kodkod.ast.LeafExpression#equals
		while(!p.isEmpty() && p.variable!=variable) {
			p = p.parent;
		}
		return p.value;
	}
	
	public E lookupType(Variable variable) {
        Environment<T, E> p = this;
        // ok to use == for testing variable equality: 
        // see kodkod.ast.LeafExpression#equals
        while(!p.isEmpty() && p.variable!=variable) {
            p = p.parent;
        }
        return p.type;
    }
	
	/**
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return (parent.isEmpty() ? "[]" : parent.toString()) + "["+variable+"="+value+"]";
	}

}
