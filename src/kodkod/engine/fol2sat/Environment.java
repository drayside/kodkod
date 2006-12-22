/*
 * Environment.java
 * Created on May 27, 2005
 */
package kodkod.engine.fol2sat;

import kodkod.ast.Variable;



/** 
 * Represents a  variable binding environment as a 
 * map from a {@link kodkod.ast.Variable variable} to a 
 * {@link java.lang.Object value}.  An environment has
 * a (possibly empty) parent environment to which unsuccessful lookups 
 * are delegated.
 * 
 * @specfield variable: lone Variable
 * @specfield value: lone T
 * @specfield parent: Environment
 *
 * @author Emina Torlak 
 */
@SuppressWarnings("unchecked")
final class Environment<T> {
	private final Variable variable;
	private final T value;
	private final Environment<T> parent;
	
	/**
	 * The empty environment; EMPTY is its own parent.
	 */
	static final Environment EMPTY = new Environment(EMPTY,null,null);
	
	/**  
	 * Constructs a new environment with the specified parent
	 * and mapping.
	 * 
	 * @effects this.parent' = parent && this.variable' = variable && this.value' = value
	 */
	private Environment(Environment<T> parent, Variable variable, T value) {
		this.variable = variable;
		this.value = value;
		this.parent = parent;
	}
	
	/**
	 * Returns the empty environment.
	 * @return the empty environment.
	 */
	public static <T> Environment<T> empty() {
		return (Environment<T>) EMPTY;
	}
	
	/**
	 * Returns the parent environment of this, or null if this
	 * does not have a parent.
	 * 
	 * @return this.parent
	 */
	public Environment<T> parent() { return parent; }
	
	/**
	 * Returns a new environment that extends this environment with the specified
	 * mapping.
	 * @requires variable != null
	 * @return e : Environment | e.parent = this && e.variable = variable && e.value = value
	 */
	public Environment<T> extend(Variable variable, T value) {
		assert variable !=  null;
		return new Environment<T>(this, variable, value);
	}
	
	/**
	 * Returns this.variable.
	 * @return this.variable
	 */
	public Variable variable() {
		return this.variable;
	}
	
	/**
	 * Returns this.value.
	 * @return this.value
	 */
	public T value() {
		return this.value;
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
		Environment<T> p = this;
		// ok to use == for testing variable equality: 
		// see kodkod.ast.LeafExpression#equals
		while(p!=EMPTY && p.variable!=variable) {
			p = p.parent;
		}
		return p.value;
	}
	
	/**
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return (parent == EMPTY ? "" : parent.toString()) + "["+variable+"="+value+"]";
	}
	
	
}
