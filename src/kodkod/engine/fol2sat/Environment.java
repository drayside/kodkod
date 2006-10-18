/*
 * Environment.java
 * Created on May 27, 2005
 */
package kodkod.engine.fol2sat;

import kodkod.ast.Variable;



/** 
 * Represents a  variable binding environment as a 
 * map from a {@link kodkod.ast.Variable variable} to a 
 * {@link java.lang.Object value}.  An environment may have
 * a parent environment to which lookups that are unsuccessful in
 * the child's environment are delegated.
 * 
 * @specfield var: lone Variable
 * @specfield val: lone T
 * @specfield parent: lone Environment
 *
 * @author Emina Torlak 
 */
final class Environment<T> {
	private final Variable var;
	private T val;
	private final Environment<T> parent;
	
	
	/**  
	 * Constructs a new environment with the specified parent
	 * and mapping.
	 * 
	 * @effects this.parent' = parent && this.var' = var && this.val' = val
	 */
	private Environment(Environment<T> parent, Variable var, T val) {
		this.var = var;
		this.val = val;
		this.parent = parent;
	}
	
	/**  
	 * Constructs a new empty environment with no parent.
	 * 
	 * @effects no this.parent' && no this.var' && no this.val'
	 */
	public Environment() {
		this(null, null, null);
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
	 * @return e : Environment | e.parent = this && e.var = variable && e.val = value
	 */
	public Environment<T> extend(Variable variable, T value) {
		return new Environment<T>(this, variable, value);
	}
	
		
	/**
	 * Binds this.var to the specified value,
	 * erasing any prior mappings for the variable in this environment.
	 * 
	 * @effects this.val' = value
	 */
	public void bindVarTo(T value) {
		this.val = value;
	}
	
	/**
	 * Returns the closest ancestor of this environment (or this
	 * environment itself) that contains a binding for the given
	 * variable.  If the variable is not bound, null is returned.
	 * @return this.var = variable => this, this.parent.bindingEnvironment(variable)
	 */
	public Environment<T> bindingEnvironment(Variable variable) {
		Environment<T> p = this;
		// ok to use == for testing variable equality: 
		// see kodkod.ast.LeafExpression#equals
		while(p!=null && p.var!=variable) {
			p = p.parent;
		}
		return p;
	}
	
	/**
	 * Looks up the given variable in this environment and its
	 * ancestors.  If the variable is not bound in this
	 * environment or any of its ancestors, null is returned.
	 * If the variable is bound in multiple environments,
	 * the first found binding is returned.  Note that null
	 * will also be returned if the variable is bound to null.
	 * @return variable = this.var => this.val, this.parent.lookup(variable)
	 */
	public T lookup(Variable variable) {
		final Environment<T> bindingEnv = bindingEnvironment(variable);
		return bindingEnv==null ? null : bindingEnv.val;
	}
	
	
	
	public String toString() {
		return (parent == null ? "" : parent.toString()) + "["+var+"="+val+"]";
	}
	
	
}
