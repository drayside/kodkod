/*
 * Environment.java
 * Created on May 27, 2005
 */
package kodkod.engine.fol2sat;

import java.util.HashMap;
import java.util.Map;

import kodkod.ast.Variable;



/** 
 * Represents a  variable binding environment as a 
 * map from {@link kodkod.ast.Variable variables} to  
 * {@link java.lang.Object values}.  An environment may have
 * a parent environment to which lookups that are unsuccessful in
 * the child's environment are delegated.
 * 
 * @specfield map:  Variable ->lone T
 * @specfield parent: lone Environment
 *
 * @author Emina Torlak 
 */
final class Environment<T> {
	private final Map<Variable, T> map;
	private final Environment<T> parent;
	
	
	/**  
	 * Constructs a new environment with the specified parent
	 * and mapping.
	 * 
	 * @effects this.parent' = parent && this.map' = map
	 */
	private Environment(Environment<T> parent, Map<Variable, T> map) {
		this.map = map;
		this.parent = parent;
	}
	
	/**  
	 * Constructs a new environment with no parent.
	 * 
	 * @effects no this.parent' && no this.map'
	 */
	public Environment() {
		this(null, new HashMap<Variable, T>(1));
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
	 *  
	 * @return e : Environment | e.parent = this && e.map = variable -> value
	 */
	public Environment<T> extend(Variable variable, T value) {
		final Environment<T> ret = extend(new HashMap<Variable, T>());
		ret.bind(variable, value);
		return ret;
	}
	
	/**
	 * Returns a new environment that extends this environment with the specified
	 * mapping of variables to values.  Any changes to the argument map will be
	 * reflected by this environment.
	 * 
	 * @return e : Environment | e.parent = this && e.map = variableToValue
	 */
	public Environment<T> extend(Map<Variable, T> variableToValue) {
		return new Environment<T>(this, variableToValue);
	}
	
	/**
	 * Returns true if this environment was obtained by extending
	 * the given environment zero or more times.  Note that the
	 * method will return true if the argument is null since 
	 * each environment chain ends in null.
	 * @return environment in this.*parent
	 */
	public boolean isExtensionOf(Environment<T> environment) {
		Environment<T> p = this;
		while (p!=environment && p!=null) {
			p = p.parent;
		}
		return environment == p;
	}
	
	/**
	 * Binds the specified variable to the specified value,
	 * erasing any prior mappings for the variable
	 * 
	 * @effects this.map' = this.map ++ variable->value
	 */
	public void bind(Variable variable, T value) {
		map.put(variable, value);
	}
	
	/**
	 * Returns true if this environment has a binding
	 * for the given variable.
	 * @return some this.map[variable]
	 */
	public boolean binds(Variable variable) {
		return map.containsKey(variable);
	}
	
	/**
	 * Returns the binding for the given variable 
	 * in this environment, if any.  If not, returns
	 * null.
	 * @return this.map[variable]
	 */
	public T get(Variable variable) {
		return map.get(variable);
	}
	
	/**
	 * Returns the closest ancestor of this environment (or this
	 * environment itself) that contains a binding for the given
	 * variable.  If the variable is not bound, null is returned.
	 * @return this.binds(variable) => this, this.parent.bindingEnvironment(variable)
	 */
	public Environment<T> bindingEnvironment(Variable variable) {
		Environment<T> p = this;
		while(p!=null && !p.map.containsKey(variable)) {
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
	 * @return some this.map[variable] => this.map[variable], this.parent.lookup(variable)
	 */
	public T lookup(Variable variable) {
		final Environment<T> bindingEnv = bindingEnvironment(variable);
		return bindingEnv==null ? null : bindingEnv.map.get(variable);
//		T binding = map.get(variable);
//		if (binding == null) {
//			Environment<T> p = parent;
//			while(binding==null && p!=null) {
//				binding = p.map.get(variable);
//				p = p.parent;
//			}
//		}
//		
//		return binding;
	}
	
	
	
	public String toString() {
		return (parent == null ? "" : parent.toString()) + map.toString();
	}
	
	
}
