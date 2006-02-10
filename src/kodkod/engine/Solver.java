/*
 * Solver.java
 * Created on May 18, 2005
 */
package kodkod.engine;

import kodkod.ast.Formula;
import kodkod.ast.Relation;
import kodkod.engine.fol2sat.Translator;
import kodkod.engine.fol2sat.Translation;
import kodkod.instance.Bounds;
import kodkod.instance.Instance;

import org.sat4j.specs.TimeoutException;


/** 
 * Implementation of a computational engine for solving relational formulae.
 * A {@link kodkod.ast.Formula formula} is solved with respect to given 
 * {@link kodkod.instance.Bounds bounds}.
 * 
 * @specfield solverName: SATSolverName // name of the SAT solver used by this solver
 * @specfield timeout: int // timeout for the solver, in seconds
 * @specfield options: Options 
 * @author Emina Torlak 
 */
public final class Solver {
	private int numPrimaryVars, numIntermediateVars, numClauses;
	private final Options options;
	
	/**
	 * Constructs a new Solver with the default options.
	 * @effects this.options' = new Options()
	 */
	public Solver() {
		this.options = new Options();
	}
	
	/**
	 * Constructs a new Solver with the given options.
	 * @effects this.options' = options
	 * @throws NullPointerException - options = null
	 */
	public Solver(Options options) {
		if (options==null)
			throw new NullPointerException();
		this.options = options;
	}
	/**
	 * Returns the Options object used by this Solver
	 * to guide translation of formulas from first-order
	 * logic to cnf.
	 * @return this.options
	 */
	public Options options() {
		return options;
	}
	
	/**
	 * Returns the number of primary CNF variables generated while
	 * executing the last call to {@link #solve(Formula, Bounds)}.  
	 * If the solve method has not been called yet, returns -1.
	 * @return the number of primary CNF variables generated by
	 * the most recent call to {@link #solve(Formula, Bounds)}, or
	 * -1 if the method has not been called yet.
	 */
	public int numberOfPrimaryVariables() { return numPrimaryVars; }
	
	/**
	 * Returns the number of intermediate CNF variables generated while
	 * executing the last call to {@link #solve(Formula, Bounds)}.  
	 * If the solve method has not been called yet, returns -1.
	 * @return the number of intermediate CNF variables generated by
	 * the most recent call to {@link #solve(Formula, Bounds)}, or
	 * -1 if the method has not been called yet.
	 */
	public int numberOfIntermediateVariables() { return numIntermediateVars; }
	
	/**
	 * Returns the number of CNF clauses generated while
	 * executing the last call to {@link #solve(Formula, Bounds)}.  
	 * If the solve method has not been called yet, returns -1.
	 * @return the number of CNF clauses generated by
	 * the most recent call to {@link #solve(Formula, Bounds)}, or
	 * -1 if the method has not been called yet.
	 */
	public int numberOfClauses() { return numClauses; }
	
	
	/**
	 * "Pads" the argument instance with the mappings that occur in bounds.lowerBound
	 * but not in the instance. 
	 * @requires instance.relations in bounds.relations
	 * @effects instance.relations' = bounds.relations' &&
	 *          instance.tuples' = bounds.lowerBound ++ instance.tuples
	 * @return instance
	 */
	private static Instance padInstance(Instance instance, Bounds bounds) {
		for(Relation r: bounds.relations()) {
			if (!instance.contains(r)) {
				instance.add(r, bounds.lowerBound(r));
			}
		}
		return instance;
	}
	
	/**
	 * Creates an instance from the given Bounds.  The instance
	 * is simply the mapping bounds.lowerBound.
	 * @return the instance corresponding to bounds.lowerBound
	 */
	private static Instance toInstance(Bounds bounds) {
		final Instance instance = new Instance(bounds.universe());
		for (Relation r : bounds) {
			instance.add(r, bounds.lowerBound(r));
		}
		return instance;
	}
	
	/**
	 * Attempts to satisfy the given formula with respect to the specified instance.
	 * If the operation is successful, the method returns an Instance of the formula (in 
	 * conjunction with constraints implied by the bounds).  If the formula and 
	 * the instance constraints cannot be satisfied, null is returned.
	 * 
	 * If the solver runs out of time, a TimeoutException is thrown.  
	 * 
	 * @return Instance of the formula if it is satisfiable with respect to the given bounds; null otherwise.
	 * @throws NullPointerException - formula = null || bounds = null
	 * @throws IllegalArgumentException - the formula contains an unbound variable
	 * @throws IllegalArgumentException - the formula contains a relation not mapped by the given bounds object
	 * @throws TimeoutException - it takes more than this.timeout of seconds to solve the formula
	 */
	public Instance solve(Formula formula, Bounds bounds) throws kodkod.engine.TimeoutException {
		numPrimaryVars = numIntermediateVars = numClauses = 0;
		
		try {
//			System.out.println("translating...");
			final Translation translation = Translator.translate(formula, bounds, options);
			
			numPrimaryVars = translation.numberOfPrimaryVariables();
			numIntermediateVars = translation.cnf().numberOfVariables() - numPrimaryVars;
			numClauses = translation.cnf().numberOfClauses();
//			System.out.println("p cnf " + translation.cnf().numberOfVariables() + " " + translation.cnf().numberOfClauses());
			if (translation.cnf().solve()) {
				return padInstance(translation.interpret(), bounds);
			}
			
		} catch (TrivialFormulaException trivial) {
			if (trivial.formulaValue().booleanValue()) {
				return padInstance(toInstance(trivial.bounds()), bounds);
			} 
		}
		
		return null;
	}
	
	public String toString() {
		return options.toString();
	}

}
