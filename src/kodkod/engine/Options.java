package kodkod.engine;

import kodkod.engine.satlab.SATFactory;

/**
 * This class stores information about various
 * user-level translation options.  It can be
 * used to choose the SAT solver, set the timeout, etc.
 * 
 * @specfield solver: SATFactory // SAT solver factory to use
 * @specfield timeout:  int // SAT solver timeout, in seconds
 * @specfield symmetryBreaking: int // the amount of symmetry breaking to perform
 * @specfield skolemize: boolean // skolemize existential quantifiers?
 * @specfield flatten: boolean // eliminate extraneous intermediate variables?
 * @specfield logEncodeFunctions: boolean // use a compact encoding for functions?
 * @specfield trackVars: boolean // keep track of variables assigned to non-leaf nodes?
 * @author Emina Torlak
 */
public final class Options {
	private SATFactory solver = SATFactory.DefaultSAT4J;
	private int timeout = Integer.MAX_VALUE;
	private int symmetryBreaking = 20;
	private boolean skolemize = true;
	private boolean flatten = true;
	private boolean logEncodeFunctions = true;
	private boolean trackVars = false;
	
	
	/**
	 * Constructs an Options object initalized with 
	 * default values.
	 * @effects this.solver' = SATFactory.DefaultSAT4J
	 *          this.timeout' = Integer.MAX_VALUE 
	 *          this.symmetryBreaking' = 20
	 *          this.skolemize' = true
	 *          this.flatten' = true
	 *          this.logEncodeFunctions' = true
	 *          this.trackVars' = false
	 */
	public Options() {}
	
	/**
	 * Constructs an Options object using the given
	 * value for the solver option and default values
	 * for other options.
	 * @effects this.solver' = solver
	 *          this.seed' = 0
	 *          this.timeout' = Integer.MAX_VALUE
	 *          this.symmetryBreaking' = 20
	 *          this.skolemize' = true
	 *          this.flatten' = true
	 *          this.logEncodeFunctions' = true
	 *          this.trackVars' = false
	 * @throws NullPointerException - solver = null
	 */
	public Options(SATFactory solver) {
		this();
		setSolver(solver);
	}
	
	/**
	 * Returns the value of the solver options.
	 * The default is SATSolver.DefaultSAT4J.
	 * @return this.solver
	 */
	public SATFactory solver() {
		return solver;
	}
	
	/**
	 * Sets the solver option to the given value.
	 * @effects this.solver' = solver
	 * @throws NullPointerException - solver = null
	 */
	public void setSolver(SATFactory solver) {
		if (solver==null)
			throw new NullPointerException();
		this.solver = solver;
	}
	
	/**
	 * Returns the number of seconds that the 
	 * SAT solver is allowed to spend on any given problem.
	 * The default is Integer.MAX_VALUE:  the 
	 * SAT solver is allowed to run as long as
	 * necessary to determine the satisfiability
	 * of a formula.
	 * @return this.timeout
	 */
	public int timeout() {
		return timeout;
	}
	
	/**
	 * @throws IllegalArgumentException - arg !in [min..max]
	 */
	private void checkRange(int arg, int min, int max) {
		if (arg < min || arg > max)
			throw new IllegalArgumentException(arg + " !in [" + min + ".." + max + "]");
	}
	
	/**
	 * Sets the timeout option to the given value.
	 * @effects  this.timeout' = timeout
	 * @throws IllegalArgumentException - timeout !in [0..Integer.MAX_VALUE]
	 */
	public void setTimeout(int timeout) {
		checkRange(timeout, 0, Integer.MAX_VALUE);
		this.timeout = timeout;
	}
	
	/**
	 * Returns the value of the flattening flag, which specifies whether
	 * to eliminate extraneous intermediate variables.  The flag is true by default.  
	 * Flattening must be off if the tracking of variables is enabled.  
	 * @return this.flatten
	 */
	public boolean flatten() {
		return flatten;
	}
	
	/**
	 * Sets the flattening option to the given value.
	 * @effects this.flatten' = flatten
	 * @throws IllegalArgumentException - this.trackVars && flatten
	 */
	public void setFlatten(boolean flatten) {
		if (trackVars && flatten)
			throw new IllegalStateException("trackVars enabled:  flattening must be off.");
		this.flatten = flatten;
	}
	
	/**
	 * Returns the 'amount' of symmetry breaking to perform.
	 * If a non-symmetric solver is chosen for this.solver,
	 * this value controls the maximum length of the generated
	 * lex-leader symmetry breaking predicate.  If a symmetric
	 * solver is chosen, this value controls the amount of 
	 * symmetry information to pass to the solver.  (For example,
	 * if a formula has 10 relations on which symmetry can be broken,
	 * and the symmetryBreaking option is set to 5, then symmetry information
	 * will be computed for only 5 of the 10 relations.)  In general, 
	 * the higher this value, the more symmetries will be broken, and the 
	 * faster the formula will be solved.  But, setting the value too high 
	 * may have the opposite effect and slow down the solving.  The default
	 * value for this property is 20.  
	 * @return this.symmetryBreaking
	 */
	public int symmetryBreaking() {
		return symmetryBreaking;
	}
	
	/**
	 * Sets the symmetryBreaking option to the given value.
	 * @effects this.symmetryBreaking' = symmetryBreaking
	 * @throws IllegalArgumentException - symmetryBreaking !in [0..Integer.MAX_VALUE]
	 */
	public void setSymmetryBreaking(int symmetryBreaking) {
		checkRange(symmetryBreaking, 0, Integer.MAX_VALUE);
		this.symmetryBreaking = symmetryBreaking;
	}
	
	/**
	 * Returns the value of the skolemization flag, which
	 * controls whether or not existentially quantified variables are
	 * skolemized.  Skolemization is turned on by default.
	 * @return this.skolemize
	 */
	public boolean skolemize() {
		return skolemize;
	}
	
	/**
	 * Sets the skolemization flag to the given value.
	 * @effects this.skolemize = skolemize
	 */
	public void setSkolemize(boolean skolemize) {
		this.skolemize = skolemize;
	}
	
	/**
	 * Returns true if a compact encoding should be used for functions.
	 * The compact encoding uses N(log M) boolean variables to represent 
	 * a function whose domain and range contain up to N and M values, 
	 * respectively.  (The regular encoding uses N*M variables.)  Although
	 * the compact encoding reduces the number of boolean variables, it
	 * increases the number of clauses which may slow down the SAT solver.  
	 * The default value of this flag is <code>true</code>.
	 * @return this.logEncodeFunctions
	 */
	public boolean logEncodeFunctions() {
		return logEncodeFunctions;
	}
	
	/**
	 * Returns true if a mapping from non-leaf nodes to boolean variables that
	 * represent them should be generated during translation.  This is useful
	 * for determining which formulas/expressions occur in the unsat core of an 
	 * unsatisfiable formula.  The flatten flag must be off whenever 
	 * this flag is enabled.  Variable tracking is off by default, since 
	 * it incurs a non-trivial memory overheaad.
	 * @return this.trackVars
	 */
	public boolean trackVars() {
		return trackVars;
	}
	
	/**
	 * Sets the value of the variable tracking flag.  If the 
	 * flag is turned on, flatten is automatically set to false.
	 * @effects this.trackVars' = trackVars &&
	 *          trackVars => this.flatten' = false
	 */
	public void setTrackVars(boolean trackVars) {
		if (trackVars)
			flatten = false;
		this.trackVars = trackVars;
	}
	
	/**
	 * Returns a string representation of this Options object.
	 * @return a string representation of this Options object.
	 */
	public String toString() {
		StringBuilder b = new StringBuilder();
		b.append("Options:");
		b.append("\n solver: ");
		b.append(solver);
		b.append("\n timeout: ");
		b.append(timeout);
		b.append("\n flatten: ");
		b.append(flatten);
		b.append("\n symmetryBreaking: ");
		b.append(symmetryBreaking);
		b.append("\n skolemize: ");
		b.append(skolemize);
		b.append("\n logEncodeFunctions: ");
		b.append(logEncodeFunctions);
		b.append("\n trackVars: ");
		b.append(trackVars);
		return b.toString();
	}
}
