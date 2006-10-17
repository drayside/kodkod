package kodkod.engine;

import kodkod.engine.satlab.SATFactory;

/**
 * This class stores information about various
 * user-level translation options.  It can be
 * used to choose the SAT solver, set the timeout, etc.
 * 
 * @specfield solver: SATFactory // SAT solver factory to use
 * @specfield symmetryBreaking: int // the amount of symmetry breaking to perform
 * @specfield sharing: int // the depth to which circuits should be checked for equivalence during translation
 * @specfield intEncoding: IntEncoding // encoding to use for translating {@link kodkod.ast.IntExpression int expressions}
 * @specfield bitwidth: int // the bitwidth to use for integer representation / arithmetic
 * @specfield skolemDepth: int // skolemization depth
 * @specfield flatten: boolean // eliminate extraneous intermediate variables?
 * @specfield trackVars: boolean // keep track of variables assigned to non-leaf nodes?
 * @author Emina Torlak
 */
public final class Options {
	private SATFactory solver = SATFactory.DefaultSAT4J;
	private int symmetryBreaking = 20;
	private IntEncoding intEncoding = IntEncoding.BINARY;
	private int bitwidth = 5;
	private int sharing = 3;
	private int skolemDepth = 0;
	private boolean flatten = true;
	private boolean trackVars = false;
	
	/**
	 * Constructs an Options object initalized with 
	 * default values.
	 * @effects this.solver' = SATFactory.DefaultSAT4J
	 *          this.symmetryBreaking' = 20
	 *          this.sharing' = 3
	 *          this.intEncoding' = BINARY
	 *          this.bitwidth' = 5
	 *          this.skolemDepth' = 0
	 *          this.flatten' = true
	 *          this.trackVars' = false
	 */
	public Options() {}
	
	/**
	 * Constructs an Options object using the given
	 * value for the solver option and default values
	 * for other options.
	 * @effects this.solver' = solver
	 *          this.seed' = 0
	 *          this.symmetryBreaking' = 20
	 *          this.sharing' = 3
	 *          this.intEncoding' = BINARY
	 *          this.bitwidth' = 5
	 *          this.skolemDepth' = 0
	 *          this.flatten' = true
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
	 * @throws IllegalArgumentException - arg !in [min..max]
	 */
	private void checkRange(int arg, int min, int max) {
		if (arg < min || arg > max)
			throw new IllegalArgumentException(arg + " !in [" + min + ".." + max + "]");
	}
	

	
	/**
	 * Returns the integer encoding that will be used for translating {@link kodkod.ast.IntExpression int nodes}.
	 * The default is BINARY representation, which allows negative numbers.  UNARY representation is best suited to
	 * problems with small scopes, in which cardinalities are only compared (and possibly added to each other or
	 * non-negative numbers).   
	 * @return this.intEncoding
	 */
	public IntEncoding intEncoding() { 
		return intEncoding;
	}
	
	/**
	 * Sets the intEncoding option to the given value.
	 * @effects this.intEncoding' = encoding
	 * @throws NullPointerException - encoding = null
	 */
	public void setIntEncoding(IntEncoding encoding) {
		if (encoding==null) throw new NullPointerException();
		this.intEncoding = encoding;
	}
	
	/**
	 * Returns the size of the integer representation.  For example, if this.intEncoding is 
	 * BINARY and this.bitwidth = 5 (the default), then all operations will yield 
	 * one of the five-bit numbers in the range [-16..15].  If this.intEncoding is UNARY and
	 * this.bitwidth = 5, then all operations will yield one of the numbers in the
	 * range [0..5].  
	 * @return this.bitwidth
	 */
	public int bitwidth() {
		return bitwidth;
	}
	
	/**
	 * Sets this.bitwidth to the given value.
	 * @effects this.bitwidth' = bitwidth
	 * @throws IllegalArgumentException - bitwidth < 1
	 */
	public void setBitwidth(int bitwidth) {
		checkRange(bitwidth, 1, Integer.MAX_VALUE);
		this.bitwidth = bitwidth;
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
	 * Returns the depth to which circuits are checked for equivalence during translation.
	 * The default depth is 3, and the minimum allowed depth is 1.  Increasing the sharing
	 * may result in a smaller CNF, but at the cost of slower translation times.
	 * @return this.sharing
	 */
	public int sharing() {
		return sharing;
	}
	
	/**
	 * Sets the sharing option to the given value.
	 * @effects this.sharing' = sharing
	 * @throws IllegalArgumentException - sharing !in [1..Integer.MAX_VALUE]
	 */
	public void setSharing(int sharing) {
		checkRange(sharing, 1, Integer.MAX_VALUE);
		this.sharing = sharing;
	}
	
	/**
	 * Returns the depth to which existential quantifiers are skolemized.
	 * A negative depth  means that no skolemization is performed.
	 * The default depth of 0 means that only existentials that are not nested
	 * within a universal quantifiers are skolemized.  A depth of 1 means that 
	 * existentials nested within a single universal are also skolemized, etc.
	 * @return this.skolemDepth
	 */
	public int skolemDepth() {
		return skolemDepth;
	}
	
	/**
	 * Sets the skolemDepth to the given value. 
	 * @effects this.skolemDepth' = skolemDepth
	 * @throws IllegalArgumentException - this.trackVars && skolemDepth >= 0
	 */
	public void setSkolemDepth(int skolemDepth) {
		if (trackVars && skolemDepth>=0)
			throw new IllegalStateException("trackVars enabled:  skolemization must be off.");
		this.skolemDepth = skolemDepth;
	}
	
	/**
	 * Returns true if a mapping from non-leaf nodes to boolean variables that
	 * represent them should be generated during translation.  This is useful
	 * for determining which formulas/expressions occur in the unsat core of an 
	 * unsatisfiable formula.  Flattening and skolemization  must be off whenever 
	 * this flag is enabled.  Variable tracking is off by default, since 
	 * it incurs a non-trivial memory overheaad.
	 * @return this.trackVars
	 */
	public boolean trackVars() {
		return trackVars;
	}
	
	/**
	 * Sets the value of the variable tracking flag.  If the 
	 * flag is turned on, flattening and skolemization are automatically disabled.
	 * @effects this.trackVars' = trackVars &&
	 *          trackVars => this.flatten' = false && this.skolemDepth' < 0
	 */
	public void setTrackVars(boolean trackVars) {
		if (trackVars) {
			flatten = false;
			skolemDepth = -1;
		}
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
		b.append("\n intEncoding: ");
		b.append(intEncoding);
		b.append("\n bitwidth: ");
		b.append(bitwidth);
		b.append("\n sharing: ");
		b.append(sharing);
		b.append("\n flatten: ");
		b.append(flatten);
		b.append("\n symmetryBreaking: ");
		b.append(symmetryBreaking);
		b.append("\n skolemDepth: ");
		b.append(skolemDepth);
		b.append("\n trackVars: ");
		b.append(trackVars);
		return b.toString();
	}
	
	/**
	 * Integer encoding options for the translation of 
	 * {@link kodkod.ast.IntExpression int expressions}.
	 */
	public static enum IntEncoding {
		/**
		 * Unary encoding of integers supports comparisons and
		 * addition of non-negative numbers.
		 */
		UNARY,
		/**
		 * Two's-complement encoding of integers supports
		 * comparisons, addition, subtraction, multiplication,
		 * and division.
		 */
		BINARY
	}
	
}
