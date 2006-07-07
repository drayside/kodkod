package kodkod.engine.satlab;

import org.sat4j.minisat.SolverFactory;

/**
 * An enumeration of available SAT solvers whose members
 * are factories for SATSolver instances of a given type.
 * 
 * @author Emina Torlak
 */
public abstract class SATFactory {
	private SATFactory() {}
	
	/**
	 * The factory that produces instances of the default sat4j solver.
	 * @see org.sat4j.core.ASolverFactory#defaultSolver()
	 */
	public static final SATFactory DefaultSAT4J = new SATFactory() { 
		/**
		 * Returns an instance of the default sat4j solver.
		 * @return an instance of the default sat4j solver.
		 */
		public SATSolver instance() { 
			return new SAT4J(SolverFactory.instance().defaultSolver()); 
		}
		public String toString() { return "DefaultSAT4J"; }
	};
	
	/**
	 * The factory that produces instances of the "light" sat4j solver.  The
	 * light solver is suitable for solving many small instances of SAT problems.
	 * @see org.sat4j.core.ASolverFactory#lightSolver()
	 */
	public static final SATFactory LightSAT4J = new SATFactory() {
		/**
		 * Returns an instance of the "light" sat4j solver.
		 * @return an instance of the "light" sat4j solver.
		 */
		public SATSolver instance() { 
			return new SAT4J(SolverFactory.instance().lightSolver()); 
		}
		public String toString() { return "LightSAT4J"; }
	};
	
	/**
	 * The factory that produces instances of the zchaff solver from Princeton; 
	 * the returned instances 
	 * support only basic sat solver operations (adding variables/clauses,
	 * solving, and obtaining a satisfying solution, if any).
	 */
	public static final SATFactory ZChaff = new SATFactory() {
		/**
		 * Returns an instance of the zchaff solver.
		 * @return an instance of the zchaff solver.
		 */
		public SATSolver instance() { 
			return new ZChaff(false); 
		}
		public String toString() { return "ZChaff"; }
	};
	
	/**
	 * The factory the produces core-extracting instances of the zchaff solver from Princeton; the 
	 * {@link kodkod.engine.satlab.SATSolver#isCoreExtractor() } method of
	 * the returned instances returns true.  Note that core
	 * extraction can incur a significant memory overhead during solving,
	 * so if you do not need this functionality, use the {@link #ZChaff} factory
	 * instead.
	 */
	public static final SATFactory ZChaffPlus = new SATFactory() {
		/**
		 * Returns an instance of the zchaff solver with the core extraction
		 * functionality.
		 * @return an instance of the zchaff solver with the core extraction
		 * functionality.
		 */
		public SATSolver instance() { 
			return new ZChaff(true); 
		}
		public String toString() { return "ZChaffPlus"; }
	};
	
	/**
	 * Returns a SATFactory that produces instances of the specified
	 * SAT4J solver.  For the list of available SAT4J solvers see
	 * {@link org.sat4j.core.ASolverFactory#solverNames() org.sat4j.core.ASolverFactory#solverNames()}.
	 * @requires solverName is a valid solver name
	 * @return a SATFactory that returns the instances of the specified
	 * SAT4J solver
	 * @see org.sat4j.core.ASolverFactory#solverNames()
	 */
	public static final SATFactory sat4jFactory(final String solverName) {
		return new SATFactory() {
			@Override
			public SATSolver instance() {
				return new SAT4J(SolverFactory.instance().createSolverByName(solverName));
			}
			public String toString() { return solverName; }
		};
	}
	
	/**
	 * Returns an instance of a SATSolver produced by this factory.
	 * @return a SATSolver instance
	 */
	public abstract SATSolver instance();

}
