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
	public static final SATFactory ZChaffBasic = new SATFactory() {
		public SATSolver instance() { 
			return new ZChaffBasic(); 
		}
		public String toString() { return "ZChaffBasic"; }
	};
	
	/**
	 * The factory the produces {@link SATProver core-extracting} 
	 * instances of the zchaff solver from Princeton.  Note that core
	 * extraction can incur a significant memory overhead during solving,
	 * so if you do not need this functionality, use the {@link #ZChaffBasic} factory
	 * instead.
	 */
	public static final SATFactory ZChaffProver = new SATFactory() {
		public SATSolver instance() { 
			return new ZChaffProver(); 
		}
		@Override
		public boolean provers() { return true; }
		public String toString() { return "ZChaffProver"; }
	};
	
	/**
	 * The factory the produces {@link SATMinSolver cost-minimizing} 
	 * instances of the zchaff solver from Princeton.  Note that cost minimization
	 * can incur a time and/or memory overhead during solving,
	 * so if you do not need this functionality, use the {@link #ZChaffBasic} factory
	 * instead.
	 */
	public static final SATFactory ZChaffMincost = new SATFactory() {
		public SATSolver instance() {
			return new ZChaffMincost();
		}
		@Override
		public boolean minimizers() { return true; }
		public String toString() { return "ZChaffMincost"; }
	};
	
	/**
	 * The factory that produces instances of Niklas EŽn and Niklas Sšrensson's
	 * MiniSat solver.
	 */
	public static final SATFactory MiniSat = new SATFactory() {
		public SATSolver instance() {
			return new MiniSAT();
		}
		public String toString() { return "MiniSat"; }
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
	
	/**
	 * Returns true if the solvers returned by this.instance() are
	 * {@link SATProver SATProvers}.  Otherwise returns false.
	 * @return true if the solvers returned by this.instance() are
	 * {@link SATProver SATProvers}.  Otherwise returns false.
	 */
	public boolean provers() {
		return false;
	}
	
	/**
	 * Returns true if the solvers returned by this.instance() are 
	 * {@link SATMinSolver SATMinSolvers}.  Otherwise returns false.
	 * @return true if the solvers returned by this.instance() are
	 * {@link SATMinSolver SATMinSolvers}.  Otherwise returns false.
	 */
	public boolean minimizers() { 
		return false;
	}

}
