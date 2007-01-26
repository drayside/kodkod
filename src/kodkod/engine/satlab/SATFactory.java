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
	 * solving, and obtaining a satisfying solution, if any).  ZChaff is not incremental.
	 */
	public static final SATFactory ZChaff = new SATFactory() {
		public SATSolver instance() { 
			return new ZChaff(); 
		}
		public boolean incremental() { return false; }
		public String toString() { return "ZChaff"; }
	};
	
	
	
	/**
	 * The factory the produces {@link SATMinSolver cost-minimizing} 
	 * instances of the zchaff solver from Princeton.  Note that cost minimization
	 * can incur a time and/or memory overhead during solving,
	 * so if you do not need this functionality, use the {@link #ZChaff} factory
	 * instead.  ZChaffMincost is not incremental.
	 */
	public static final SATFactory ZChaffMincost = new SATFactory() {
		public SATSolver instance() {
			return new ZChaffMincost();
		}
		@Override
		public boolean minimizer() { return true; }
		public boolean incremental() { return false; }
		public String toString() { return "ZChaffMincost"; }
	};
	
	/**
	 * The factory the produces {@link SATProver proof logging} 
	 * instances of the MiniSat solver.  Note that core
	 * extraction can incur a significant time overhead during solving,
	 * so if you do not need this functionality, use the {@link #MiniSat} factory
	 * instead.
	 */
	public static final SATFactory MiniSatProver = new SATFactory() {
		public SATSolver instance() { 
			return new MiniSatProver(); 
		}
		@Override
		public boolean prover() { return true; }
		public String toString() { return "MiniSatProver"; }
	};
	
	/**
	 * The factory that produces instances of Niklas EŽn and Niklas Sšrensson's
	 * MiniSat solver.
	 */
	public static final SATFactory MiniSat = new SATFactory() {
		public SATSolver instance() {
			return new MiniSat();
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
	 * Returns a SATFactory that produces SATSolver wrappers for the external
	 * SAT solver specified by the executable parameter.  The solver's input
	 * and output formats must conform to the SAT competition standards
	 * (http://www.satcompetition.org/2004/format-solvers2004.html).  The solver
	 * will be called with the specified options, and the given tempInput file name will
	 * be used to store the generated CNF files.  If the tempOutput string is empty,
	 * the solver specified by the executable string is assumed to write its output 
	 * to standard out; otherwise, the
	 * solver is assumed to write its output to the tempOutput file.  It is the caller's responsibility to 
	 * delete the temporary file(s) when no longer needed.  External solvers are always
	 * interruptible and never incremental.
	 * @return  SATFactory that produces interruptible SATSolver wrappers for the specified external
	 * SAT solver
	 */
	public static final SATFactory externalFactory(final String executable, final String options, final String tempInput, final String tempOutput) {
		return new SATFactory() {

			@Override
			public SATSolver instance() {
				return new ExternalSolver(executable, options, tempInput, tempOutput);
			}
			
			@Override
			public boolean interruptible() {
				return true;
			}
			
			@Override
			public boolean incremental() {
				return false;
			}
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
	public boolean prover() {
		return false;
	}
	
	/**
	 * Returns true if the solvers returned by this.instance() are 
	 * {@link SATMinSolver SATMinSolvers}.  Otherwise returns false.
	 * @return true if the solvers returned by this.instance() are
	 * {@link SATMinSolver SATMinSolvers}.  Otherwise returns false.
	 */
	public boolean minimizer() { 
		return false;
	}
	
	/**
	 * Returns true if the solvers returned by this.instance() are interruptible;
	 * i.e. if a solver return by this.instance() will terminate the current call 
	 * to solve if the thread in which it is executing is interrupted. 
	 * @return true if the solvers returned by this.instance() are interruptible.
	 */
	public boolean interruptible() {
		return false;
	}
	
	/**
	 * Returns true if the solvers returned by this.instance() are incremental;
	 * i.e. if clauses/variables can be added to the solver between multiple
	 * calls to solve().
	 * @return true if the solvers returned by this.instance() are incremental
	 */
	public boolean incremental() {
		return true;
	}

}
