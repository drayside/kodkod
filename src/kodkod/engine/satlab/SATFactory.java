package kodkod.engine.satlab;

import org.sat4j.minisat.SolverFactory;

/**
 * An enumeration of available SAT solvers that 
 * also doubles as a factory for SATSolver instances
 * of a given type.
 * 
 * @author Emina Torlak
 */
public enum SATFactory {
	/**
	 * A factory for instances of the default sat4j solver.
	 * @see org.sat4j.core.ASolverFactory#defaultSolver()
	 */
	DefaultSAT4J { 
		/**
		 * Returns an instance of the default sat4j solver.
		 * @return an instance of the default sat4j solver.
		 */
		public SATSolver instance() { 
			return new MiniSAT(SolverFactory.newMiniSAT2Heap()); 
		}
	},
	
	/**
	 * A factory for instances of the "light" sat4j solver that is
	 * suitable for solving many small instances of SAT problems.
	 * @see org.sat4j.core.ASolverFactory#lightSolver()
	 */
	LightSAT4J {
		/**
		 * Returns an instance of the "light" sat4j solver.
		 * @return an instance of the "light" sat4j solver.
		 */
		public SATSolver instance() { 
			return new MiniSAT(SolverFactory.newMini3SAT()); 
		}
	},
	
	/**
	 * A factory for instances of the "minilearning" sat4j solver 
	 * that learns clauses of size smaller than 10 % of the total number of variables
	 * @see org.sat4j.minisat.SolverFactory#newMiniLearning()
	 */
	MiniLearning {
		/**
		 * Returns an instance of the "minilearning" sat4j solver.
		 * @return an instance of the "minilearning" sat4j solver.
		 */
		public SATSolver instance() { 
			return new MiniSAT(SolverFactory.newMiniLearning()); 
		}
	},
	
	/**
	 * A factory for instances of the sat4j "active learning" solver that
	 * uses First UIP clause generator, watched literals, etc.
	 * @see org.sat4j.minisat.SolverFactory#newActiveLearning()
	 */
	ActiveLearning {
		/**
		 * Returns an instance of the "active learning" sat4j solver.
		 * @return an instance of the "active learning" sat4j solver.
		 */
		public SATSolver instance() { 
			return new MiniSAT(SolverFactory.newActiveLearning()); 
		}
	},
	
	/**
	 * A factory for instances of the "backjumping" sat4j solver 
	 * with VSIDS heuristics, FirstUIP clause generator for backjumping but no learning.
	 * @see org.sat4j.minisat.SolverFactory#newBackjumping()
	 */
	Backjumping {
		/**
		 * Returns an instance of the "backjumping" sat4j solver.
		 * @return an instance of the "backjumping" sat4j solver.
		 */
		public SATSolver instance() { 
			return new MiniSAT(SolverFactory.newBackjumping()); 
		}
	},
	
	/**
	 * A factory for instances of the sat4j "relsat" solver with 
	 * decision UIP clause generator.
	 * @see org.sat4j.minisat.SolverFactory#newRelsat()
	 */
	Relsat {
		/**
		 * Returns an instance of the sat4j "relsat" solver.
		 * @return an instance of the sat4j "relsat" solver.
		 */
		public SATSolver instance() { 
			return new MiniSAT(SolverFactory.newRelsat()); 
		}
	},
	
	/**
	 * A factory for instances that provide access to the basic
	 * functionality of the zchaff solver from Princeton; 
	 * the returned instances 
	 * support only basic sat solver operations (adding variables/clauses,
	 * solving, and obtaining a satisfying solution, if any).
	 */
	ZChaff {
		/**
		 * Returns an instance of the zchaff solver.
		 * @return an instance of the zchaff solver.
		 */
		public SATSolver instance() { 
			return new ZChaff(); 
		}
	},
	
	/**
	 * A factory for instances that proivde access to the core extraction
	 * functionality of the zchaff solver from Princeton; the returned
	 * instances implement the CoreExctractor interface.  Note that core
	 * extraction can incur a significant memory overhead during solving,
	 * so if you do not need this functionality, use ZChaff factory
	 * instead.
	 */
	ZChaffPlus {
		/**
		 * Returns an instance of the zchaff solver with the core extraction
		 * functionality.
		 * @return an instance of the zchaff solver with the core extraction
		 * functionality.
		 */
		public CoreExtractor instance() { 
			return null; 
		}
		
		/**
		 * Returns true.
		 * @return true
		 */
		public boolean canProveUnsatisfiability() {
			return true;
		}
	};
	
	
	/**
	 * Returns an instance of a SATSolver produced by this factory.
	 * @return a SATSolver instance
	 */
	public abstract SATSolver instance();
	
	/**
	 * Returns true if the SATSolver instances returned by this
	 * factory implement the SymmetryDrivenSolver interface.
	 * @return true if the SATSolvers returned by this
	 * implement the SymmetryDrivenSolver interface.
	 */
	public boolean isSymmetryDriven() {
		return false;
	}
	
	/**
	 * Returns true if the SATSolvers returned by this
	 * factory implement the CoreExtractor interface.
	 * @return true if the SATSolvers returned by this
	 * implement the CoreExtractor interface.
	 */
	public boolean canProveUnsatisfiability() {
		return false;
	}
}
