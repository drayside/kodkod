/*
 * Sat2CnfTranslator.java
 * Created on Aug 29, 2005
 */
package kodkod.engine.fol2sat;

import static kodkod.engine.bool.MultiGate.Operator.AND;

import java.util.Iterator;

import kodkod.engine.Options;
import kodkod.engine.bool.BooleanConstant;
import kodkod.engine.bool.BooleanValue;
import kodkod.engine.bool.BooleanVariable;
import kodkod.engine.bool.BooleanVisitor;
import kodkod.engine.bool.MultiGate;
import kodkod.engine.bool.NotGate;
import kodkod.engine.satlab.SATSolver;
import kodkod.util.IntSet;
import kodkod.util.Ints;


/** 
 * Translates a {@link kodkod.engine.bool.BooleanValue boolean formula} to 
 * a CNF represented as {@link org.sat4j.specs.Vec vectors} readable
 * by {@link org.sat4j.specs.ISolver minSAT solvers}.
 * 
 * @author Emina Torlak 
 */
final class Sat2CnfTranslator {
	
	/**
	 * Returns an instance of SATSolver that contains the CNF representation of the given
	 * formula.  The SATSolver is obtained from the Options object and initialized with
	 * the values of its timeout and random seed options.  
	 * @return a SATSolver obtained from options.solver().instance() and modified to 
	 * contained the CNF representation of the given formula.
	 */
	static SATSolver translate(BooleanValue formula, Options options) {
		final SATSolver solver = options.solver().instance();
		solver.setTimeout(options.timeout());
		final Translator translator = new Translator(solver, StrictMath.abs(formula.literal()));
		solver.addClause(formula.accept(translator,null));
		return solver;
	}
	
	/**
	 * The helper class that actually performs translation.
	 */
	private static final class Translator implements BooleanVisitor<int[], Object> {
		private final SATSolver solver;
		private final IntSet visited;
		private final int[] unaryClause = new int[1];
				
		private Translator(SATSolver solver, int numPrimaryVars, int maxLiteral) {
			this.solver = solver;
			this.solver.addVariables(maxLiteral);
			final int minGateLiteral = numPrimaryVars+1;
			this.visited = Ints.bestSet(minGateLiteral, StrictMath.max(minGateLiteral, maxLiteral));
		}
		
		private Translator(SATSolver solver, int maxLiteral) {
			this.solver = solver;
			this.solver.addVariables(maxLiteral);
			this.visited = Ints.bestSet(StrictMath.abs(maxLiteral)+1);
		}
		
		/**
		 * Adds translation clauses to the solver and returns a VecInt containing the
		 * gate's literal. The CNF clauses are generated according to the standard SAT to CNF translation:
		 * o = AND(i1, i2, ... ik) ---> (i1 | !o) & (i2 | !o) & ... & (ik | !o) & (!i1 | !i2 | ... | !ik | o),
		 * o = OR(i1, i2, ... ik)  ---> (!i1 | o) & (!i2 | o) & ... & (!ik | o) & (i1 | i2 | ... | ik | !o).
		 * @return o: int[] | o.length = 1 && o.[0] = multigate.literal
		 * @effects if the multigate has not yet been visited, its children are visited
		 * and the clauses are added to the solver connecting the multigate's literal to
		 * its input literal, as described above.
		 */
		public int[] visit(MultiGate multigate, Object arg) {  
			final int oLit = multigate.literal();
			if (visited.add(oLit)) { 
				final int sgn  = (multigate.op()==AND ? 1 : -1);
				final int[] lastClause = new int[multigate.numInputs()+1];
				final int[] binaryClause = {0, oLit * -sgn};
				int i = 0;
				for(Iterator<BooleanValue> inputs = multigate.inputs(); inputs.hasNext();) {
					int iLit = inputs.next().accept(this, arg)[0];
					binaryClause[0] = iLit * sgn;
					solver.addClause(binaryClause);
					lastClause[i++] = iLit * -sgn;
				}
				lastClause[i] = oLit * sgn;
				solver.addClause(lastClause);
			}
			unaryClause[0] = oLit;
			return unaryClause;        
		}
		
		
		/** 
		 * Returns the negation of the result of visiting negation.input, wrapped in
		 * an array.
		 * @return o: int[] | o.length = 1 && o[0] = - translate(negation.inputs)[0]
		 *  */
		public int[] visit(NotGate negation, Object arg) {
			final int[] o = negation.input().accept(this, arg);
			assert o.length == 1;
			o[0] = -o[0];
			return o;
		}
		
		/**
		 * Returns the variable's literal wrapped in a an array.
		 * @return o: int[] | o.length = 1 && o[0] = variable.literal
		 */
		public int[] visit(BooleanVariable variable, Object arg) {
			unaryClause[0] = variable.literal();
			return unaryClause;
		}
		
		/**
		 * Throws an UnsupportedOperationException.
		 * @throws UnsupportedOperationException
		 */
		public int[] visit(BooleanConstant constant, Object arg) {
			throw new UnsupportedOperationException();
		}
		
	}
	
}
