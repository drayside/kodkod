package kodkod.engine.fol2sat;

import static kodkod.engine.bool.Operator.AND;

import java.util.Iterator;

import kodkod.engine.bool.BooleanFormula;
import kodkod.engine.bool.BooleanVariable;
import kodkod.engine.bool.BooleanVisitor;
import kodkod.engine.bool.ITEGate;
import kodkod.engine.bool.MultiGate;
import kodkod.engine.bool.NotGate;
import kodkod.engine.satlab.SATFactory;
import kodkod.engine.satlab.SATSolver;
import kodkod.util.IntSet;
import kodkod.util.Ints;

/**
 * Transforms a boolean circuit into a formula in conjunctive
 * normal form.
 * 
 * @author Emina Torlak
 */
final class Bool2CnfTranslator {

	private Bool2CnfTranslator() {}

	/**
	 * Creates a new instance of SATSolver using the provided factory
	 * and uses to translate the given circuit into conjunctive normal form.
	 * The third parameter is required to contain the number of primary variables
	 * allocated during translation from FOL to booelan.
	 * @return a SATSolver instance returned by the given factory and initialized
	 * to contain the CNF translation of the given circuit.
	 */
	static SATSolver translate(BooleanFormula circuit, SATFactory factory, int numPrimaryVariables) {
		final SATSolver solver = factory.instance();
		final int maxLiteral = StrictMath.abs(circuit.label());
		solver.addClause(circuit.accept(new Translator(solver, numPrimaryVariables, maxLiteral),null));
		return solver;
	}
	
	/**
	 * The helper class that performs the translation.
	 */
	private static final class Translator implements BooleanVisitor<int[], Object> {
		private final SATSolver solver;
		private final IntSet visited;
		private final int[] unaryClause = new int[1];
		private final int[] binaryClause = new int[2];
		private final int[] ternaryClause = new int[3];
		
		private Translator(SATSolver solver, int numPrimaryVars, int maxLiteral) {
			this.solver = solver;
			this.solver.addVariables(maxLiteral);
			final int minGateLiteral = numPrimaryVars+1;
			this.visited = Ints.bestSet(minGateLiteral, StrictMath.max(minGateLiteral, maxLiteral));
		}
		
		/**
		 * Adds translation clauses to the solver and returns an array containing the
		 * gate's literal. The CNF clauses are generated according to the standard SAT to CNF translation:
		 * o = AND(i1, i2, ... ik) ---> (i1 | !o) & (i2 | !o) & ... & (ik | !o) & (!i1 | !i2 | ... | !ik | o),
		 * o = OR(i1, i2, ... ik)  ---> (!i1 | o) & (!i2 | o) & ... & (!ik | o) & (i1 | i2 | ... | ik | !o).
		 * @return o: int[] | o.length = 1 && o.[0] = multigate.literal
		 * @effects if the multigate has not yet been visited, its children are visited
		 * and the clauses are added to the solver connecting the multigate's literal to
		 * its input literal, as described above.
		 */
		public int[] visit(MultiGate multigate, Object arg) {  
			final int oLit = multigate.label();
			if (visited.add(oLit)) { 
				final int sgn  = (multigate.op()==AND ? 1 : -1);
				final int[] lastClause = new int[multigate.size()+1];
				final int output = oLit * -sgn;
				int i = 0;
				for(Iterator<BooleanFormula> inputs = multigate.iterator(); inputs.hasNext();) {
					int iLit = inputs.next().accept(this, arg)[0];
					binaryClause[0] = iLit * sgn;
					binaryClause[1] = output;
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
		 * Adds translation clauses to the solver and returns an array containing the
		 * gate's literal. The CNF clauses are generated according to the standard SAT to CNF translation:
		 * o = ITE(i, t, e) ---> (!i | !t | o) & (!i | t | !o) & (i | !e | o) & (i | e | !o)
		 * @return o: int[] | o.length = 1 && o.[0] = itegate.literal
		 * @effects if the itegate has not yet been visited, its children are visited
		 * and the clauses are added to the solver connecting the multigate's literal to
		 * its input literal, as described above.
		 */
		public int[] visit(ITEGate itegate, Object arg) {
			final int oLit = itegate.label();
			if (visited.add(oLit)) {
				final int i = itegate.input(0).accept(this, arg)[0];
				final int t = itegate.input(1).accept(this, arg)[0];
				final int e = itegate.input(2).accept(this, arg)[0];
				ternaryClause[0] = -i; ternaryClause[1] = -t; ternaryClause[2] = oLit;
				solver.addClause(ternaryClause);
				ternaryClause[1] = t; ternaryClause[2] = -oLit;
				solver.addClause(ternaryClause);
				ternaryClause[0] = i; ternaryClause[1] = -e; ternaryClause[2] = oLit;
				solver.addClause(ternaryClause);
				ternaryClause[1] = e; ternaryClause[2] = -oLit;
				solver.addClause(ternaryClause);
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
			final int[] o = negation.input(0).accept(this, arg);
			assert o.length == 1;
			o[0] = -o[0];
			return o;
		}
		
		/**
		 * Returns the variable's literal wrapped in a an array.
		 * @return o: int[] | o.length = 1 && o[0] = variable.literal
		 */
		public int[] visit(BooleanVariable variable, Object arg) {
			unaryClause[0] = variable.label();
			return unaryClause;
		}
	}
}
