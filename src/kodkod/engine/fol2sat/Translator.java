/*
 * Translator.java
 * Created on Jul 5, 2005
 */
package kodkod.engine.fol2sat;

import static kodkod.engine.bool.MultiGate.Operator.AND;

import java.util.Iterator;
import java.util.Set;

import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.Node;
import kodkod.engine.Options;
import kodkod.engine.TrivialFormulaException;
import kodkod.engine.bool.BooleanConstant;
import kodkod.engine.bool.BooleanFactory;
import kodkod.engine.bool.BooleanMatrix;
import kodkod.engine.bool.BooleanValue;
import kodkod.engine.bool.BooleanVariable;
import kodkod.engine.bool.BooleanVisitor;
import kodkod.engine.bool.MultiGate;
import kodkod.engine.bool.NotGate;
import kodkod.engine.satlab.SATSolver;
import kodkod.instance.Bounds;
import kodkod.instance.Instance;
import kodkod.util.IntSet;
import kodkod.util.Ints;


/** 
 * Translates a formula in first order logic, represented as an
 * {@link kodkod.ast.Formula abstract syntax tree}, into a 
 * {@link kodkod.engine.satlab.SATSolver cnf formula}.
 * @author Emina Torlak 
 */
public final class Translator {
	private Translator() {}

	/**
	 * Translates the given formula using the specified bounds and options.
	 * @return a Translation whose solver is a SATSolver instance initalized with the 
	 * CNF representation of the given formula, with respect to the given bounds.
	 * @throws TrivialFormulaException - the given formula is reduced to a constant during translation
	 * (i.e. the formula is trivially (un)satisfiable).
	 * @throws NullPointerException - any of the arguments are null
	 * @throws IllegalArgumentExeption - the formula refers to an undeclared variable or a 
	 *                                   relation not mapped by the given bounds.
	 */
	public static Translation translate(Formula formula, Bounds bounds, Options options) throws TrivialFormulaException {
//		System.out.println("getting formula structure...");
		final NodeAnalyzer.FormulaAnnotations notes = NodeAnalyzer.annotate(formula);
		final Bounds optimalBounds = bounds.copy();
//		System.out.println("optimizing bounds...");
		final Set<IntSet> symmetricParts = 
			BoundsOptimizer.optimize(optimalBounds, notes.relations(), 
					                 notes.topLevelOrders(), notes.topLevelAcyclics());

		
		if (options.skolemize()) {
//			System.out.println("skolemizing...");
			formula = Skolemizer.skolemize(formula, notes.sharedNodes(), optimalBounds);
		}
		
		final BooleanVariableAllocator allocator = new BooleanVariableAllocator(optimalBounds, notes.topLevelFunctions());
		final BooleanFactory factory = allocator.factory();
		final int numPrimaryVariables = factory.maxVariableLiteral();
		
//		System.out.println("fol2sat...");
		BooleanValue sat = Fol2BoolTranslator.translate(formula, notes.sharedNodes(), allocator);//formula.accept(new Fol2Sat(allocator, formula, notes.sharedNodes()));
		if (sat==BooleanConstant.TRUE || sat==BooleanConstant.FALSE) {
			throw new TrivialFormulaException(formula, optimalBounds, (BooleanConstant)sat);
		}
		
		final boolean symmetricSolver = false;//options.solver().isSymmetryDriven();

		if (!symmetricSolver) {
//			System.out.println("generating sbp...");
			sat = factory.and(sat, SymmetryBreaker.generateSBP(symmetricParts, allocator, options));
		}
		
		if (options.flatten()) {
//			System.out.println("flattening...");
			// remove everything but the variables from the factory
			factory.clear(numPrimaryVariables);
			sat = BooleanFormulaFlattener.flatten(sat, factory);
			// release the memory used by the factory
			factory.clear(0);
		}
		
		if (sat==BooleanConstant.TRUE || sat==BooleanConstant.FALSE) {
			throw new TrivialFormulaException(formula, optimalBounds, (BooleanConstant)sat);
		}
//		System.out.println(sat);
//		System.out.println("sat2cnf...");
		final SATSolver cnf = options.solver().instance();
		cnf.setTimeout(options.timeout());
		cnf.addClause(sat.accept(new Sat2Cnf(cnf, numPrimaryVariables, StrictMath.abs(sat.literal())),null));
		
		if (symmetricSolver) {
			// add symmetry information to the solver
		}

		return new Translation(cnf, optimalBounds, allocator.allocationMap(), numPrimaryVariables);
	}
	
	/**
	 * Evaluates the given node using the given constant allocator.
	 * @return an object that represents the value of the node
	 * @throws NullPointerException - node = null || allocator = null
	 * @throws IllegalArgumentException - the node refers to an undeclared variable or 
	 *                                    a relation not mapped by the allocator
	 */
	@SuppressWarnings("unchecked")
	static <T> T evaluate(Node node, BooleanConstantAllocator allocator) {
		return (T) Fol2BoolTranslator.translate(node, NodeAnalyzer.detectSharing(node), allocator);//node.accept(new Fol2Sat(allocator, node, NodeAnalyzer.detectSharing(node)));
	}
	
	/**
	 * Evaluates the given formula to a BooleanConstant using the provided instance.  
	 * 
	 * @return a BooleanConstant that represents the value of the formula.
	 * @throws NullPointerException - formula = null || instance = null
	 * @throws IllegalArgumentException - the formula refers to an undeclared variable or 
	 *                                    a relation not mapped by the instance
	 */
	public static BooleanConstant evaluate(Formula formula, Instance instance) {
		return evaluate(formula, new BooleanConstantAllocator.Exact(instance));
	}
	
	/**
	 * Evaluates the given expression to a BooleanMatrix using the provided instance.
	 * 
	 * @return a BooleanMatrix whose TRUE entries represent the tuples contained by the expression.
	 * @throws NullPointerException - formula = null || instance = null
	 * @throws IllegalArgumentException - the expression refers to an undeclared variable or 
	 *                                    a relation not mapped by the instance
	 */
	public static BooleanMatrix evaluate(Expression expression,Instance instance) {
		return evaluate(expression, new BooleanConstantAllocator.Exact(instance));
	}
	
	/**
	 * Performs translation from SAT to CNF.
	 */
	private static final class Sat2Cnf implements BooleanVisitor<int[], Object> {
		private final SATSolver solver;
		private final IntSet visited;
		private final int[] unaryClause = new int[1];
		private final int[] binaryClause = new int[2];
				
		private Sat2Cnf(SATSolver solver, int numPrimaryVars, int maxLiteral) {
			this.solver = solver;
			this.solver.addVariables(maxLiteral);
			final int minGateLiteral = numPrimaryVars+1;
			this.visited = Ints.bestSet(minGateLiteral, StrictMath.max(minGateLiteral, maxLiteral));
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
				final int output = oLit * -sgn;
				int i = 0;
				for(Iterator<BooleanValue> inputs = multigate.inputs(); inputs.hasNext();) {
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
