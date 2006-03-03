/*
 * Translator.java
 * Created on Jul 5, 2005
 */
package kodkod.engine.fol2sat;

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
import kodkod.engine.satlab.SATSolver;
import kodkod.instance.Bounds;
import kodkod.instance.Instance;
import kodkod.util.IntSet;


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
		
//		System.out.println("optimizing bounds...");
		bounds = bounds.copy();
		final Set<IntSet> symmetricParts = 
			BoundsOptimizer.optimize(bounds, notes.relations(), 
					                 notes.topLevelOrders(), notes.topLevelAcyclics());
		
		if (options.skolemize()) {
//			System.out.println("skolemizing...");
			formula = Skolemizer.skolemize(formula, notes.sharedNodes(), bounds);
		}
		
		final BooleanVariableAllocator allocator = new BooleanVariableAllocator(bounds, notes.topLevelFunctions());
		final BooleanFactory factory = allocator.factory();
		final int numPrimaryVariables = factory.maxVariableLiteral();
		
//		System.out.println("fol2sat...");
		BooleanValue sat = Fol2BoolTranslator.translate(formula, notes.sharedNodes(), allocator);//formula.accept(new Fol2Sat(allocator, formula, notes.sharedNodes()));
		if (sat==BooleanConstant.TRUE || sat==BooleanConstant.FALSE) {
			throw new TrivialFormulaException(formula, bounds, (BooleanConstant)sat);
		}
		
//		System.out.println("generating sbp...");
		sat = factory.and(sat, SymmetryBreaker.generateSBP(symmetricParts, allocator, options));
		
		if (options.flatten()) {
//			System.out.println("flattening...");
			// remove everything but the variables from the factory
			factory.clear(numPrimaryVariables);
			sat = BooleanFormulaFlattener.flatten(sat, factory);
			// release the memory used by the factory
			factory.clear(0);
		}
		
		if (sat==BooleanConstant.TRUE || sat==BooleanConstant.FALSE) {
			throw new TrivialFormulaException(formula, bounds, (BooleanConstant)sat);
		}
		
//		System.out.println(sat);
//		System.out.println("sat2cnf...");
		final SATSolver cnf = Bool2CnfTranslator.translate(sat, options.solver(), numPrimaryVariables);
		cnf.setTimeout(options.timeout());
		
		return new Translation(cnf, bounds, allocator.allocationMap(), numPrimaryVariables);
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

}
