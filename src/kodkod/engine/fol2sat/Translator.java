/*
 * Translator.java
 * Created on Jul 5, 2005
 */
package kodkod.engine.fol2sat;

import java.util.Collections;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.Map;
import java.util.Set;

import kodkod.ast.Decl;
import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.Node;
import kodkod.ast.Relation;
import kodkod.ast.RelationPredicate;
import kodkod.engine.Options;
import kodkod.engine.bool.BooleanConstant;
import kodkod.engine.bool.BooleanFactory;
import kodkod.engine.bool.BooleanFormula;
import kodkod.engine.bool.BooleanMatrix;
import kodkod.engine.bool.BooleanValue;
import kodkod.engine.bool.Operator;
import kodkod.engine.satlab.SATSolver;
import kodkod.instance.Bounds;
import kodkod.instance.Instance;
import kodkod.util.ints.IntRange;
import kodkod.util.ints.IntSet;
import kodkod.util.ints.Ints;


/** 
 * Translates a formula in first order logic, represented as an
 * {@link kodkod.ast.Formula abstract syntax tree}, into a 
 * {@link kodkod.engine.satlab.SATSolver cnf formula}.
 * @author Emina Torlak 
 */
public final class Translator {
	private Translator() {}
	
	/**
	 * Determines the ranges of literals that need to be assigned to each
	 * relation in order to represent it as a matrix of boolean values.
	 * Returns the total number of literals needed to represent all relations.
	 */
	private static int assignLiterals(Bounds bounds, Map<Relation, IntRange> literals) {
		int maxLit = 1;
		for(Relation r : bounds.relations()) {
			int rLits = bounds.upperBound(r).size() - bounds.lowerBound(r).size();
			if (rLits > 0) {
				literals.put(r, Ints.range(maxLit, maxLit + rLits - 1));
				maxLit += rLits;
			}
		}
		return maxLit-1;
	}
	
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
	public static Translation translate(Formula formula, Bounds bounds, Options options) 
	throws TrivialFormulaException {
		AnnotatedNode<Formula> annotated = new AnnotatedNode<Formula>(formula);
		Map<RelationPredicate.Name, Set<RelationPredicate>> preds = AnnotatedNode.predicates(annotated);
		bounds = bounds.clone();
		Set<IntSet> symmetricParts = BoundsOptimizer.optimize(bounds, AnnotatedNode.relations(annotated), preds);
		final Map<Decl, Relation> skolems;
		if (options.skolemize()) {
//			System.out.println("skolemizing...");
			Skolemizer skolemizer = Skolemizer.skolemize(annotated, bounds, constantFactory(options));
			annotated = skolemizer.skolemized();
			skolems = skolemizer.skolems();
		} else {
			skolems = null;
		}
		
		final Map<Relation, IntRange> relVars = new HashMap<Relation, IntRange>();
		BooleanFactory factory = BooleanFactory.factory(assignLiterals(bounds, relVars), options);
		
		BoundsInterpreter manager = new BoundsInterpreter.Exact(bounds, factory, relVars);
		
		final int numPrimaryVariables = factory.numberOfVariables();
		
		final Map<Node, IntSet> varUsage;
		BooleanValue circuit;
		if (options.trackVars()) {
			FOL2BoolTranslator acircuit = FOL2BoolTranslator.translateAndTrack(annotated,  manager);
			circuit = acircuit.translation();
			if (circuit.op()==Operator.CONST) {
				throw new TrivialFormulaException(TrivialFormulaReducer.reduce(annotated,preds,acircuit), 
						(BooleanConstant)circuit, bounds, skolems);
			}
			varUsage = new IdentityHashMap<Node, IntSet>(relVars.size() + acircuit.variableUsage().size());
			varUsage.putAll(acircuit.variableUsage());
		} else {
			circuit = FOL2BoolTranslator.translate(annotated,  manager);
			if (circuit.op()==Operator.CONST) {
				throw new TrivialFormulaException(formula, (BooleanConstant)circuit, bounds, skolems);
			}
			varUsage = new IdentityHashMap<Node, IntSet>(relVars.size());
		}
		
		annotated = null; // release structural information
		
		for(Map.Entry<Relation, IntRange> e: relVars.entrySet()) {
			varUsage.put(e.getKey(), Ints.rangeSet(e.getValue()));
		}

		circuit = factory.and(circuit, SymmetryBreaker.generateSBP(symmetricParts, manager, options.symmetryBreaking()));
	
		manager = null; symmetricParts = null; // release the allocator and symmetric partitions

		if (options.flatten()) {
//			System.out.println("flattening...");
			// remove everything but the variables from the factory
			factory.clear();
			circuit = BooleanFormulaFlattener.flatten((BooleanFormula)circuit, factory);
			// release the factory itself
			factory = null;
		}
		
		if (circuit.op()==Operator.CONST) {
			throw new TrivialFormulaException(formula, (BooleanConstant)circuit, bounds, skolems);
		}
		
//		System.out.println("translating to cnf...");
		
		final SATSolver cnf = Bool2CNFTranslator.definitional((BooleanFormula)circuit, options.solver(), numPrimaryVariables);
		cnf.setTimeout(options.timeout());
		return new Translation(cnf, bounds, skolems, Collections.unmodifiableMap(varUsage), numPrimaryVariables, options.trackVars());

	}
	
	/**
	 * Returns a BooleanFactory with 0 variables, which is essentially a factory
	 * that can manipulate only constants.
	 * @return a BooleanFactory with 0 variables, which is essentially a factory
	 * that can manipulate only constants.
	 */
	private static BooleanFactory constantFactory(Options options) { 
		return BooleanFactory.factory(0, options);
	}
	
	/**
	 * Evaluates the given formula to a BooleanConstant using the provided instance and options.  
	 * 
	 * @return a BooleanConstant that represents the value of the formula.
	 * @throws NullPointerException - formula = null || instance = null || options = null
	 * @throws IllegalArgumentException - the formula refers to an undeclared variable or 
	 *                                    a relation not mapped by the instance
	 */
	public static BooleanConstant evaluate(Formula formula, Instance instance, Options options) {
		return (BooleanConstant) 
		 FOL2BoolTranslator.translate(new AnnotatedNode<Formula>(formula), 
				 new InstanceInterpreter(instance, constantFactory(options)));
	}
	
	/**
	 * Evaluates the given expression to a BooleanMatrix using the provided instance and options.
	 * 
	 * @return a BooleanMatrix whose TRUE entries represent the tuples contained by the expression.
	 * @throws NullPointerException - formula = null || instance = null || options = null
	 * @throws IllegalArgumentException - the expression refers to an undeclared variable or 
	 *                                    a relation not mapped by the instance
	 */
	public static BooleanMatrix evaluate(Expression expression,Instance instance, Options options) {
		return (BooleanMatrix) 
		 FOL2BoolTranslator.translate(new AnnotatedNode<Expression>(expression),
				 new InstanceInterpreter(instance, constantFactory(options)));
	}

}


