/*
 * Translator.java
 * Created on Jul 5, 2005
 */
package kodkod.engine.fol2sat;

import java.util.Collections;
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
	
	/**
	 * Returns an exact interpreter based on the given bounds and options.
	 * @return an exact interpreter based on the given bounds and options.
	 */
	private static BoundsInterpreter.Exact exactInterpreter(Bounds bounds, Options options) {
		final Map<Relation, IntRange> vars = new IdentityHashMap<Relation,IntRange>();
		int maxLit = 1;
		for(Relation r : bounds.relations()) {
			int rLits = bounds.upperBound(r).size() - bounds.lowerBound(r).size();
			if (rLits > 0) {
				vars.put(r, Ints.range(maxLit, maxLit + rLits - 1));
				maxLit += rLits;
			}
		}
		return new BoundsInterpreter.Exact(bounds, BooleanFactory.factory(maxLit-1, options), vars);
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
	public static Translation translate(Formula formula, Bounds bounds, Options options) throws TrivialFormulaException {
		// extract structural information about the formula (i.e. syntactically shared internal nodes)
		AnnotatedNode<Formula> annotated = new AnnotatedNode<Formula>(formula);
		// extract top-level predicates
		Map<RelationPredicate.Name, Set<RelationPredicate>> preds = AnnotatedNode.predicates(annotated);
		
		// copy the bounds and optimize the copy by breaking symmetry on total orders and acyclic
		bounds = bounds.clone();
		Set<IntSet> symmetricParts = BoundsOptimizer.optimize(bounds, AnnotatedNode.relations(annotated), preds);
		
		// skolemize
		final Map<Decl, Relation> skolems;
		if (options.skolemize()) {
//			System.out.println("skolemizing...");
			Skolemizer skolemizer = Skolemizer.skolemize(annotated, bounds, constantFactory(options));
			annotated = skolemizer.skolemized();
			skolems = skolemizer.skolems();
		} else {
			skolems = null;
		}
		
		// translate to boolean, checking for trivial (un)satisfiability
		BoundsInterpreter.Exact interpreter = exactInterpreter(bounds, options);
		
		final Map<Node, IntSet> varUsage;
		BooleanValue circuit;
		if (options.trackVars()) {
			FOL2BoolTranslator acircuit = FOL2BoolTranslator.translateAndTrack(annotated,  interpreter);
			circuit = acircuit.translation();
			if (circuit.op()==Operator.CONST) {
				throw new TrivialFormulaException(TrivialFormulaReducer.reduce(annotated,preds,acircuit), 
						(BooleanConstant)circuit, bounds, skolems);
			}
			varUsage = new IdentityHashMap<Node, IntSet>(interpreter.vars().size() + acircuit.variableUsage().size());
			varUsage.putAll(acircuit.variableUsage());
		} else {
			circuit = FOL2BoolTranslator.translate(annotated,  interpreter);
			if (circuit.op()==Operator.CONST) {
				throw new TrivialFormulaException(formula, (BooleanConstant)circuit, bounds, skolems);
			}
			varUsage = new IdentityHashMap<Node, IntSet>(interpreter.vars().size());
		}
		
		annotated = null; // release structural information
		
		for(Map.Entry<Relation, IntRange> e: interpreter.vars().entrySet()) {
			varUsage.put(e.getKey(), Ints.rangeSet(e.getValue()));
		}

		BooleanFactory factory = interpreter.factory();
		final int numPrimaryVariables = factory.numberOfVariables();
		
		// break symmetries on the remaining relations
		circuit = factory.and(circuit, SymmetryBreaker.generateSBP(symmetricParts, interpreter, options.symmetryBreaking()));
	
		interpreter = null; symmetricParts = null; // release the allocator and symmetric partitions

		// flatten
		if (options.flatten()) {
//			System.out.println("flattening...");
			factory.clear(); // remove everything but the variables from the factory
			circuit = BooleanFormulaFlattener.flatten((BooleanFormula)circuit, factory);
			factory = null; // release the factory itself
		}
		
		if (circuit.op()==Operator.CONST) {
			throw new TrivialFormulaException(formula, (BooleanConstant)circuit, bounds, skolems);
		}
		
//		System.out.println("translating to cnf...");
		
		// translate to cnf and return the translation
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


