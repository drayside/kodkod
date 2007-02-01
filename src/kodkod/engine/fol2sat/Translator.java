/*
 * Translator.java
 * Created on Jul 5, 2005
 */
package kodkod.engine.fol2sat;

import java.util.Map;

import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.IntExpression;
import kodkod.ast.Relation;
import kodkod.engine.bool.BooleanConstant;
import kodkod.engine.bool.BooleanFormula;
import kodkod.engine.bool.BooleanMatrix;
import kodkod.engine.bool.BooleanValue;
import kodkod.engine.bool.Int;
import kodkod.engine.bool.Operator;
import kodkod.engine.config.Options;
import kodkod.engine.config.Reporter;
import kodkod.engine.satlab.SATSolver;
import kodkod.instance.Bounds;
import kodkod.instance.Instance;
import kodkod.util.ints.IntSet;


/** 
 * Translates a formula in first order logic, represented as an
 * {@link kodkod.ast.Formula abstract syntax tree}, into a 
 * {@link kodkod.engine.satlab.SATSolver cnf formula}.
 * @author Emina Torlak 
 */
public final class Translator {
	
	/**
	 * Translates the given formula using the specified bounds and options.
	 * @return a Translation whose solver is a SATSolver instance initalized with the 
	 * CNF representation of the given formula, with respect to the given bounds.
	 * @throws TrivialFormulaException - the given formula is reduced to a constant during translation
	 * (i.e. the formula is trivially (un)satisfiable).
	 * @throws NullPointerException - any of the arguments are null
	 * @throws UnboundLeafException - the formula refers to an undeclared variable or a relation not mapped by the given bounds.
	 * @throws HigherOrderDeclException - the formula contains a higher order declaration that cannot
	 * be skolemized, or it can be skolemized but options.skolemize is false.
	 */
	public static Translation translate(Formula formula, Bounds bounds, Options options) throws TrivialFormulaException {
		final Reporter reporter = options.reporter(); // grab the reporter
		
		// extract structural information about the formula (i.e. syntactically shared internal nodes)
		reporter.collectingStructuralInfo();
		AnnotatedNode<Formula> annotated = new AnnotatedNode<Formula>(formula);
				
		// copy the bounds and remove bindings for unused relations/ints
		bounds = bounds.clone();
		bounds.relations().retainAll(annotated.relations());
		if (!annotated.usesIntBounds()) bounds.ints().clear();
		
		// detect and break symmetries on total orders and acyclic relations
		SymmetryBreaker breaker = new SymmetryBreaker(bounds, options);
		breaker.breakPredicateSymmetries(annotated.predicates());
			
		// skolemize
		if (options.skolemDepth()>=0) {	annotated = Skolemizer.skolemize(annotated, bounds, options); } 

		// translate to boolean, checking for trivial (un)satisfiability
		reporter.translatingToBoolean(formula, bounds);	
		LeafInterpreter interpreter = LeafInterpreter.exact(bounds, options);
		BooleanValue circuit; 
		TranslationLog log = null;	
		if (options.logTranslation()) {
			final TranslationLogger logger = new FileLogger(annotated, bounds);
			circuit = FOL2BoolTranslator.translate(annotated, interpreter, logger, options.interruptible());
			log = logger.log();
			if (circuit.op()==Operator.CONST) {
				throw new TrivialFormulaException(TrivialFormulaReducer.reduce(annotated.node()==formula ? annotated : new AnnotatedNode<Formula>(formula), breaker.broken(), log), bounds, (BooleanConstant) circuit);
			}
		} else {
			circuit = (BooleanValue)FOL2BoolTranslator.translate(annotated, interpreter, options.interruptible());
			if (circuit.op()==Operator.CONST) {
				throw new TrivialFormulaException(formula, bounds, (BooleanConstant)circuit);
			}
		}
		annotated = null; // release structural information
		
		// break lex symmetries
		reporter.breakingSymmetries();
		circuit = interpreter.factory().and(circuit, breaker.breakLexSymmetries(interpreter));
		breaker = null; // release the symmetry breaker
	
		// flatten
		if (options.flatten()) {
			reporter.flattening((BooleanFormula)circuit);
			circuit = BooleanFormulaFlattener.flatten((BooleanFormula)circuit, interpreter.factory());
		}

		final Map<Relation, IntSet> varUsage = interpreter.vars();
		final int numPrimaryVariables = interpreter.factory().numberOfVariables();
		interpreter = null; // release the interpreter
		
		if (circuit.op()==Operator.CONST) {
			throw new TrivialFormulaException(formula, bounds, (BooleanConstant)circuit);
		}
		
		// translate to cnf and return the translation
		reporter.translatingToCNF((BooleanFormula)circuit);
		final SATSolver cnf = Bool2CNFTranslator.translate((BooleanFormula)circuit, options.solver(), numPrimaryVariables);
		return new Translation(cnf, bounds, varUsage, numPrimaryVariables, log);
	}
	
	/**
	 * Overapproximates the value of the given expression using the provided bounds and options.
	 * @return a BooleanMatrix whose TRUE entries represent the tuples contained in a sound overapproximation
	 * of the expression.
	 * @throws expression = null || instance = null || options = null
	 * @throws UnboundLeafException - the expression refers to an undeclared variable or a relation not mapped by the instance
	 * @throws HigherOrderDeclException - the expression contains a higher order declaration
	 */
	@SuppressWarnings("unchecked")
	public static BooleanMatrix approximate(Expression expression, Bounds bounds, Options options) {
		return FOL2BoolTranslator.approximate(new AnnotatedNode<Expression>(expression), 
				LeafInterpreter.overapproximating(bounds, options), Environment.EMPTY);
	}
	
	/**
	 * Evaluates the given formula to a BooleanConstant using the provided instance and options.  
	 * 
	 * @return a BooleanConstant that represents the value of the formula.
	 * @throws NullPointerException - formula = null || instance = null || options = null
	 * @throws UnboundLeafException - the formula refers to an undeclared variable or a relation not mapped by the instance
	 * @throws HigherOrderDeclException - the formula contains a higher order declaration
	 */
	public static BooleanConstant evaluate(Formula formula, Instance instance, Options options) {
		return (BooleanConstant) 
		 FOL2BoolTranslator.translate(new AnnotatedNode<Formula>(formula), 
				 LeafInterpreter.exact(instance, options), false);
	}
	
	/**
	 * Evaluates the given expression to a BooleanMatrix using the provided instance and options.
	 * 
	 * @return a BooleanMatrix whose TRUE entries represent the tuples contained by the expression.
	 * @throws NullPointerException - expression = null || instance = null || options = null
	 * @throws UnboundLeafException - the expression refers to an undeclared variable or a relation not mapped by the instance
	 * @throws HigherOrderDeclException - the expression contains a higher order declaration
	 */
	public static BooleanMatrix evaluate(Expression expression,Instance instance, Options options) {
		return (BooleanMatrix) 
		 FOL2BoolTranslator.translate(new AnnotatedNode<Expression>(expression),
				 LeafInterpreter.exact(instance, options), false);
	}

	/**
	 * Evalutes the given intexpression to an {@link kodkod.engine.bool.Int} using the provided instance and options. 
	 * @return an {@link kodkod.engine.bool.Int} representing the value of the intExpr with respect
	 * to the specified instance and options.
	 * @throws NullPointerException - formula = null || instance = null || options = null
	 * @throws UnboundLeafException - the expression refers to an undeclared variable or a relation not mapped by the instance
	 * @throws HigherOrderDeclException - the expression contains a higher order declaration
	 */
	public static Int evaluate(IntExpression intExpr, Instance instance, Options options) {
		return (Int)
		 FOL2BoolTranslator.translate(new AnnotatedNode<IntExpression>(intExpr),
				 LeafInterpreter.exact(instance,options), false);
	}
	
}


