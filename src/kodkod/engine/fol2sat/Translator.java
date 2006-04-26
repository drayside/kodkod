/*
 * Translator.java
 * Created on Jul 5, 2005
 */
package kodkod.engine.fol2sat;

import java.util.Collections;
import java.util.IdentityHashMap;
import java.util.Map;
import java.util.Set;

import kodkod.ast.BinaryFormula;
import kodkod.ast.ComparisonFormula;
import kodkod.ast.Decl;
import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.MultiplicityFormula;
import kodkod.ast.Node;
import kodkod.ast.QuantifiedFormula;
import kodkod.ast.Relation;
import kodkod.ast.RelationPredicate;
import kodkod.ast.visitor.DepthFirstReplacer;
import kodkod.engine.Options;
import kodkod.engine.TrivialFormulaException;
import kodkod.engine.bool.BooleanConstant;
import kodkod.engine.bool.BooleanFactory;
import kodkod.engine.bool.BooleanFormula;
import kodkod.engine.bool.BooleanMatrix;
import kodkod.engine.bool.BooleanValue;
import kodkod.engine.satlab.SATSolver;
import kodkod.instance.Bounds;
import kodkod.instance.Instance;
import kodkod.util.IntRange;
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
//		System.out.println("analyzing structure...");
		NodeAnalyzer.FormulaAnnotations notes = NodeAnalyzer.annotate(formula);
		bounds = bounds.copy();
		Set<IntSet> symmetricParts = BoundsOptimizer.optimize(bounds, notes.relations(), 
					                                          notes.topLevelOrders(), notes.topLevelAcyclics());
//		System.out.println("Symmetry classes: " + symmetricParts);
//		System.out.println(bounds);
		final Map<Decl, Relation> skolems;
		if (options.skolemize()) {
//			System.out.println("skolemizing...");
			Skolemizer.SkolemizedFormula sf = Skolemizer.skolemize(formula, notes.sharedNodes(), bounds);
			formula = sf.formula();
			skolems = sf.skolems();
		} else {
			skolems = null;
		}
		
		BooleanVariableAllocator allocator = new BooleanVariableAllocator(bounds, notes.topLevelFunctions());
		BooleanFactory factory = allocator.factory();
		final int numPrimaryVariables = factory.maxVariableLabel();
		
//		System.out.println("translating to sat...");
		final Map<Node, IntSet> varUsage;
		BooleanValue circuit;
		if (options.trackVars()) {
			Fol2BoolTranslator.AnnotatedCircuit acircuit = Fol2BoolTranslator.translateAndTrack(formula, notes.sharedNodes(), allocator);
			circuit = acircuit.translation();
			if (circuit==BooleanConstant.TRUE || circuit==BooleanConstant.FALSE) {
				throw new TrivialFormulaException(Reducer.reduce(formula, acircuit, notes), 
						                         (BooleanConstant)circuit, bounds, skolems);
			}
			varUsage = new IdentityHashMap<Node, IntSet>(allocator.allocationMap().size() + acircuit.variableUsage().size());
			varUsage.putAll(acircuit.variableUsage());
		} else {
			circuit = Fol2BoolTranslator.translate(formula, notes.sharedNodes(), allocator);
			if (circuit==BooleanConstant.TRUE || circuit==BooleanConstant.FALSE) {
				throw new TrivialFormulaException(formula, (BooleanConstant)circuit, bounds, skolems);
			}
			varUsage = new IdentityHashMap<Node, IntSet>(allocator.allocationMap().size());
		}
	//	System.out.println(circuit);
	//	System.out.println(allocator.allocationMap());
		notes = null; // release structural information
		
		for(Map.Entry<Relation, IntRange> e: allocator.allocationMap().entrySet()) {
			varUsage.put(e.getKey(), Ints.rangeSet(e.getValue()));
		}
		
//		System.out.println("breaking symmetry...");
		circuit = factory.and(circuit, SymmetryBreaker.generateSBP(symmetricParts, allocator, options));
		
		allocator = null; symmetricParts = null; // release the allocator and symmetric partitions
		
		if (options.flatten()) {
//			System.out.println("flattening...");
			// remove everything but the variables from the factory
			factory.clear(numPrimaryVariables);
			circuit = BooleanFormulaFlattener.flatten((BooleanFormula)circuit, factory);
			// release the memory used by the factory and the factory itself
			factory.clear(0);
			factory = null;
		}
		//System.out.println(circuit);
		if (circuit==BooleanConstant.TRUE || circuit==BooleanConstant.FALSE) {
			throw new TrivialFormulaException(formula, (BooleanConstant)circuit, bounds, skolems);
		}
		
//		System.out.println("translating to cnf...");
		final SATSolver cnf = Bool2CnfTranslator.translate((BooleanFormula)circuit, options.solver(), numPrimaryVariables);
		cnf.setTimeout(options.timeout());
		
		return new Translation(cnf, bounds, skolems, Collections.unmodifiableMap(varUsage), numPrimaryVariables, options.trackVars());
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
	 * Reduces a trivially (un)satisfiable formula to a subtree that 
	 * caused the formula's (un)satisfiability.
	 */
	private static final class Reducer extends DepthFirstReplacer {
		private final Set<Formula> trues, falses;
		private final Map<Node,Node> cache;
		/**
		 * Constructs a reducer for the given formula using the specified values.
		 * @requires notes.formula = formula && acircuit.formula = formula &&
		 * acircuit.translation in BooleanConstant
		 */
		private Reducer(Formula reducible, Fol2BoolTranslator.AnnotatedCircuit acircuit,
	                    NodeAnalyzer.FormulaAnnotations notes) {
			this.trues = acircuit.formulasThatAre(BooleanConstant.TRUE);
			this.falses = acircuit.formulasThatAre(BooleanConstant.FALSE);
			this.cache = new IdentityHashMap<Node,Node>(notes.sharedNodes().size());
			for(Node n: notes.sharedNodes()) {
				cache.put(n,null);
			}
		}
		
		private static <T extends RelationPredicate> 
			Formula predicates(Set<Relation> rels, Set<T> formulaPreds,
					           Set<T> reducedPreds) {
			Formula ret = Formula.TRUE;
			for(T p: formulaPreds) {
				if (!reducedPreds.contains(p) && rels.contains(p.relation()))
					ret = ret.and(p);
			}
		    return ret;
		}
				 
		/**
		 * Reduces the given formula to the subformula that causes it to simplify
		 * to a constant.
		 * @requires notes.formula = formula && acircuit.formula = formula &&
		 * acircuit.translation in BooleanConstant
		 * @return the subformula of the given formula that causes it to simplify
		 * to a constant.
		 */
		static Formula reduce(Formula reducible, Fol2BoolTranslator.AnnotatedCircuit acircuit,
				              NodeAnalyzer.FormulaAnnotations notes) {
			final Reducer r = new Reducer(reducible, acircuit, notes );
			Formula reduced = reducible.accept(r);
			if (reduced != reducible && 
					(!notes.topLevelOrders().isEmpty()
					|| !notes.topLevelAcyclics().isEmpty() || 
					!notes.topLevelFunctions().isEmpty())) {
				final NodeAnalyzer.FormulaAnnotations rnotes = NodeAnalyzer.annotate(reduced);
				final Set<Relation> rrels = rnotes.relations();
				final Set<RelationPredicate.TotalOrdering> rords = rnotes.topLevelOrders();
				for(RelationPredicate.TotalOrdering p: notes.topLevelOrders()) {
					if (!rords.contains(p) && r.trues.contains(p) &&
						(rrels.contains(p.relation()) || rrels.contains(p.first()) ||
						 rrels.contains(p.last()) || rrels.contains(p.ordered())))
						reduced = reduced.and(p);
				}
				reduced = reduced.and(predicates(rrels, notes.topLevelAcyclics(), rnotes.topLevelAcyclics()));
				reduced = reduced.and(predicates(rrels, notes.topLevelFunctions(), rnotes.topLevelFunctions()));
			}
			return reduced;
		}
		
		/**
		 * If the given node is shared and its replacement
		 * cached, the cached value is returned.  Otherwise, null is returned.
		 * @return this.cache[node]
		 */
		@SuppressWarnings("unchecked")
		@Override
		protected <N extends Node> N lookup(N node) {
			return (N) cache.get(node);
		}
		
		/**
		 * Caches the given replacement for the specified node, if the node
		 * is shared.  Otherwise does nothing.  The method returns
		 * the replacement node. 
		 * @effects this.cache' = this.cache ++ node->replacement 
		 * @return replacement
		 */
		@Override
		protected <N extends Node> N cache(N node, N replacement) {
			if (cache.containsKey(node)) {
				cache.put(node, replacement);
			}
			return replacement;
		}
	
		private final boolean isTrue(Formula formula) {
			return trues.contains(formula);
		}
		
		private final boolean isFalse(Formula formula) {
			return falses.contains(formula);
		}
		
		private final boolean isConstant(Formula formula) {
			return falses.contains(formula) || trues.contains(formula);
		}
		
		public Formula visit(BinaryFormula binFormula) {
			Formula ret = lookup(binFormula);
			
			if (ret==null) {
				// if this method was called with this argument,
				// binFormula must be either in trues or falseDecendents
				boolean binValue = isTrue(binFormula); 
				final Formula l = binFormula.left(), r = binFormula.right();
				final Formula lnew, rnew; 
				if (!isConstant(l) && !isConstant(r)) {
					lnew = l; rnew = r;
				} else {
					switch(binFormula.op()) {
					case AND : 
						lnew = binValue || isFalse(l) ? l.accept(this) : Formula.TRUE;
						rnew = binValue || isFalse(r) ? r.accept(this) : Formula.TRUE;
						break;
					case OR : 
						lnew = !binValue || isTrue(l) ? l.accept(this) : Formula.FALSE;
						rnew = !binValue || isTrue(r) ? r.accept(this) : Formula.FALSE;
						break;
					case IMPLIES: // !l || r
						lnew = !binValue || isFalse(l) ? l.accept(this) : Formula.FALSE;  
						rnew = !binValue || isTrue(r) ? r.accept(this) : Formula.FALSE;
						break;
					case IFF: 
						lnew = isConstant(l) ? l.accept(this) : l;
						rnew = isConstant(r) ? r.accept(this) : r;
						break;
					default :
						throw new IllegalArgumentException("Unknown operator: " + binFormula.op());
					}
				}
				ret = (lnew==l && rnew==r) ? binFormula : lnew.compose(binFormula.op(), rnew);     
			}
			return cache(binFormula,ret);
		}
		
		public Formula visit(QuantifiedFormula quantFormula) {
			Formula ret = lookup(quantFormula);
			if (ret==null) {
				ret = quantFormula; // can't reduce inside quantifiers
			}
			return cache(quantFormula,ret);
		}
		
		public Formula visit(ComparisonFormula compFormula) {
			Formula ret = lookup(compFormula);
			if (ret==null) {
				ret = compFormula; // no expression reduction
			}
			return cache(compFormula,ret);
		}
		
		public Formula visit(MultiplicityFormula multFormula) {
			Formula ret = lookup(multFormula);
			if (ret==null) {
				ret = multFormula; // no expression reduction
			}
			return cache(multFormula,ret);
		}
		
		public Formula visit(RelationPredicate pred) {
			Formula ret = lookup(pred);
			if (ret==null) {
				ret = pred; // no expression reduction
			}
			return cache(pred,ret);
		}
	}

}
