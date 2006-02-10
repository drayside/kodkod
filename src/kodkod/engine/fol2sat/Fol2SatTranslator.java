/*
 * Fol2SatTranslator.java
 * Created on Jul 5, 2005
 */
package kodkod.engine.fol2sat;

import static kodkod.engine.bool.BooleanConstant.FALSE;
import static kodkod.engine.bool.BooleanConstant.TRUE;

import java.util.ArrayList;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import kodkod.ast.BinaryExpression;
import kodkod.ast.BinaryFormula;
import kodkod.ast.ComparisonFormula;
import kodkod.ast.Comprehension;
import kodkod.ast.ConstantExpression;
import kodkod.ast.ConstantFormula;
import kodkod.ast.Decl;
import kodkod.ast.Decls;
import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.IfExpression;
import kodkod.ast.Multiplicity;
import kodkod.ast.MultiplicityFormula;
import kodkod.ast.Node;
import kodkod.ast.NotFormula;
import kodkod.ast.QuantifiedFormula;
import kodkod.ast.Relation;
import kodkod.ast.RelationPredicate;
import kodkod.ast.UnaryExpression;
import kodkod.ast.Variable;
import kodkod.ast.visitor.ReturnVisitor;
import kodkod.engine.Options;
import kodkod.engine.TrivialFormulaException;
import kodkod.engine.bool.BooleanConstant;
import kodkod.engine.bool.BooleanFactory;
import kodkod.engine.bool.BooleanMatrix;
import kodkod.engine.bool.BooleanValue;
import kodkod.engine.bool.Dimensions;
import kodkod.engine.bool.MutableMultiGate;
import kodkod.engine.bool.MultiGate.Operator;
import kodkod.engine.satlab.SATSolver;
import kodkod.instance.Bounds;
import kodkod.instance.Instance;
import kodkod.util.IntSet;


/** 
 * Translates a formula in first order logic, represented as an
 * {@link kodkod.ast.Formula abstract syntax tree}, into a 
 * {@link kodkod.engine.bool.BooleanValue boolean formula}.
 * @author Emina Torlak 
 */
public final class Fol2SatTranslator {
	private Fol2SatTranslator() {}

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
		final int numPrimaryVariables = allocator.numAllocatedVariables();
		
//		System.out.println("fol2sat...");
		BooleanValue sat = formula.accept(new Translator(allocator, notes.sharedNodes()));
		if (sat==BooleanConstant.TRUE || sat==BooleanConstant.FALSE) {
			throw new TrivialFormulaException(formula, optimalBounds, (BooleanConstant)sat);
		}
		
		final boolean symmetricSolver = options.solver().isSymmetryDriven();
		
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
		
		final SATSolver cnf = Sat2CnfTranslator.translate(sat, options);
		
		if (symmetricSolver) {
			// add symmetry information to the solver
		}
//		System.out.println("sat2cnf...");
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
		return (T) node.accept(new Translator(allocator, NodeAnalyzer.detectSharing(node)));
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
	 * The helper class that actually performs the translation.
	 */
	private static final class Translator implements ReturnVisitor<BooleanMatrix, BooleanValue, Object> {
		
		private final BooleanFormulaAllocator allocator;
		
		/* The size of the universal relation */
		private final int univSize;
		
		/* When visiting the body of a quantified formula or a comprehension, this
		 * environment contains the current values of the enclosing quantified variable(s) */
		private Environment<BooleanMatrix> env;
		
		/* Used for caching translation of AST nodes with more than one parent.  If a node
		 * has multiple parents and has not yet been visited, it is mapped to a TranslationInfo
		 * with null translation field.  If it has been visited, the translation field of its 
		 * TranslationInfo is mapped, along with the bindings for any free variables that the
		 * node might have.  Unshared nodes are not mapped by the cache. */
		private final Map<Node,TranslationInfo> cache;
		
		/**
		 * Constructs a new translator that will use the given allocator to perform the 
		 * translation.  The set sharedInternalNodes is used to determine which translations
		 * should be cached; specifically, the set should contain the shared non-leaf
		 * descendents of the formula/expression to which this visitor will be applied. 
		 */    
		Translator(final BooleanFormulaAllocator allocator, Set<Node> sharedInternalNodes) {
			if (allocator==null) throw new NullPointerException("allocator==null");
			this.allocator = allocator;
			this.env = new Environment<BooleanMatrix>();
			this.cache = new IdentityHashMap<Node,TranslationInfo>(sharedInternalNodes.size());
			for(Map.Entry<Node,Set<Variable>> nodeVars : FreeVariableDetector.collectFreeVars(sharedInternalNodes).entrySet()) {
				Set<Variable> freeVars = nodeVars.getValue();
				if (freeVars.isEmpty()) 
					cache.put(nodeVars.getKey(), new NoVarTranslationInfo());
				else 
					cache.put(nodeVars.getKey(), new MultiVarTranslationInfo(freeVars));
			}
			this.univSize = allocator.universe().size();
		}
		
		/**
		 * If the given node has already been visited and its translation
		 * cached, the cached value is returned.  Otherwise, null is returned.
		 * @return this.cache[node]
		 */
		@SuppressWarnings("unchecked")
		private <T> T cachedTranslation(Node node) {
			final TranslationInfo info = cache.get(node);
			return info==null ? null : (T) info.get(env);
		}
		
		/**
		 * Caches the given replacement for the specified node, if the node 
		 * has multiple parents.  Otherwise does nothing.  The method returns
		 * its second argument.  
		 * @effects node in this.cache.keySet() => 
		 *           this.cache' = this.cache ++ node->translation, 
		 *           this.cache' = this.cache
		 * @return translation
		 */
		private <T> T cache(Node node, T translation) {
			final TranslationInfo info = cache.get(node);
			if (info != null) {
				info.set(translation, env);
			}
			return translation;
		}
		
		/**
		 * @return a list of BooleanMatrices A such that A[i] = decls.declarations[i].expression.accept(this) 
		 */
		public List<BooleanMatrix> visit(Decls decls) {
			List<BooleanMatrix> matrices = cachedTranslation(decls);
			if (matrices!=null) return matrices;
			
			final List<Decl> dlist = decls.declarations();
			matrices = new ArrayList<BooleanMatrix>(dlist.size());
			for (Decl decl : dlist) {
				matrices.add(visit(decl));
			}
			
			return cache(decls, matrices);
		}
		
		/**
		 * @return the BooleanMatrix that is the translation of decl.expression.
		 */
		public BooleanMatrix visit(Decl decl) {
			BooleanMatrix matrix = cachedTranslation(decl);
			return matrix==null ? cache(decl, decl.expression().accept(this)) : matrix;
		}
		
		/**
		 * @return a boolean matrix representing the current value of the given quantified variable
		 * @throws IllegalArgumentException - variable is undeclared
		 */
		public BooleanMatrix visit(Variable variable) {
			final BooleanMatrix ret = env.lookup(variable);
			if (ret != null) return ret;
			else throw new IllegalArgumentException("unbound variable " + variable);
		}
		
		/**
		 * @return a matrix of freshly allocated variables representing the given relation
		 * @effects some instance.state[relation] => some allocator.vars'[relation]
		 * @throws IllegalArgumentException - !this.instance.contains(relation)
		 */
		public BooleanMatrix visit(Relation relation) {
			return allocator.allocate(relation);
		}
		
		/**
		 * @return a matrix that represents the given constant expression;
		 * constExpr = Expression.UNIV => {m: BooleanMatrix | m.zero = TRUE && no m.elements},
		 * constExpr = Expression.IDEN => translate(UNIV).diagonal(),
		 * constExpr = Expression.NONE => UNIV.not()
		 */
		public BooleanMatrix visit(ConstantExpression constExpr) {
			final BooleanMatrix ret;
			if (constExpr==Expression.UNIV) {
				ret= allocator.factory().matrix(Dimensions.square(1, univSize), TRUE);
			} else if (constExpr==Expression.IDEN) {
				final Dimensions dim2 = Dimensions.square(2, univSize);
				ret = allocator.factory().matrix(dim2, FALSE);
				for(int i = 0; i < univSize; i++) {
					ret.set(i*univSize + i, TRUE);
				}			
			} else if (constExpr==Expression.NONE) {
				ret = allocator.factory().matrix(Dimensions.square(1, univSize), FALSE);
			} else {
				throw new IllegalArgumentException("unknown constant expression: " + constExpr);
			}
			
			return ret;
		}
		
		/**
		 * @return let tLeft = translate(binExpr.left), tRight = translate(binExpr.right) | 
		 *           binExpr.op = UNION => tLeft.or(tRight),
		 *           binExpr.op = INTERSECTION => tLeft.and(tRight),
		 *           binExpr.op = DIFFERENCE => tLeft.and(tRight.not()),
		 *           binExpr.op = JOIN => tLeft.dotProduct(tRight),
		 *           binExpr.op = PRODUCT => tLeft.crossProduct(tRight)
		 */
		public BooleanMatrix visit(BinaryExpression binExpr) {
			BooleanMatrix ret = cachedTranslation(binExpr);
			if (ret!=null) return ret;

			final BooleanMatrix left = binExpr.left().accept(this);
			final BooleanMatrix right = binExpr.right().accept(this);
			final BinaryExpression.Operator op = binExpr.op();
			
			switch(op) {
			case UNION        	: ret = left.or(right); break;
			case INTERSECTION	: ret = left.and(right); break;
			case DIFFERENCE 		: ret = left.and(right.not()); break;
			case OVERRIDE 		: ret = left.override(right); break;
			case JOIN 			: ret = left.dot(right); break;
			case PRODUCT			: ret = left.cross(right); break;
			default : 
				throw new IllegalArgumentException("Unknown operator: " + op);
			}
			
			return cache(binExpr, ret);
		}
		/**
		 * @return let tChild = translate(unaryExpr.child) |
		 *           unaryExpr.op = TRANSPOSE => tChild.transpose(),
		 *           unaryExpr.op = TRANSITIVE_CLOSURE => tChild.closure() 
		 */
		public BooleanMatrix visit(UnaryExpression unaryExpr) {
			BooleanMatrix ret = cachedTranslation(unaryExpr);
			if (ret!=null) return ret;
			
			final BooleanMatrix child = unaryExpr.expression().accept(this);
			final UnaryExpression.Operator op = unaryExpr.op();
			
			switch(op) {
			case TRANSPOSE         	: ret = child.transpose(); break;
			case CLOSURE           	: ret = child.closure(); break;
			case REFLEXIVE_CLOSURE	: ret = child.closure().or(visit((ConstantExpression)Expression.IDEN)); break;
			default : 
				throw new IllegalArgumentException("Unknown operator: " + op);
			}
			return cache(unaryExpr,ret);
		}
		
		/**
		 * @return { translate(comprehension.declarations) | translate(comprehension.formula) }
		 */
		public BooleanMatrix visit(Comprehension comprehension) {
			BooleanMatrix ret = cachedTranslation(comprehension);
			if (ret!=null) return ret;
			
			/* Let comprehension = { a: A, b: B, ..., x: X | F(a, b, ..., x) }.  It
			 * is translated as { a: A, b: B, ..., x: X | a in A && b in B && ... && x in X && F(a, b, ..., x) }.
			 * Specifically, the appropriate cell in the returned matrix holds the formula
			 * A_i && B_j && ... && X_k && translate(F(A_i, B_j, ..., X_k)), where A_i etc.
			 * stand for boolean variables that represent the tuples of the expression A, etc. */
			final Decls decls = comprehension.declarations();
			final List<BooleanMatrix> declTransls = visit(decls);
			final GroundValueGenerator generator = new GroundValueGenerator(env, decls, declTransls);
			final BooleanFactory factory = allocator.factory();
			
			Dimensions dims = declTransls.get(0).dimensions();
			for (int i = 1; i < declTransls.size(); i++) { 
				dims = dims.cross(declTransls.get(i).dimensions());
			}
			
			ret = factory.matrix(dims, FALSE);
			
			while(generator.hasNext()) {
				env = generator.next(factory);
				
				int[] index = generator.index();
				MutableMultiGate conjunct = MutableMultiGate.treeGate(Operator.AND);
				// A_index[0] && B_index[1] && ... && X_index[index.length-1]
				for(int i = 0; i < index.length; i++) {
					conjunct.addInput(declTransls.get(i).get(index[i]));
				}
				// A_index[0] && B_index[1] && ... && X_index[index.length-1] &&
				//   translate(F(A_index[0], B_index[1], ..., X_index[index.length-1]))
				conjunct.addInput(comprehension.formula().accept(this));
				
				ret.set(dims.convert(index), factory.toImmutableValue(conjunct)); 
			}	
			env = generator.baseEnvironment();
			
			return cache(comprehension,ret);
		}
		
		/**
		 * @return translate(ifExpr.condition) => translate(ifExpr.then), translate(ifExpr.else)
		 */
		public BooleanMatrix visit(IfExpression ifExpr) {
			BooleanMatrix ret = cachedTranslation(ifExpr);
			if (ret!=null) return ret;
			
			final BooleanValue condition = ifExpr.condition().accept(this);
			final BooleanMatrix thenExpr = ifExpr.thenExpr().accept(this);
			final BooleanMatrix elseExpr = ifExpr.elseExpr().accept(this);
			ret = thenExpr.choice(condition, elseExpr);
			
			return cache(ifExpr,ret);
		}
		
		/**
		 * @return constant = ConstantFormula.TRUE => BooleanConstant.TRUE, BooleanConstant.FALSE
		 */
		public BooleanValue visit(ConstantFormula constant) {
			return BooleanConstant.constant(constant.booleanValue());
		}
		
		/**
		 * Translates the given universally quantified formula as follows 
		 * (where A_0...A_|A| stand for boolean variables that represent the 
		 * tuples of the expression A, etc.):
		 * let quantFormula = "all a: A, b: B, ..., x: X | F(a, b, ..., x)" |
		 *     (A_0 && B_0 && ... && X_0 => translate(F(A_0, B_0, ..., X_0))) && ... && 
		 *     (A_|A| && B_|B| && ... && X_|X| => translate(F(A_|A|, B_|B|, ..., X_|X|))
		 * @requires qf.quantifier = ALL && this.cache[qf] = null
		 * @return translation of qf
		 */
		private BooleanValue visitUniversalFormula(QuantifiedFormula qf) {
			// holds the top level conjuction
			final MutableMultiGate conjunct = MutableMultiGate.treeGate(Operator.AND);
			
			final Decls decls = qf.declarations();
			final List<BooleanMatrix> declTransls = visit(decls);
			final GroundValueGenerator generator = new GroundValueGenerator(env, decls, declTransls);
			final BooleanFactory factory = allocator.factory();
			
			while(generator.hasNext() && !conjunct.isShortCircuited()) {
				env = generator.next(factory);
				
				int index[] = generator.index();
				MutableMultiGate disjunct = MutableMultiGate.treeGate(Operator.OR);
				// !A_index[0] || !B_index[1] || ... || !X_index[index.length-1]
				for(int i = 0; i < index.length; i++) {
					disjunct.addInput(factory.not(declTransls.get(i).get(index[i])));
				}
				// !A_index[0] || !B_index[1] || ... || !X_index[index.length-1] ||
				//   translate(F(A_index[0], B_index[1], ..., X_index[index.length-1]))
				disjunct.addInput(qf.formula().accept(this));
				
				conjunct.addInput(factory.toImmutableValue(disjunct));
				
			}
			env = generator.baseEnvironment();
			
			return factory.toImmutableValue(conjunct);
		}
		
		/**
		 * Translates the given existentially quantified formula as follows 
		 * (where A_0...A_|A| stand for boolean variables that represent the 
		 * tuples of the expression A, etc.):
		 * let quantFormula = "some a: A, b: B, ..., x: X | F(a, b, ..., x)" |
		 *     (A_0 && B_0 && ... && X_0 && translate(F(A_0, B_0, ..., X_0))) || ... || 
		 *     (A_|A| && B_|B| && ... && X_|X| && translate(F(A_|A|, B_|B|, ..., X_|X|))
		 * @requires qf.quantifier = SOME && this.cache[qf] = null
		 * @return translation of qf
		 */
		private BooleanValue visitExistentialFormula(QuantifiedFormula qf) {
			// holds the top level disjuction
			final MutableMultiGate disjunct = MutableMultiGate.treeGate(Operator.OR);
			
			final Decls decls = qf.declarations();
			final List<BooleanMatrix> declTransls = visit(decls);
			final GroundValueGenerator generator = new GroundValueGenerator(env, decls, declTransls);
			final BooleanFactory factory = allocator.factory();
			
			while(generator.hasNext() && !disjunct.isShortCircuited()) {
				env = generator.next(factory);
				
				int index[] = generator.index();
				MutableMultiGate conjunct = MutableMultiGate.treeGate(Operator.AND);
				// A_index[0] && B_index[1] && ... && X_index[index.length-1]
				for(int i = 0; i < index.length; i++) {
					conjunct.addInput(declTransls.get(i).get(index[i]));
				}
				// A_index[0] && B_index[1] && ... && X_index[index.length-1] &&
				//   translate(F(A_index[0], B_index[1], ..., X_index[index.length-1]))
				conjunct.addInput(qf.formula().accept(this));
				
				disjunct.addInput(factory.toImmutableValue(conjunct));	
			}
			env = generator.baseEnvironment();
			
			return factory.toImmutableValue(disjunct);
		}
		
		/** 
		 * @return let quantFormula = "quant a: A, b: B, ..., x: X | F(a, b, ..., x)" |
		 *           quant = ALL  => translate(F(A_0, B_0, ..., X_0)) && ... && translate(F(A_|A|, B_|B|, ..., X_|X|))
		 *           quant = SOME => translate(F(A_0, B_0, ..., X_0)) || ... || translate(F(A_|A|, B_|B|, ..., X_|X|))
		 */
		public BooleanValue visit(QuantifiedFormula quantFormula) {
			BooleanValue ret = cachedTranslation(quantFormula);
			if (ret!=null) return ret;
			
			final QuantifiedFormula.Quantifier quantifier = quantFormula.quantifier();
			
			switch(quantifier) {
			case ALL		: ret = visitUniversalFormula(quantFormula); break;
			case SOME	: ret = visitExistentialFormula(quantFormula); break;
			default :
				throw new IllegalArgumentException("Unknown quantifier: " + quantifier);
			}
			
			return cache(quantFormula,ret);
		}
		
		/**
		 * @return let tLeft = translate(binFormula.left), tRight = translate(binFormula.right) | 
		 *           binFormula.op = AND => tLeft.and(tRight),
		 *           binFormula.op = OR => tLeft.or(tRight),
		 *           binFormula.op = IMPLIES => tLeft.not().or(tRight),
		 *           binFormula.op = IFF => tLeft.not().or(tRight).and(tRight.not().or(tLeft)),
		 */
		public BooleanValue visit(BinaryFormula binFormula) {
			BooleanValue ret = cachedTranslation(binFormula);
			if (ret!=null) return ret;
			
			final BooleanValue left = binFormula.left().accept(this);
			final BooleanValue right = binFormula.right().accept(this);
			final BinaryFormula.Operator op = binFormula.op();
			final BooleanFactory f = allocator.factory();
			
			switch(op) {
			case AND		: ret = f.and(left, right); break;
			case OR		: ret = f.or(left, right); break;
			case IMPLIES	: ret = f.implies(left, right); break;
			case IFF		: ret = f.iff(left, right); break;
			default : 
				throw new IllegalArgumentException("Unknown operator: " + op);
			}
		
			return cache(binFormula, ret);
		}
		
		/**
		 * @return translate(not.child).not()
		 */    
		public BooleanValue visit(NotFormula not) {
			BooleanValue ret = cachedTranslation(not);
			return ret==null ? 
					cache(not, allocator.factory().not(not.formula().accept(this))) : ret;
		}
		
		/**
		 * Generates a formula stating that every entry in left implies the corresponding entry in right.
		 * 
		 * @return left.not().or(right).andFold()
		 */
		private BooleanValue subset(final BooleanMatrix left, final BooleanMatrix right) {
			return left.not().or(right).andFold();
		}
		
		/**
		 * @return let tLeft = translate(compFormula.left), tRight = translate(compFormula.right) | 
		 *          compFormula.op = SUBSET => 
		 *           tLeft.not().or(tRight).conjunctiveFold(),
		 *           tLeft.not().or(tRight).and(tRight.not().or(tLeft)).conjunctiveFold()              
		 */
		public BooleanValue visit(ComparisonFormula compFormula) {
			BooleanValue ret = cachedTranslation(compFormula);
			if (ret!=null) return ret;
			
			final BooleanMatrix left = compFormula.left().accept(this);
			final BooleanMatrix right = compFormula.right().accept(this);
			final ComparisonFormula.Operator op = compFormula.op();
			
			switch(op) {
			case SUBSET	: ret = subset(left, right); break;
			case EQUALS	: ret = allocator.factory().and(subset(left, right), subset(right, left)); break;
			default : 
				throw new IllegalArgumentException("Unknown operator: " + compFormula.op());
			}
	
			return cache(compFormula,ret);
		}
		
		/**
		 * @return let child = translate(multFormula.expression) | 
		 *           multFormula.multiplicity = NO => child.none(),
		 *           multFormula.multiplicity = SOME => child.some(),
		 *           multFormula.multiplicity = ONE => child.one(),
		 *           multFormula.multiplicity = LONE => child.lone()
		 */
		public BooleanValue visit(MultiplicityFormula multFormula) {
			BooleanValue ret = cachedTranslation(multFormula);
			if (ret!=null) return ret;
			
			final BooleanMatrix child = multFormula.expression().accept(this);
			final Multiplicity mult = multFormula.multiplicity();
			
			switch(mult) {
			case NO 		: ret = child.none(); break;
			case SOME	: ret = child.some(); break;
			case ONE 	: ret = child.one();  break;
			case LONE 	: ret = child.lone(); break;
			default : 
				throw new IllegalArgumentException("Unknown multiplicity: " + mult);
			}
			
			return cache(multFormula, ret);
		}
		
		/**
		 * @return translate(pred.toConstraints())
		 */
		public BooleanValue visit(RelationPredicate pred) {
			BooleanValue ret = cachedTranslation(pred);
			return ret != null ? ret : cache(pred, pred.toConstraints().accept(this));
		}
		
		/**
		 * A container class that stores the translation of a shared node
		 * (BooleanValue for formulas and BooleanMatrix for expressions)
		 * and bindings for the node's free variables which were used to 
		 * generate the translation.
		 * Storing the bindings is necessary for proper handling of 
		 * sharing within quantified formulas and comprehensions.
		 * This implementation assumes that each free variable is 
		 * mapped to a BooleanMatrix of density one, whose sole entry
		 * is the BooleanConstant TRUE.
		 * @specfield varBinding: Variable -> lone int
		 * @specfield translation: lone Object
		 */
		private static abstract class TranslationInfo {
			Object translation;
			/**
			 * Returns this.translation if the given environment
			 * has the same mappings for the free variables of 
			 * the translated node as the ones used to generate
			 * this.translation.  Otherwise returns null.  
			 * @requires all v: varBinding.int | some e.lookup(v)
			 * @return all v: varBinding.int | e.lookup(v).get(varBinding[v])=TRUE => this.translation, null
			 * @throws NullPointerException - e = null
			 */
			abstract Object get(Environment<BooleanMatrix> e);
			
			/**
			 * Sets this.translation to the given translation
			 * and sets the free variable bindings to those 
			 * given by the specified environment.
			 * @requires all v: varBinding.int | some env.lookup(v)
			 * @effects this.translation' = translation && 
			 *          this.varBinding' = 
			 *           {v: this.varBinding.int, tupleIndex: int | 
			 *             tupleIndex = env.lookup(v).iterator().next().index() }
			 */
			abstract void set(Object transl, Environment<BooleanMatrix> env);
		}
		
		/**
		 * A TranslationInfo for a node with one or more free variables. 
		 */
		private static final class MultiVarTranslationInfo extends TranslationInfo {
			final Variable[] vars;
			final int[] tuples;
			
			/**
			 * Constructs a translation unit for a node which
			 * has the given set of free variables.
			 * @effects this.freeVariables' = vars &&
			 *          no this.translation' 
			 */
			MultiVarTranslationInfo(Set<Variable> freeVariables) {
				this.vars = freeVariables.toArray(new Variable[freeVariables.size()]);
				this.tuples = new int[freeVariables.size()];
			}
			
			@Override
			Object get(Environment<BooleanMatrix> e) {
				if (translation==null) return null;
				for(int i = 0; i < vars.length; i++) {
					if (e.lookup(vars[i]).get(tuples[i])!=BooleanConstant.TRUE)
						return null;
				}
				return translation;
			}
			
			@Override
			void set(Object transl, Environment<BooleanMatrix> env) {
				translation = transl;
				for(int i = 0; i < vars.length; i++) {
					tuples[i] = env.lookup(vars[i]).iterator().next().index();
				}
			}
		}
		
		/**
		 * A TranslationInfo for a node with no free variables. 
		 */
		private static final class NoVarTranslationInfo extends TranslationInfo {
			
			@Override
			Object get(Environment<BooleanMatrix> e) {
				return translation;
			}
			
			@Override
			void set(Object transl, Environment<BooleanMatrix> env) {
				translation = transl;
			}
			
		}		
	}
}
