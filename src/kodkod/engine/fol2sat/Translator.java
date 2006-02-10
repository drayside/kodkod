/*
 * Translator.java
 * Created on Jul 5, 2005
 */
package kodkod.engine.fol2sat;

import static kodkod.engine.bool.BooleanConstant.FALSE;
import static kodkod.engine.bool.BooleanConstant.TRUE;
import static kodkod.engine.bool.MultiGate.Operator.AND;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
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
import kodkod.engine.bool.BooleanVariable;
import kodkod.engine.bool.BooleanVisitor;
import kodkod.engine.bool.Dimensions;
import kodkod.engine.bool.MultiGate;
import kodkod.engine.bool.MutableMultiGate;
import kodkod.engine.bool.NotGate;
import kodkod.engine.bool.MultiGate.Operator;
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
		BooleanValue sat = formula.accept(new Fol2Sat(allocator, formula, notes.sharedNodes()));
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
		return (T) node.accept(new Fol2Sat(allocator, node, NodeAnalyzer.detectSharing(node)));
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
	 * Performs translation from FOL to SAT.
	 */
	private static final class Fol2Sat implements ReturnVisitor<BooleanMatrix, BooleanValue, Object> {
		
		private final BooleanFormulaAllocator allocator;
		
		/* The size of the universal relation */
		private final int univSize;
		
		/* When visiting the body of a quantified formula or a comprehension, this
		 * environment contains the current values of the enclosing quantified variable(s) */
		private Environment<BooleanMatrix> env;
		
		private final TranslationCache cache;
		
		/**
		 * Constructs a new translator that will use the given allocator to perform the 
		 * translation of the specified node.  The set sharedInternalNodes is used to 
		 * determine which translations should be cached.
		 * @requires sharedInternalNodes = {n: Node | #(n.~children & node.*children) > 1 } 
		 */    
		Fol2Sat(final BooleanFormulaAllocator allocator, Node node, Set<Node> sharedInternalNodes) {
			if (allocator==null) throw new NullPointerException("allocator==null");
			this.allocator = allocator;
			this.env = new Environment<BooleanMatrix>();
			this.cache = new TranslationCache(node, sharedInternalNodes);
			this.univSize = allocator.universe().size();
		}
	
		/**
		 * @return a list of BooleanMatrices A such that A[i] = decls.declarations[i].expression.accept(this) 
		 */
		public List<BooleanMatrix> visit(Decls decls) {
			List<BooleanMatrix> matrices = cache.get(decls, env);
			if (matrices!=null) return matrices;
			
			final List<Decl> dlist = decls.declarations();
			matrices = new ArrayList<BooleanMatrix>(dlist.size());
			for (Decl decl : dlist) {
				matrices.add(visit(decl));
			}
			
			return cache.cache(decls, matrices, env);
		}
		
		/**
		 * @return the BooleanMatrix that is the translation of decl.expression.
		 */
		public BooleanMatrix visit(Decl decl) {
			BooleanMatrix matrix = cache.get(decl, env);
			return matrix==null ? cache.cache(decl, decl.expression().accept(this), env) : matrix;
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
			BooleanMatrix ret = cache.get(binExpr, env);
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
			
			return cache.cache(binExpr, ret, env);
		}
		/**
		 * @return let tChild = translate(unaryExpr.child) |
		 *           unaryExpr.op = TRANSPOSE => tChild.transpose(),
		 *           unaryExpr.op = TRANSITIVE_CLOSURE => tChild.closure() 
		 */
		public BooleanMatrix visit(UnaryExpression unaryExpr) {
			BooleanMatrix ret = cache.get(unaryExpr, env);
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
			return cache.cache(unaryExpr,ret, env);
		}
		
		/**
		 * @return { translate(comprehension.declarations) | translate(comprehension.formula) }
		 */
		public BooleanMatrix visit(Comprehension comprehension) {
			BooleanMatrix ret = cache.get(comprehension, env);
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
			
			return cache.cache(comprehension,ret, env);
		}
		
		/**
		 * @return translate(ifExpr.condition) => translate(ifExpr.then), translate(ifExpr.else)
		 */
		public BooleanMatrix visit(IfExpression ifExpr) {
			BooleanMatrix ret = cache.get(ifExpr, env);
			if (ret!=null) return ret;
			
			final BooleanValue condition = ifExpr.condition().accept(this);
			final BooleanMatrix thenExpr = ifExpr.thenExpr().accept(this);
			final BooleanMatrix elseExpr = ifExpr.elseExpr().accept(this);
			ret = thenExpr.choice(condition, elseExpr);
			
			return cache.cache(ifExpr,ret, env);
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
			BooleanValue ret = cache.get(quantFormula, env);
			if (ret!=null) return ret;

			final QuantifiedFormula.Quantifier quantifier = quantFormula.quantifier();
			
			switch(quantifier) {
			case ALL		: ret = visitUniversalFormula(quantFormula); break;
			case SOME	: ret = visitExistentialFormula(quantFormula); break;
			default :
				throw new IllegalArgumentException("Unknown quantifier: " + quantifier);
			}

			return cache.cache(quantFormula,ret, env);
		}
		
		/**
		 * @return let tLeft = translate(binFormula.left), tRight = translate(binFormula.right) | 
		 *           binFormula.op = AND => tLeft.and(tRight),
		 *           binFormula.op = OR => tLeft.or(tRight),
		 *           binFormula.op = IMPLIES => tLeft.not().or(tRight),
		 *           binFormula.op = IFF => tLeft.not().or(tRight).and(tRight.not().or(tLeft)),
		 */
		public BooleanValue visit(BinaryFormula binFormula) {
			BooleanValue ret = cache.get(binFormula, env);
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
		
			return cache.cache(binFormula, ret, env);
		}
		
		/**
		 * @return translate(not.child).not()
		 */    
		public BooleanValue visit(NotFormula not) {
			BooleanValue ret = cache.get(not, env);
			return ret==null ? 
					cache.cache(not, allocator.factory().not(not.formula().accept(this)), env) : ret;
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
			BooleanValue ret = cache.get(compFormula, env);
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
	
			return cache.cache(compFormula,ret, env);
		}
		
		/**
		 * @return let child = translate(multFormula.expression) | 
		 *           multFormula.multiplicity = NO => child.none(),
		 *           multFormula.multiplicity = SOME => child.some(),
		 *           multFormula.multiplicity = ONE => child.one(),
		 *           multFormula.multiplicity = LONE => child.lone()
		 */
		public BooleanValue visit(MultiplicityFormula multFormula) {
			BooleanValue ret = cache.get(multFormula, env);
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
			
			return cache.cache(multFormula, ret, env);
		}
		
		/**
		 * @return translate(pred.toConstraints())
		 */
		public BooleanValue visit(RelationPredicate pred) {
			BooleanValue ret = cache.get(pred, env);
			return ret != null ? ret : cache.cache(pred, pred.toConstraints().accept(this), env);
		}
	}
	
	/**
	 * Performs translation from SAT to CNF.
	 */
	private static final class Sat2Cnf implements BooleanVisitor<int[], Object> {
		private final SATSolver solver;
		private final IntSet visited;
		private final int[] unaryClause = new int[1];
				
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
