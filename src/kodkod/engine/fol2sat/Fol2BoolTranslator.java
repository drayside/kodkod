package kodkod.engine.fol2sat;

import static kodkod.engine.bool.BooleanConstant.FALSE;
import static kodkod.engine.bool.BooleanConstant.TRUE;

import java.util.ArrayList;
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
import kodkod.engine.bool.BooleanConstant;
import kodkod.engine.bool.BooleanFactory;
import kodkod.engine.bool.BooleanMatrix;
import kodkod.engine.bool.BooleanValue;
import kodkod.engine.bool.Dimensions;
import kodkod.engine.bool.MutableMultiGate;
import kodkod.engine.bool.MultiGate.Operator;

/**
 * Translates a first order logic formula into a boolean circuit.
 * 
 * @author Emina Torlak
 */
final class Fol2BoolTranslator {

	private Fol2BoolTranslator() {}
	
	/**
	 * Translates the given first order formula or expression into a boolean
	 * formula or matrix, using the provided allocator and structural information.
	 * @requires sharedInternalNodes = {n: Node | #(n.~children & node.*children) > 1 } 
	 * @requires allocator.relations = n.*children & Relation
	 * @return {transl: T | 
	 *           node in Formula => transl in BooleanValue, 
	 *           node in Expression => transl in BooleanMatrix}
	 */
	@SuppressWarnings("unchecked")
	static final <T> T translate(Node node, Set<Node> sharedInternalNodes, BooleanFormulaAllocator allocator) {
		return (T) node.accept(new Translator(allocator, node, sharedInternalNodes));
	}
	
	/**
	 * The helper class that actually performs the translation.  This
	 * version of the translator does not track variables.
	 */
	private static class Translator implements ReturnVisitor<BooleanMatrix, BooleanValue, Object> {
		
		final BooleanFormulaAllocator allocator;
		
		/* When visiting the body of a quantified formula or a comprehension, this
		 * environment contains the current values of the enclosing quantified variable(s) */
		Environment<BooleanMatrix> env;
		
		final TranslationCache cache;
		
		/**
		 * Constructs a new translator that will use the given allocator to perform the 
		 * translation of the specified node.  The set sharedInternalNodes is used to 
		 * determine which translations should be cached.
		 * @requires sharedInternalNodes = {n: Node | #(n.~children & node.*children) > 1 } 
		 */    
		Translator(final BooleanFormulaAllocator allocator, Node node, Set<Node> sharedInternalNodes) {
			if (allocator==null) throw new NullPointerException("allocator==null");
			this.allocator = allocator;
			this.env = new Environment<BooleanMatrix>();
			this.cache = new TranslationCache(node, sharedInternalNodes);
		}
	
		/**
		 * Retrieves the cached translation for the given node, if any.
		 * Otherwise returns null.
		 * @return translation for the given node, if it has been recorded;
		 * null if not.
		 */
		protected <T> T retrieve(Node node) {
			return cache.get(node, env);
		}
		
		/**
		 * If the given node is one for which we are caching translations,
		 * the provided translation is cached and returned.  Otherwise,
		 * the translation is simply returned.
		 * @return translation
		 * @effects if the node is one for which we are caching translations,
		 * the provided translation is cached.
		 */
		protected <T> T record(Node node, T translation) {
			return cache.cache(node, translation, env);
		}
		
		/**
		 * If the given expression is one for which we are caching translations,
		 * the provided translation is cached and returned.  Otherwise,
		 * the translation is simply returned. 
		 * @return translation
		 * @effects if the expression is one for which we are caching translations,
		 * the provided translation is cached.
		 */
		protected BooleanMatrix record(Expression expr, BooleanMatrix translation) {
			return cache.cache(expr, translation, env);
		}
		
		/**
		 * If the given formula is one for which we are caching translations,
		 * the provided translation is cached and returned.  Otherwise,
		 * the translation is simply returned. 
		 * @return translation
		 * @effects if the formula is one for which we are caching translations,
		 * the provided translation is cached.
		 */
		protected BooleanValue record(Formula formula, BooleanValue translation) {
			return cache.cache(formula, translation, env);
		}
		
		/**
		 * @return a list of BooleanMatrices A such that A[i] = decls.declarations[i].expression.accept(this) 
		 */
		public final List<BooleanMatrix> visit(Decls decls) {
			List<BooleanMatrix> matrices = retrieve(decls);
			if (matrices!=null) return matrices;
			
			final List<Decl> dlist = decls.declarations();
			matrices = new ArrayList<BooleanMatrix>(dlist.size());
			for (Decl decl : dlist) {
				matrices.add(visit(decl));
			}
			
			return record(decls, matrices);
		}
		
		/**
		 * @return the BooleanMatrix that is the translation of decl.expression.
		 */
		public final BooleanMatrix visit(Decl decl) {
			BooleanMatrix matrix = retrieve(decl);
			return matrix==null ? record(decl, decl.expression().accept(this)) : matrix;
		}
		
		/**
		 * @return a boolean matrix representing the current value of the given quantified variable
		 * @throws IllegalArgumentException - variable is undeclared
		 */
		public final BooleanMatrix visit(Variable variable) {
			final BooleanMatrix ret = env.lookup(variable);
			if (ret != null) return ret;
			else throw new IllegalArgumentException("unbound variable " + variable);
		}
		
		/**
		 * @return a matrix of freshly allocated variables representing the given relation
		 * @effects some instance.state[relation] => some allocator.vars'[relation]
		 * @throws IllegalArgumentException - !this.instance.contains(relation)
		 */
		public final BooleanMatrix visit(Relation relation) {
			return allocator.allocate(relation);
		}
		
		/**
		 * @return a matrix that represents the given constant expression;
		 * constExpr = Expression.UNIV => {m: BooleanMatrix | m.zero = TRUE && no m.elements},
		 * constExpr = Expression.IDEN => translate(UNIV).diagonal(),
		 * constExpr = Expression.NONE => UNIV.not()
		 */
		public final BooleanMatrix visit(ConstantExpression constExpr) {
			final BooleanMatrix ret;
			final int univSize = allocator.universe().size();
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
		public final BooleanMatrix visit(BinaryExpression binExpr) {
			BooleanMatrix ret = retrieve(binExpr);
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
			
			return record(binExpr, ret);
		}
		/**
		 * @return let tChild = translate(unaryExpr.child) |
		 *           unaryExpr.op = TRANSPOSE => tChild.transpose(),
		 *           unaryExpr.op = TRANSITIVE_CLOSURE => tChild.closure() 
		 */
		public final BooleanMatrix visit(UnaryExpression unaryExpr) {
			BooleanMatrix ret = retrieve(unaryExpr);
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
			return record(unaryExpr,ret);
		}
		
		/**
		 * @return { translate(comprehension.declarations) | translate(comprehension.formula) }
		 */
		public final BooleanMatrix visit(Comprehension comprehension) {
			BooleanMatrix ret = retrieve(comprehension);
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
			
			return record(comprehension,ret);
		}
		
		/**
		 * @return translate(ifExpr.condition) => translate(ifExpr.then), translate(ifExpr.else)
		 */
		public final BooleanMatrix visit(IfExpression ifExpr) {
			BooleanMatrix ret = retrieve(ifExpr);
			if (ret!=null) return ret;
			
			final BooleanValue condition = ifExpr.condition().accept(this);
			final BooleanMatrix thenExpr = ifExpr.thenExpr().accept(this);
			final BooleanMatrix elseExpr = ifExpr.elseExpr().accept(this);
			ret = thenExpr.choice(condition, elseExpr);
			
			return record(ifExpr,ret);
		}
		
		/**
		 * @return constant = ConstantFormula.TRUE => BooleanConstant.TRUE, BooleanConstant.FALSE
		 */
		public final BooleanValue visit(ConstantFormula constant) {
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
		public final BooleanValue visit(QuantifiedFormula quantFormula) {
			BooleanValue ret = retrieve(quantFormula);
			if (ret!=null) return ret;

			final QuantifiedFormula.Quantifier quantifier = quantFormula.quantifier();
			
			switch(quantifier) {
			case ALL		: ret = visitUniversalFormula(quantFormula); break;
			case SOME	: ret = visitExistentialFormula(quantFormula); break;
			default :
				throw new IllegalArgumentException("Unknown quantifier: " + quantifier);
			}

			return record(quantFormula,ret);
		}
		
		/**
		 * @return let tLeft = translate(binFormula.left), tRight = translate(binFormula.right) | 
		 *           binFormula.op = AND => tLeft.and(tRight),
		 *           binFormula.op = OR => tLeft.or(tRight),
		 *           binFormula.op = IMPLIES => tLeft.not().or(tRight),
		 *           binFormula.op = IFF => tLeft.not().or(tRight).and(tRight.not().or(tLeft)),
		 */
		public final BooleanValue visit(BinaryFormula binFormula) {
			BooleanValue ret = retrieve(binFormula);
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
		
			return record(binFormula, ret);
		}
		
		/**
		 * @return translate(not.child).not()
		 */    
		public final BooleanValue visit(NotFormula not) {
			BooleanValue ret = retrieve(not);
			return ret==null ? 
				   record(not, allocator.factory().not(not.formula().accept(this))) : ret;
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
		public final BooleanValue visit(ComparisonFormula compFormula) {
			BooleanValue ret = retrieve(compFormula);
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
	
			return record(compFormula,ret);
		}
		
		/**
		 * @return let child = translate(multFormula.expression) | 
		 *           multFormula.multiplicity = NO => child.none(),
		 *           multFormula.multiplicity = SOME => child.some(),
		 *           multFormula.multiplicity = ONE => child.one(),
		 *           multFormula.multiplicity = LONE => child.lone()
		 */
		public final BooleanValue visit(MultiplicityFormula multFormula) {
			BooleanValue ret = retrieve(multFormula);
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
			
			return record(multFormula, ret);
		}
		
		/**
		 * @return translate(pred.toConstraints())
		 */
		public final BooleanValue visit(RelationPredicate pred) {
			BooleanValue ret = retrieve(pred);
			return ret != null ? ret : record(pred, pred.toConstraints().accept(this));
		}
	}


}
