package kodkod.engine.fol2sat;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;

import kodkod.ast.BinaryExpression;
import kodkod.ast.BinaryFormula;
import kodkod.ast.BinaryIntExpression;
import kodkod.ast.ComparisonFormula;
import kodkod.ast.Comprehension;
import kodkod.ast.ConstantExpression;
import kodkod.ast.ConstantFormula;
import kodkod.ast.Decl;
import kodkod.ast.Decls;
import kodkod.ast.ExprToIntCast;
import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.IfExpression;
import kodkod.ast.IfIntExpression;
import kodkod.ast.IntComparisonFormula;
import kodkod.ast.IntConstant;
import kodkod.ast.IntToExprCast;
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
import kodkod.engine.bool.BooleanAccumulator;
import kodkod.engine.bool.BooleanConstant;
import kodkod.engine.bool.BooleanFactory;
import kodkod.engine.bool.BooleanMatrix;
import kodkod.engine.bool.BooleanValue;
import kodkod.engine.bool.Dimensions;
import kodkod.engine.bool.Int;
import kodkod.engine.bool.Operator;
import kodkod.util.ints.IntIterator;
import kodkod.util.ints.IntSet;

/**
 * Translates a first order logic formula into a boolean circuit, 
 * and stores the translation and annotations computed by 
 * {@link FOL2BoolTranslator#translateAndTrack(AnnotatedNode, LeafInterpreter)}. 
 * @specfield node: AnnotatedNode<? extends Node> // the node being translated
 * @specfield interpreter: LeafInterpreter // the interpreter used for translation	 
 * @author Emina Torlak
 */
final class FOL2BoolTranslator {
	private final BooleanValue translation;
	private final Map<Node,IntSet> variableUsage;
	private final Set<Formula> trueFormulas, falseFormulas;
	
	private FOL2BoolTranslator(BooleanValue translation, Map<Node,IntSet> varUsage, 
            Set<Formula> trueFormulas, Set<Formula> falseFormulas) {
		this.translation = translation;
		this.variableUsage = Collections.unmodifiableMap(varUsage);
		this.trueFormulas = Collections.unmodifiableSet(trueFormulas);
		this.falseFormulas = Collections.unmodifiableSet(falseFormulas);
	}
	
	/**
	 * Translates the given annotated formula or expression into a boolean
	 * formula or matrix, using the provided interpreter and integer encoding.
	 * @requires interpreter.relations = AnnotatedNode.relations(annotated)
	 * @return {transl: T | 
	 *           annotated.node in Formula => transl in BooleanValue, 
	 *           annotated.node in Expression => transl in BooleanMatrix}
	 * @throws HigherOrderDeclException - annotated.node contains a higher order declaration
	 * @throws UnboundLeafException - annotated.node refers to an undeclared variable 
	 **/
	@SuppressWarnings("unchecked")
	static final <T> T translate(AnnotatedNode<? extends Node> annotated, LeafInterpreter interpreter) {
		return (T) annotated.node().accept(new Translator(new TranslationCache.Simple(annotated), interpreter));
	}
	
	/**
	 * Translates the given annotated formula into a boolean circuit using
	 * the provided interpreter and integer encoding.  Additionally, it 
	 * keeps track of which variables comprise the descendents of the formula,
	 * and which of its descendents are reduced to constants during translation.
	 * @requires interpreter.relations = AnnotatedNode.relations(annotated)
	 * @return {ret: FOL2BoolTranslator | ret.node = annotated && ret.manager = interpreter } 
	 * @throws HigherOrderDeclException - annotated.node contains a higher order declaration
	 * @throws UnboundLeafException - annotated.node refers to an undeclared variable 
	 */
	static final FOL2BoolTranslator translateAndTrack(AnnotatedNode<Formula> annotated,  LeafInterpreter interpreter) {
		final TranslationCache.Tracking c = new TranslationCache.Tracking(annotated);
		return new FOL2BoolTranslator(annotated.node().accept(new Translator(c, interpreter)), 
				            c.varUsage(), c.trueFormulas(), c.falseFormulas());
	}
	
	
	/**
	 * Returns the translation of this.node to a boolean circuit.
	 * @return the translation of this.node to a boolean circuit.
	 */
	BooleanValue translation() {
		return translation;
	}
	
	/**
	 * Returns a map from the descendents of node to the 
	 * literals representing the BooleanFormulas that comprise 
	 * the descendents' translations.  Descendents that evaluate 
	 * to BooleanConstants or BooleanMatrices comprised of BooleanConstants
	 * are not mapped.
	 * @return from the descendents of this.node to the 
	 * literals representing the BooleanValues that comprise 
	 * the descendents' translations.
	 */
	Map<Node, IntSet> variableUsage() {
		return variableUsage;
	}
	
	/**
	 * Returns the set of descendents of this.node that 
	 * evaluate to TRUE.
	 * @return {f: this.node.node.*children & Formula | translate(f,this.manager) = TRUE}
	 */
	Set<Formula> trueFormulas() {
		return trueFormulas;
	}
	
	/**
	 * Returns the set of descendents of this.node that 
	 * evaluate to FALSE.
	 * @return {f: this.node.node.*children & Formula | translate(f,this.manager) = FALSE}
	 */
	Set<Formula> falseFormulas() {
		return falseFormulas;
	}

	/**
	 * Translates a FOL node to boolean
	 * @specfield node: Node // the translated node
	 */
	private static class Translator implements ReturnVisitor<BooleanMatrix, BooleanValue, Object, Int> {
		final LeafInterpreter interpreter;

		
		/* When visiting the body of a quantified formula or a comprehension, this
		 * environment contains the current values of the enclosing quantified variable(s) */
		Environment<BooleanMatrix> env;
		
		final TranslationCache cache;
	
		/**
		 * Constructs a new translator that will use the given interpreter, annotated universe, and cache to perform the 
		 * translation.
		 * @effects this.node' = cache.node
		 */   
		Translator(TranslationCache cache,  LeafInterpreter interpreter) {
			this.interpreter = interpreter;
			this.env = new Environment<BooleanMatrix>();
			this.cache = cache;
		}
		
		/**
		 * Retrieves the cached translation for the given node, if any.
		 * Otherwise returns null.
		 * @return translation for the given node, if it has been recorded;
		 * null if not.
		 */
		<T> T lookup(Node node) {
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
		<T> T record(Node node, T translation) {
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
		BooleanMatrix record(Expression expr, BooleanMatrix translation) {
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
		BooleanValue record(Formula formula, BooleanValue translation) {
			return cache.cache(formula, translation, env);
		}
		
		/**
		 * @return a list of BooleanMatrices A such that A[i] = decls.declarations[i].expression.accept(this) 
		 */
		public final List<BooleanMatrix> visit(Decls decls) {
			List<BooleanMatrix> matrices = lookup(decls);
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
			BooleanMatrix matrix = lookup(decl);
			if (matrix!=null) return matrix;
			if (decl.multiplicity()!=Multiplicity.ONE)
				throw new HigherOrderDeclException(decl);
			return record(decl, decl.expression().accept(this));
		}
		
		/**
		 * @return a boolean matrix representing the current value of the given quantified variable
		 * @throws IllegalArgumentException - variable is undeclared
		 */
		public final BooleanMatrix visit(Variable variable) {
			final BooleanMatrix ret = env.lookup(variable);
			if (ret != null) return ret;
			else throw new UnboundLeafException("Unbound variable.", variable);
		}
		
		/**
		 * @return a matrix of freshly allocated variables representing the given relation
		 * @effects some instance.state[relation] => some interpreter.vars'[relation]
		 * @throws IllegalArgumentException - !this.instance.contains(relation)
		 */
		public final BooleanMatrix visit(Relation relation) {
			return interpreter.interpret(relation);
		}
		
		/**
		 * @return a matrix that represents the given constant expression;
		 * constExpr = Expression.UNIV => {m: BooleanMatrix | m.zero = TRUE && no m.elements},
		 * constExpr = Expression.IDEN => translate(UNIV).diagonal(),
		 * constExpr = Expression.NONE => UNIV.not()
		 */
		public final BooleanMatrix visit(ConstantExpression constExpr) {
			return interpreter.interpret(constExpr);
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
			BooleanMatrix ret = lookup(binExpr);
			if (ret!=null) return ret;

			final BooleanMatrix left = binExpr.left().accept(this);
			final BooleanMatrix right = binExpr.right().accept(this);
			final BinaryExpression.Operator op = binExpr.op();
			
			switch(op) {
			case UNION        	: ret = left.or(right); break;
			case INTERSECTION	: ret = left.and(right); break;
			case DIFFERENCE 		: ret = left.difference(right); break;
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
			BooleanMatrix ret = lookup(unaryExpr);
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
			BooleanMatrix ret = lookup(comprehension);
			if (ret!=null) return ret;
			
			/* Let comprehension = { a: A, b: B, ..., x: X | F(a, b, ..., x) }.  It
			 * is translated as { a: A, b: B, ..., x: X | a in A && b in B && ... && x in X && F(a, b, ..., x) }.
			 * Specifically, the appropriate cell in the returned matrix holds the formula
			 * A_i && B_j && ... && X_k && translate(F(A_i, B_j, ..., X_k)), where A_i etc.
			 * stand for boolean variables that represent the tuples of the expression A, etc. */
			final Decls decls = comprehension.declarations();
			final List<BooleanMatrix> declTransls = visit(decls);
			final GroundValueGenerator generator = new GroundValueGenerator(env, decls, declTransls);
			final BooleanFactory factory = interpreter.factory();
			
			Dimensions dims = declTransls.get(0).dimensions();
			for (int i = 1; i < declTransls.size(); i++) { 
				dims = dims.cross(declTransls.get(i).dimensions());
			}
			
			ret = factory.matrix(dims);
			
			while(generator.hasNext()) {
				env = generator.next(factory);
				
				int[] index = generator.index();
				BooleanAccumulator conjunct = BooleanAccumulator.treeGate(Operator.AND);
				// A_index[0] && B_index[1] && ... && X_index[index.length-1]
				for(int i = 0; i < index.length; i++) {
					conjunct.add(declTransls.get(i).get(index[i]));
				}
				// A_index[0] && B_index[1] && ... && X_index[index.length-1] &&
				//   translate(F(A_index[0], B_index[1], ..., X_index[index.length-1]))
				conjunct.add(comprehension.formula().accept(this));
				
				ret.set(dims.convert(index), factory.accumulate(conjunct)); 
			}	
			env = generator.baseEnvironment();
			
			return record(comprehension,ret);
		}
		
		/**
		 * @return translate(ifExpr.condition) => translate(ifExpr.then), translate(ifExpr.else)
		 */
		public final BooleanMatrix visit(IfExpression ifExpr) {
			BooleanMatrix ret = lookup(ifExpr);
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
			final BooleanAccumulator conjunct = BooleanAccumulator.treeGate(Operator.AND);
			
			final Decls decls = qf.declarations();
			final List<BooleanMatrix> declTransls = visit(decls);
			final GroundValueGenerator generator = new GroundValueGenerator(env, decls, declTransls);
			final BooleanFactory factory = interpreter.factory();
			
			while(generator.hasNext() && !conjunct.isShortCircuited()) {
				env = generator.next(factory);
				
				int index[] = generator.index();
				BooleanAccumulator disjunct = BooleanAccumulator.treeGate(Operator.OR);
				// !A_index[0] || !B_index[1] || ... || !X_index[index.length-1]
				for(int i = 0; i < index.length; i++) {
					disjunct.add(factory.not(declTransls.get(i).get(index[i])));
				}
				// !A_index[0] || !B_index[1] || ... || !X_index[index.length-1] ||
				//   translate(F(A_index[0], B_index[1], ..., X_index[index.length-1]))
				disjunct.add(qf.formula().accept(this));
				
				conjunct.add(factory.accumulate(disjunct));				
			}
			env = generator.baseEnvironment();
			
			return factory.accumulate(conjunct);
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
			final BooleanAccumulator disjunct = BooleanAccumulator.treeGate(Operator.OR);
			
			final Decls decls = qf.declarations();
			final List<BooleanMatrix> declTransls = visit(decls);
			final GroundValueGenerator generator = new GroundValueGenerator(env, decls, declTransls);
			final BooleanFactory factory = interpreter.factory();
			
			while(generator.hasNext() && !disjunct.isShortCircuited()) {
				env = generator.next(factory);
				
				int index[] = generator.index();
				BooleanAccumulator conjunct = BooleanAccumulator.treeGate(Operator.AND);
				// A_index[0] && B_index[1] && ... && X_index[index.length-1]
				for(int i = 0; i < index.length; i++) {
					conjunct.add(declTransls.get(i).get(index[i]));
				}
				// A_index[0] && B_index[1] && ... && X_index[index.length-1] &&
				//   translate(F(A_index[0], B_index[1], ..., X_index[index.length-1]))
				conjunct.add(qf.formula().accept(this));
				
				disjunct.add(factory.accumulate(conjunct));	
			}
			env = generator.baseEnvironment();
			
			return factory.accumulate(disjunct);
		}
		
		/** 
		 * @return let quantFormula = "quant a: A, b: B, ..., x: X | F(a, b, ..., x)" |
		 *           quant = ALL  => translate(F(A_0, B_0, ..., X_0)) && ... && translate(F(A_|A|, B_|B|, ..., X_|X|))
		 *           quant = SOME => translate(F(A_0, B_0, ..., X_0)) || ... || translate(F(A_|A|, B_|B|, ..., X_|X|))
		 */
		public final BooleanValue visit(QuantifiedFormula quantFormula) {
			BooleanValue ret = lookup(quantFormula);
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
			BooleanValue ret = lookup(binFormula);
			if (ret!=null) return ret;
			
			final BooleanValue left = binFormula.left().accept(this);
			final BooleanValue right = binFormula.right().accept(this);
			final BinaryFormula.Operator op = binFormula.op();
			final BooleanFactory f = interpreter.factory();
			
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
			BooleanValue ret = lookup(not);
			return ret==null ? 
				   record(not, interpreter.factory().not(not.formula().accept(this))) : ret;
		}
		
		/**
		 * @return let tLeft = translate(compFormula.left), tRight = translate(compFormula.right) | 
		 *          compFormula.op = SUBSET => 
		 *           tLeft.subset(tRight), tleft.eq(tRight)          
		 */
		public final BooleanValue visit(ComparisonFormula compFormula) {
			BooleanValue ret = lookup(compFormula);
			if (ret!=null) return ret;
			
			final BooleanMatrix left = compFormula.left().accept(this);
			final BooleanMatrix right = compFormula.right().accept(this);
			final ComparisonFormula.Operator op = compFormula.op();
			
			switch(op) {
			case SUBSET	: ret = left.subset(right); break;
			case EQUALS	: ret = left.eq(right); break;
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
			BooleanValue ret = lookup(multFormula);
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
			BooleanValue ret = lookup(pred);
			return ret != null ? ret : record(pred, pred.toConstraints().accept(this));
		}

		/**
		 * @return boolean matrix representing a singleton set containing the
		 * atom that represents the castExpr.intExpr.
		 */
		public BooleanMatrix visit(IntToExprCast castExpr) {
			BooleanMatrix ret = lookup(castExpr);
			if (ret!=null) return ret;
			
			final Int child = castExpr.intExpr().accept(this);
			final BooleanFactory factory =  interpreter.factory();
			ret = factory.matrix(Dimensions.square(1, interpreter.universe().size()));
			for(IntIterator iter = interpreter.ints().iterator(); iter.hasNext(); ) {
				int i = iter.nextInt();
				int atomIndex = interpreter.interpret(i);
				ret.set(atomIndex, factory.or(ret.get(atomIndex), child.eq(factory.integer(i))));
			}
			
			return record(castExpr, ret);
		}	
		
		/**
		 * @return factory.integer(intConst.value, this.encoding)
		 */
		public Int visit(IntConstant intConst) {
			return interpreter.factory().integer(intConst.value());
		}

		/**
		 * @return translate(intExpr.condition) => translate(intExpr.then), translate(intExpr.else)
		 */
		public Int visit(IfIntExpression intExpr) { 
			Int ret = lookup(intExpr);
			if (ret!=null) return ret;
			
			final BooleanValue condition = intExpr.condition().accept(this);
			final Int thenExpr = intExpr.thenExpr().accept(this);
			final Int elseExpr = intExpr.elseExpr().accept(this);
			ret = thenExpr.choice(condition, elseExpr);
			
			return record(intExpr, ret);
		}
		
		/**
		 * Returns an Int that represents the sum of all the integers that
		 * correspond to non-FALSE entries in the given matrix.
		 * @param iter an iterator over all the bound integers.  Initial should be this.manager.ints().iterator().
		 * @param lo the first element of the current partial sum. Initial should be 0.
		 * @param hi the last element of the current partial sum.  Initial should be size-1, where size is the total
		 * number of elements returned by the iterator.
		 * @return  an Int that represents the sum of all the integers that
		 * correspond to non-FALSE entries in the given matrix.
		 */
		private Int sum(BooleanMatrix m, IntIterator iter, int low, int high) {
			if (low > high)
				return interpreter.factory().integer(0);
			else if (low==high) {
				int i = iter.nextInt();
				return interpreter.factory().integer(i, m.get(interpreter.interpret(i)));
			} else {
				final int mid = (low + high) / 2;
				final Int lsum = sum(m, iter, low, mid);
				final Int hsum = sum(m, iter, mid+1, high);
				return lsum.plus(hsum);
			}
		}
		
		/**
		 * @return translate(intExpr.expression).cardinality()
		 */
		public Int visit(ExprToIntCast intExpr) {
			Int ret = lookup(intExpr);
			if (ret!=null) return ret;
			switch(intExpr.op()) {
			case CARDINALITY : 
				ret = intExpr.expression().accept(this).cardinality(); break;
			case SUM         :
				final IntSet ints = interpreter.ints();
				ret = sum(intExpr.expression().accept(this), ints.iterator(), 0, ints.size()-1); break;
			default: 
				throw new IllegalArgumentException("unknown operator: " + intExpr.op());
			}
			return record(intExpr, ret);
		}

		/**
		 * @return translate(intExpr.left) intExpr.op translate(intExpr.right)
		 */
		public Int visit(BinaryIntExpression intExpr) {
			Int ret = lookup(intExpr);
			if (ret!=null) return ret;
			final Int left = intExpr.left().accept(this);
			final Int right = intExpr.right().accept(this);
			switch(intExpr.op()) {
			case PLUS  		: ret = left.plus(right); break;
			case MINUS 		: ret = left.minus(right); break;
			case MULTIPLY 	: ret = left.multiply(right); break;
			case DIVIDE 		: ret = left.divide(right); break;
			default    :
				throw new IllegalArgumentException("Unknown operator: " + intExpr.op());
			}
			return record(intExpr, ret);
		}
		
		/**
		 * @return translate(intComp.left) intComp.op translate(intComp.right)
		 */
		public BooleanValue visit(IntComparisonFormula intComp) {
			BooleanValue ret = lookup(intComp);
			if (ret!=null) return ret;
			final Int left = intComp.left().accept(this);
			final Int right = intComp.right().accept(this);
			switch(intComp.op()) {
			case EQ  : ret = left.eq(right); break;
			case LT  : ret = left.lt(right); break;
			case LTE : ret = left.lte(right); break;
			case GT  : ret = left.gt(right); break;
			case GTE : ret = left.gte(right); break;
			default: 
				throw new IllegalArgumentException("Unknown operator: " + intComp.op());
			}
			return record(intComp, ret);
		}
		
	}
}
