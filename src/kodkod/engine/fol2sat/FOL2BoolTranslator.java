package kodkod.engine.fol2sat;

import java.util.List;

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
import kodkod.ast.IntExpression;
import kodkod.ast.IntToExprCast;
import kodkod.ast.Multiplicity;
import kodkod.ast.MultiplicityFormula;
import kodkod.ast.Node;
import kodkod.ast.NotFormula;
import kodkod.ast.ProjectExpression;
import kodkod.ast.QuantifiedFormula;
import kodkod.ast.Relation;
import kodkod.ast.RelationPredicate;
import kodkod.ast.SumExpression;
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
import kodkod.util.ints.IndexedEntry;
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
	
	/**
	 * Translates the given annotated formula or expression into a boolean
	 * formula or matrix, using the provided interpreter g.
	 * @requires interpreter.relations = AnnotatedNode.relations(annotated)
	 * @return {transl: T | 
	 *           annotated.node in Formula => transl in BooleanValue, 
	 *           annotated.node in Expression => transl in BooleanMatrix}
	 * @throws HigherOrderDeclException - annotated.node contains a higher order declaration
	 * @throws UnboundLeafException - annotated.node refers to an undeclared variable 
	 **/
	@SuppressWarnings("unchecked")
	static final <T> T translate(AnnotatedNode<? extends Node> annotated, LeafInterpreter interpreter) {
		return (T) annotated.node().accept(new Translator(new TranslationCache(annotated), interpreter));
	}
	
	/**
	 * Translates the given annotated formula or expression into a boolean
	 * formula or matrix, using the provided interpreter and environment.
	 * @requires interpreter.relations = AnnotatedNode.relations(annotated)
	 * @return {transl: T | 
	 *           annotated.node in Formula => transl in BooleanValue, 
	 *           annotated.node in Expression => transl in BooleanMatrix}
	 * @throws HigherOrderDeclException - annotated.node contains a higher order declaration
	 * @throws UnboundLeafException - annotated.node refers to a variable that neither declared
	 * nor bound in env
	 **/
	@SuppressWarnings("unchecked")
	static final <T> T translate(AnnotatedNode<? extends Node> annotated, LeafInterpreter interpreter, Environment<BooleanMatrix> env) {
		return (T) annotated.node().accept(new Translator(new TranslationCache(annotated), interpreter, env));
	}
	
	/**
	 * Translates the given annotated formula or expression into a boolean
	 * formula or matrix, using the provided interpreter, translation cache, and environment.
	 * @requires interpreter.relations = annotated.relations
	 * @requires cache.node = annotated.node
	 * @return {transl: T | 
	 *           annotated.node in Formula => transl in BooleanValue, 
	 *           annotated.node in Expression => transl in BooleanMatrix}
	 * @throws HigherOrderDeclException - annotated.node contains a higher order declaration
	 * @throws UnboundLeafException - annotated.node refers to a variable that neither declared
	 * nor bound in env
	 **/
	@SuppressWarnings("unchecked")
	static final <T> T translate(AnnotatedNode<? extends Node> annotated, LeafInterpreter interpreter, TranslationCache cache, Environment<BooleanMatrix> env) {
		return (T) annotated.node().accept(new Translator(cache, interpreter, env));
	}

	/**
	 * Translates a FOL node to boolean
	 * @specfield node: Node // the translated node
	 */
	private static final class Translator implements ReturnVisitor<BooleanMatrix, BooleanValue, Object, Int> {
		final LeafInterpreter interpreter;

		
		/* When visiting the body of a quantified formula or a comprehension, this
		 * environment contains the current values of the enclosing quantified variable(s) */
		Environment<BooleanMatrix> env;
		
		final TranslationCache cache;
	
		/**
		 * Constructs a new translator that will use the given translation cache
		 * and interpreter to perform the translation.
		 * @effects this.node' = cache.node
		 */   
		Translator(TranslationCache cache,  LeafInterpreter interpreter) {
			this.interpreter = interpreter;
			this.env = Environment.empty();
			this.cache = cache;
		}
		
		/**
		 * Constructs a new translator that will use the given translation cache,
		 * interpreter and environment to perform the translation.
		 * @effects this.node' = cache.node
		 */   
		Translator(TranslationCache cache,  LeafInterpreter interpreter, Environment<BooleanMatrix> env) {
			this.interpreter = interpreter;
			this.env = env;
			this.cache = cache;
		}
		
		/**
		 * Retrieves the cached translation for the given node, if any.
		 * Otherwise returns null.
		 * @return translation for the given node, if it has been recorded;
		 * null if not.
		 */
		@SuppressWarnings("unchecked") <T> T lookup(Node node) {
			return (T) cache.lookup(node, env);
		}
		
		/**
		 * If we are caching translations for the given decl,
		 * the provided translation is cached and returned.  Otherwise,
		 * the translation is simply returned. 
		 * @return translation
		 * @effects if we are caching translations for the given decl,
		 * the provided translation is cached.
		 */
		BooleanMatrix cache(Decl decl, BooleanMatrix translation) {
			return cache.cache(decl, translation, env);
		}
		
		/**
		 * If the given expression is one for which we are caching translations,
		 * the provided translation is cached and returned.  Otherwise,
		 * the translation is simply returned. 
		 * @return translation
		 * @effects if the expression is one for which we are caching translations,
		 * the provided translation is cached.
		 */
		BooleanMatrix cache(Expression expr, BooleanMatrix translation) {
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
		BooleanValue cache(Formula formula, BooleanValue translation) {
			return cache.cache(formula, translation, env);
		}
		
		/**
		 * If the given int expression is one for which we are caching translations,
		 * the provided translation is cached and returned.  Otherwise,
		 * the translation is simply returned. 
		 * @return translation
		 * @effects if the int expression is one for which we are caching translations,
		 * the provided translation is cached.
		 */
		Int cache(IntExpression intexpr, Int translation) {
			return cache.cache(intexpr, translation, env);
		}
		
		/**
		 * Never gets called.
		 * @throws InternalError
		 */
		public final List<BooleanMatrix> visit(Decls decls) {
			throw new InternalError("Please contact emina@mit.edu");
		}
		
		/**
		 * @return the BooleanMatrix that is the translation of decl.expression.
		 */
		public final BooleanMatrix visit(Decl decl) {
			BooleanMatrix matrix = lookup(decl);
			if (matrix!=null) return matrix;
			if (decl.multiplicity()!=Multiplicity.ONE)
				throw new HigherOrderDeclException(decl);
			return cache(decl, decl.expression().accept(this));
		}
		
		/**
		 * @return a boolean matrix representing the current value of the given quantified variable
		 * @throws IllegalArgumentException - variable is undeclared
		 */
		public final BooleanMatrix visit(Variable variable) {
			final BooleanMatrix ret = env.lookup(variable);
			if (ret != null) return ret;
			else throw new UnboundLeafException("Unbound variable", variable);
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
			case DIFFERENCE 	: ret = left.difference(right); break;
			case OVERRIDE 		: ret = left.override(right); break;
			case JOIN 			: ret = left.dot(right); break;
			case PRODUCT		: ret = left.cross(right); break;
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
			return cache(unaryExpr,ret);
		}
		
		/**
		 * Translates the given comprehension as follows 
		 * (where A_0...A_|A| stand for boolean variables that represent the 
		 * tuples of the expression A, etc.):
		 * let comprehension = "{ a: A, b: B, ..., x: X | F(a, b, ..., x) }" |
		 *     { a: A, b: B, ..., x: X | a in A && b in B && ... && x in X && F(a, b, ..., x) }.
		 * @param decls list of formula declarations; should be quantifiedFormula.declarations initially
		 * @param formula the body of the comprehension
		 * @param declConstraints the constraints implied by the declarations; should be Boolean.TRUE intially
		 * @param partialIndex partial index into the provided matrix; should be 0 initially
		 * @param matrix boolean matrix that will retain the final results; should be an empty matrix of dimensions universe.size^decls.length initially
		 * @effects the given matrix contains the translation of the comprehension "{ decls | formula }"
		 */
		private final void comprehension(List<Decl> decls, Formula formula, 
				BooleanValue declConstraints, int partialIndex, BooleanMatrix matrix) {
			final BooleanFactory factory = interpreter.factory();
			
			if (decls.isEmpty()) {
				matrix.set(partialIndex, factory.and(declConstraints, formula.accept(this)));
				return;
			}
			
			final Decl decl = decls.get(0);
			final BooleanMatrix declTransl = visit(decl);
			final List<Decl> others = decls.subList(1, decls.size());
			final int position = (int)StrictMath.pow(interpreter.universe().size(), decls.size()-1);
			final BooleanMatrix groundValue = factory.matrix(declTransl.dimensions());
			env = env.extend(decl.variable(), groundValue);
			for(IndexedEntry<BooleanValue> entry : declTransl) {
				groundValue.set(entry.index(), BooleanConstant.TRUE);
			    comprehension(others, formula, factory.and(entry.value(), declConstraints), 
			    		partialIndex + entry.index()*position, matrix);
                groundValue.set(entry.index(), BooleanConstant.FALSE);	
			}
			env = env.parent();
		}
		
		/**
		 * @return { translate(cexpr.declarations) | translate(cexpr.formula) }
		 */
		public final BooleanMatrix visit(Comprehension cexpr) {
			BooleanMatrix ret = lookup(cexpr);
			if (ret!=null) return ret;
			
			ret = interpreter.factory().matrix(Dimensions.square(interpreter.universe().size(), cexpr.declarations().size()));
			comprehension(cexpr.declarations().declarations(), cexpr.formula(), BooleanConstant.TRUE, 0, ret);
			
			return cache(cexpr,ret);
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
			
			return cache(ifExpr,ret);
		}
		
		/**
		 * @return translate(project.expression).project(translate(project.columns))
		 */
		public final BooleanMatrix visit(ProjectExpression project) {
			BooleanMatrix ret = lookup(project);
			if (ret!=null) return ret;
			
			final Int[] cols = new Int[project.arity()];
			int i = 0;
			for(IntExpression col : project.columns()) {
				cols[i++] = col.accept(this);
			}
			
			return cache(project, project.expression().accept(this).project(cols));
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
		 * @param decls list of formula declarations; should be quantifiedFormula.declarations initially
		 * @param formula the formula body
		 * @param declConstraints the constraints implied by the declarations; should be Boolean.FALSE intially
		 * @param acc the accumulator that contains the top level conjunction; should be an empty AND accumulator initially
		 * @effects the given accumulator contains the translation of the formula "all decls | formula"
		 */
		private void all(List<Decl> decls, Formula formula, BooleanValue declConstraints, BooleanAccumulator acc) {
			if (acc.isShortCircuited()) return;
			final BooleanFactory factory = interpreter.factory();
			
			if (decls.isEmpty()) {
				acc.add(factory.or(declConstraints, formula.accept(this)));
				return;
			}
			
			final Decl decl = decls.get(0);
			final BooleanMatrix declTransl = visit(decl);
			final List<Decl> others = decls.subList(1, decls.size());
			final BooleanMatrix groundValue = factory.matrix(declTransl.dimensions());
			env = env.extend(decl.variable(), groundValue);
			for(IndexedEntry<BooleanValue> entry : declTransl) {
				groundValue.set(entry.index(), BooleanConstant.TRUE);
				all(others, formula, factory.or(factory.not(entry.value()), declConstraints), acc);
				groundValue.set(entry.index(), BooleanConstant.FALSE);	
			}
			env = env.parent();
			
		}
		
		/**
		 * Translates the given existentially quantified formula as follows 
		 * (where A_0...A_|A| stand for boolean variables that represent the 
		 * tuples of the expression A, etc.):
		 * let quantFormula = "some a: A, b: B, ..., x: X | F(a, b, ..., x)" |
		 *     (A_0 && B_0 && ... && X_0 && translate(F(A_0, B_0, ..., X_0))) || ... || 
		 *     (A_|A| && B_|B| && ... && X_|X| && translate(F(A_|A|, B_|B|, ..., X_|X|))
		 * @param decls list of formula declarations; should be quantifiedFormula.declarations initially
		 * @param formula the formula body
		 * @param declConstraints the constraints implied by the declarations; should be Boolean.TRUE intially
		 * @param acc the accumulator that contains the top level conjunction; should be an empty OR accumulator initially
		 * @effects the given accumulator contains the translation of the formula "some decls | formula"
		 */
		private void some(List<Decl> decls, Formula formula, BooleanValue declConstraints, BooleanAccumulator acc) {
			if (acc.isShortCircuited()) return;
			final BooleanFactory factory = interpreter.factory();
			
			if (decls.isEmpty()) {
				acc.add(factory.and(declConstraints, formula.accept(this)));
				return;
			}
			
			final Decl decl = decls.get(0);
			final BooleanMatrix declTransl = visit(decl);
			final List<Decl> others = decls.subList(1, decls.size());
			final BooleanMatrix groundValue = factory.matrix(declTransl.dimensions());
			env = env.extend(decl.variable(), groundValue);
			for(IndexedEntry<BooleanValue> entry : declTransl) {
				groundValue.set(entry.index(), BooleanConstant.TRUE);
			    some(others, formula, factory.and(entry.value(), declConstraints), acc);
			    groundValue.set(entry.index(), BooleanConstant.FALSE);	
			}
			env = env.parent();
			
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
			case ALL		: 
				final BooleanAccumulator and = BooleanAccumulator.treeGate(Operator.AND);
				all(quantFormula.declarations().declarations(), quantFormula.formula(), BooleanConstant.FALSE, and); 
				ret = interpreter.factory().accumulate(and);
				break;
			case SOME	: 
				final BooleanAccumulator or = BooleanAccumulator.treeGate(Operator.OR);
				some(quantFormula.declarations().declarations(), quantFormula.formula(), BooleanConstant.TRUE, or); 
				ret = interpreter.factory().accumulate(or);
				break;
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
		public final BooleanValue visit(BinaryFormula binFormula) {
			BooleanValue ret = lookup(binFormula);
			if (ret!=null) return ret;
			
			final BooleanValue left = binFormula.left().accept(this);
			final BooleanValue right = binFormula.right().accept(this);
			final BinaryFormula.Operator op = binFormula.op();
			final BooleanFactory f = interpreter.factory();
			
			switch(op) {
			case AND		: ret = f.and(left, right); break;
			case OR			: ret = f.or(left, right); break;
			case XOR		: ret = f.xor(left, right); break;
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
		public final BooleanValue visit(NotFormula not) {
			BooleanValue ret = lookup(not);
			return ret==null ? 
				   cache(not, interpreter.factory().not(not.formula().accept(this))) : ret;
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
	
			return cache(compFormula,ret);
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
			case NO 	: ret = child.none(); break;
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
		public final BooleanValue visit(RelationPredicate pred) {
			BooleanValue ret = lookup(pred);
			return ret != null ? ret : cache(pred, pred.toConstraints().accept(this));
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
			ret = factory.matrix(Dimensions.square(interpreter.universe().size(), 1));
			for(IntIterator iter = interpreter.ints().iterator(); iter.hasNext(); ) {
				int i = iter.nextInt();
				int atomIndex = interpreter.interpret(i);
				ret.set(atomIndex, factory.or(ret.get(atomIndex), child.eq(factory.integer(i))));
			}
			
			return cache(castExpr, ret);
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
			
			return cache(intExpr, ret);
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
			return cache(intExpr, ret);
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
			case DIVIDE 	: ret = left.divide(right); break;
			case AND		: ret = left.and(right); break;
			case OR			: ret = left.or(right); break;
			case XOR		: ret = left.xor(right); break;
			case SHL		: ret = left.shl(right); break;
			case SHR		: ret = left.shr(right); break;
			case SHA		: ret = left.sha(right); break;
			default    :
				throw new IllegalArgumentException("Unknown operator: " + intExpr.op());
			}
			return cache(intExpr, ret);
		}
		
		/**
		 * Translates the given sum expression as follows 
		 * (where A_0...A_|A| stand for boolean variables that represent the 
		 * tuples of the expression A, etc.):
		 * let sum = "sum a: A, b: B, ..., x: X | IE(a, b, ..., x) " |
		 *     sum a: A, b: B, ..., x: X | if (a in A && b in B && ... && x in X) then IE(a, b, ..., x) else 0 }.
		 * @param decls list of formula declarations; should be quantifiedFormula.declarations initially
		 * @param formula the formula body
		 * @param declConstraints the constraints implied by the declarations; should be Boolean.TRUE intially
		 * @return the translation of "sum decls | expr"
		 */
		private final Int sum(List<Decl> decls, IntExpression expr, BooleanValue declConstraints) {
			final BooleanFactory factory = interpreter.factory();
			final Int zero = factory.integer(0);
			if (decls.isEmpty()) {
				return expr.accept(this).choice(declConstraints, factory.integer(0));
			}
			
			final Decl decl = decls.get(0);
			final BooleanMatrix declTransl = visit(decl);
			final List<Decl> others = decls.subList(1, decls.size());
			final BooleanMatrix groundValue = factory.matrix(declTransl.dimensions());
			env = env.extend(decl.variable(), groundValue);
			Int partialSum = zero;
			for(IndexedEntry<BooleanValue> entry : declTransl) {
				groundValue.set(entry.index(), BooleanConstant.TRUE);
				partialSum = partialSum.plus(sum(others, expr, factory.and(entry.value(), declConstraints)));
				groundValue.set(entry.index(), BooleanConstant.FALSE);	
			}
			env = env.parent();
			return partialSum;
		}
		
		/** 
		 * @return let intExpr = "sum a: A, b: B, ..., x: X | IE(a, b, ..., x)" |
		 *           translate(IE(A_0, B_0, ..., X_0)) + ... + translate(IE(A_|A|, B_|B|, ..., X_|X|))
		 */
		public Int visit(SumExpression intExpr) {
			Int ret = lookup(intExpr);
			return ret != null ? ret : 
				cache(intExpr, sum(intExpr.declarations().declarations(), intExpr.intExpr(), BooleanConstant.TRUE));
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
			return cache(intComp, ret);
		}
	}
}
