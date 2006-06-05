package kodkod.engine.fol2sat;

import static kodkod.engine.bool.BooleanConstant.TRUE;

import java.util.ArrayList;
import java.util.Collections;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import kodkod.ast.BinaryExpression;
import kodkod.ast.BinaryFormula;
import kodkod.ast.BinaryIntExpression;
import kodkod.ast.Cardinality;
import kodkod.ast.ComparisonFormula;
import kodkod.ast.Comprehension;
import kodkod.ast.ConstantExpression;
import kodkod.ast.ConstantFormula;
import kodkod.ast.Decl;
import kodkod.ast.Decls;
import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.IfExpression;
import kodkod.ast.IntComparisonFormula;
import kodkod.ast.IntConstant;
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
import kodkod.engine.bool.BooleanAccumulator;
import kodkod.engine.bool.BooleanConstant;
import kodkod.engine.bool.BooleanFactory;
import kodkod.engine.bool.BooleanFormula;
import kodkod.engine.bool.BooleanMatrix;
import kodkod.engine.bool.BooleanValue;
import kodkod.engine.bool.Dimensions;
import kodkod.engine.bool.Int;
import kodkod.engine.bool.Operator;
import kodkod.util.collections.IdentityHashSet;
import kodkod.util.ints.IndexedEntry;
import kodkod.util.ints.IntSet;
import kodkod.util.ints.Ints;

/**
 * Translates a first order logic formula into a boolean circuit.
 * 
 * @author Emina Torlak
 */
final class Fol2Bool {

	private Fol2Bool() {}
	
	/**
	 * Translates the given annotated formula or expression into a boolean
	 * formula or matrix, using the provided allocator and integer encoding.
	 * @requires allocator.relations = AnnotatedNode.relations(annotated)
	 * @return {transl: T | 
	 *           annotated.node in Formula => transl in BooleanValue, 
	 *           annotated.node in Expression => transl in BooleanMatrix}
	 */
	@SuppressWarnings("unchecked")
	static final <T> T translate(AnnotatedNode<? extends Node> annotated, BooleanFormulaAllocator allocator, Options.IntEncoding encoding) {
		return (T) annotated.node().accept(new Translator(annotated, allocator, encoding));
	}
	
	/**
	 * Translates the given annotated formula into a boolean circuit using
	 * the provided allocator and integer encoding.  Additionally, it 
	 * keeps track of which variables comprise the descendents of the formula,
	 * and which of its descendents are reduced to constants during translation.
	 * @requires allocator.relations = AnnotatedNode.relations(annotated)
	 * @return {c: AnnotatedCircuit | c.formula = formula } 
	 */
	static final AnnotatedCircuit translateAndTrack(AnnotatedNode<Formula> annotated, BooleanFormulaAllocator allocator, Options.IntEncoding encoding) {
		final TrackingTranslator t = new TrackingTranslator(annotated, allocator, encoding);
		return new AnnotatedCircuit(annotated.node().accept(t), t.varUsage, t.trueFormulas, t.falseFormulas);
	}
	
	/**
	 * Stores the translation and annotations computed by 
	 * {@link Fol2Bool#translateAndTrack(Formula, Set, BooleanFormulaAllocator)}. 
	 * 
	 * @specfield formula: Formula // the formula being translated
	 * @specfield allocator: BooleanFormulaAllocator // the allocator used for translation
	 */
	static final class AnnotatedCircuit {
		private final BooleanValue translation;
		private final Map<Node,IntSet> variableUsage;
		private final Set<Formula> trueFormulas, falseFormulas;
		
		private AnnotatedCircuit(BooleanValue translation, Map<Node,IntSet> varUsage, 
				                Set<Formula> trueFormulas, Set<Formula> falseFormulas) {
			this.translation = translation;
			this.variableUsage = Collections.unmodifiableMap(varUsage);
			this.trueFormulas = Collections.unmodifiableSet(trueFormulas);
			this.falseFormulas = Collections.unmodifiableSet(falseFormulas);
		}
		/**
		 * Returns the translation of this.formula to a boolean circuit.
		 * @return the translation of this.formula to a boolean circuit.
		 */
		BooleanValue translation() {
			return translation;
		}
		
		/**
		 * Returns a map from the descendents of this.formula to the 
		 * literals representing the BooleanFormulas that comprise 
		 * the descendents' translations.  Descendents that evaluate 
		 * to BooleanConstants or BooleanMatrices comprised of BooleanConstants
		 * are not mapped.
		 * @return from the descendents of this.formula to the 
		 * literals representing the BooleanValues that comprise 
		 * the descendents' translations.
		 */
		Map<Node, IntSet> variableUsage() {
			return variableUsage;
		}
		
		/**
		 * Returns the set of descendents of this.formula that 
		 * evaluate to the given constant.
		 * @return {f: this.formula.*children & Formula | translate(f,this.allocator) = value}
		 */
		Set<Formula> formulasThatAre(BooleanConstant value) {
			return value.booleanValue() ? trueFormulas : falseFormulas;
		}
	}
	
	/**
	 * A subclass of Translator that tracks variables in addition to 
	 * translating nodes to circuits.
	 * 
	 * @specfield formula: Formula
	 * @specfield varUsage: formula.*children & (Expression + Formula) -> set int
	 * @specfield trueFormulas: set formula.*children & Formula
	 * @specfield falseFormulas: set formula.*children & Formula
	 */
	private static final class TrackingTranslator extends Translator {
		final Map<Node, IntSet> varUsage;
		final Set<Formula> trueFormulas, falseFormulas;
		
		/**
		 * Constructs a new translator that will use the given allocator to perform the 
		 * translation of the specified annotated node.
		 */    
		TrackingTranslator(AnnotatedNode<? extends Node> annotated, BooleanFormulaAllocator allocator, Options.IntEncoding encoding) {
			super(annotated, allocator, encoding);
			this.varUsage = new IdentityHashMap<Node,IntSet>();
			this.trueFormulas = new IdentityHashSet<Formula>();
			this.falseFormulas = new IdentityHashSet<Formula>();
		}
		
		/**
		 * If the given expression is one for which we are caching translations,
		 * the provided translation is cached and returned.  Otherwise,
		 * the translation is simply returned.  In addition, this method records
		 * the labels of BooleanFormulas that comprise the dense regions of 
		 * the translation in the varUsage map. 
		 * @return translation
		 * @effects if the expression is one for which we are caching translations,
		 * the provided translation is cached.
		 * @effects this.varUsage' = this.varUsage + 
		 *           expr -> {i: int | some b: translation.elements[int] - BooleanConstant | 
		 *                     i = |b.label| }
		 */
		@Override
		protected BooleanMatrix record(Expression expr, BooleanMatrix translation) {
			final BooleanFactory factory = allocator.factory();
			IntSet vars;
			if (env.parent()==null) { // top-level expression
				vars = Ints.bestSet(1, StrictMath.max(1, factory.maxFormulaLabel()));		
			} else { // not a top-level expression
				vars = varUsage.get(expr);
				if (vars==null)
					vars = Ints.bestSet(1, Integer.MAX_VALUE-1);
			}
			for(IndexedEntry<BooleanValue> e: translation) {
				if (e.value() != TRUE)
					vars.add(StrictMath.abs(((BooleanFormula)e.value()).label()));
			}
			varUsage.put(expr, vars);
			return cache.cache(expr, translation, env);
		}
		
		/**
		 * If the given formula is one for which we are caching translations,
		 * the provided translation is cached and returned.  Otherwise,
		 * the translation is simply returned. In addition, this method records
		 * the label of the translation in the varUsage map, if the translation
		 * is non-constant.  If it is constant, it records the formula as being
		 * constant.  
		 * @return translation
		 * @effects if the formula is one for which we are caching translations,
		 * the provided translation is cached.
		 * @effects translation = BooleanConstant.TRUE => 
		 * 	          this.trueFormulas' = this.trueFormulas + formula,
		 *          translation = BooleanConstant.FALSE => 
		 * 	          this.falseFormulas' = this.falseFormulas + formula,
		 *          this.varUsage' = this.varUsage + formula -> |translation.label|
		 */
		@Override
		protected BooleanValue record(Formula formula, BooleanValue translation) {
			if (translation==BooleanConstant.TRUE) {
				if (env.parent()==null) trueFormulas.add(formula);
			} else if (translation==BooleanConstant.FALSE) {
				if (env.parent()==null) falseFormulas.add(formula);
			} else if (env.parent()==null) { // top-level formula
				varUsage.put(formula, Ints.singleton(StrictMath.abs(((BooleanFormula)translation).label())));	
			} else {
				IntSet vars = varUsage.get(formula);
				if (vars==null)
					vars = Ints.bestSet(1, Integer.MAX_VALUE-1);
				vars.add(StrictMath.abs(((BooleanFormula)translation).label()));
				varUsage.put(formula, vars);
			}
			return cache.cache(formula, translation, env);
		}
	}
	
	/**
	 * The helper class that performs the translation.  This
	 * version of the translator does not track variables.
	 */
	private static class Translator implements ReturnVisitor<BooleanMatrix, BooleanValue, Object, Int> {
		final Int.Encoding encoding;
		
		final BooleanFormulaAllocator allocator;
		
		/* When visiting the body of a quantified formula or a comprehension, this
		 * environment contains the current values of the enclosing quantified variable(s) */
		Environment<BooleanMatrix> env;
		
		final TranslationCache cache;
	
		/**
		 * Constructs a new translator that will use the given allocator to perform the 
		 * translation of the specified annotated node. 
		 */   
		Translator(AnnotatedNode<? extends Node> annotated, BooleanFormulaAllocator allocator, Options.IntEncoding encoding) {
			this.allocator = allocator;
			this.env = new Environment<BooleanMatrix>();
			this.cache = new TranslationCache(annotated);
			switch(encoding) {
			case UNARY : this.encoding = Int.Encoding.UNARY; break;
			case BINARY : this.encoding = Int.Encoding.BINARY; break;
			case TWOS_COMPLEMENT : this.encoding = Int.Encoding.TWOS_COMPLEMENT; break;
			default:
				throw new IllegalArgumentException("Unknown encoding: " + encoding);
			}
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
				final IntSet all =  Ints.rangeSet(Ints.range(0, univSize-1));
				ret= allocator.factory().matrix(Dimensions.square(1, univSize), all, all);
			} else if (constExpr==Expression.IDEN) {
				final Dimensions dim2 = Dimensions.square(2, univSize);
				final IntSet iden = Ints.bestSet(dim2.capacity());
				for(int i = 0; i < univSize; i++) {
					iden.add(i*univSize + i);
				}			
				ret = allocator.factory().matrix(dim2, iden, iden);
			} else if (constExpr==Expression.NONE) {
				ret = allocator.factory().matrix(Dimensions.square(1, univSize), Ints.EMPTY_SET, Ints.EMPTY_SET);
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
				
				ret.set(dims.convert(index), factory.adopt(conjunct)); 
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
			final BooleanAccumulator conjunct = BooleanAccumulator.treeGate(Operator.AND);
			
			final Decls decls = qf.declarations();
			final List<BooleanMatrix> declTransls = visit(decls);
			final GroundValueGenerator generator = new GroundValueGenerator(env, decls, declTransls);
			final BooleanFactory factory = allocator.factory();
			
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
				
				conjunct.add(factory.adopt(disjunct));
				
			}
			env = generator.baseEnvironment();
			
			return factory.adopt(conjunct);
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
			final BooleanFactory factory = allocator.factory();
			
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
				
				disjunct.add(factory.adopt(conjunct));	
			}
			env = generator.baseEnvironment();
			
			return factory.adopt(disjunct);
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
		 * @return let tLeft = translate(compFormula.left), tRight = translate(compFormula.right) | 
		 *          compFormula.op = SUBSET => 
		 *           tLeft.subset(tRight), tleft.eq(tRight)          
		 */
		public final BooleanValue visit(ComparisonFormula compFormula) {
			BooleanValue ret = retrieve(compFormula);
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

		/**
		 * @return factory.integer(intConst.value, this.encoding)
		 */
		public Int visit(IntConstant intConst) {
			return allocator.factory().integer(intConst.value(), encoding);
		}

		/**
		 * @return translate(intExpr.expression).cardinality(this.encoding)
		 */
		public Int visit(Cardinality intExpr) {
			Int ret = retrieve(intExpr);
			if (ret!=null) return ret;
			return record(intExpr, intExpr.expression().accept(this).cardinality(encoding));
		}

		/**
		 * @return translate(intExpr.left) intExpr.op translate(intExpr.right)
		 */
		public Int visit(BinaryIntExpression intExpr) {
			Int ret = retrieve(intExpr);
			if (ret!=null) return ret;
			final Int left = intExpr.left().accept(this);
			final Int right = intExpr.right().accept(this);
			switch(intExpr.op()) {
			case PLUS  : ret = left.plus(right); break;
			case MINUS : ret = left.minus(right); break;
			default    :
				throw new IllegalArgumentException("Unknown operator: " + intExpr.op());
			}
			return record(intExpr, ret);
		}
		
		/**
		 * @return translate(intComp.left) intComp.op translate(intComp.right)
		 */
		public BooleanValue visit(IntComparisonFormula intComp) {
			BooleanValue ret = retrieve(intComp);
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
//			System.out.println(intComp.left() + ": " + left);
//			System.out.println(intComp.right() + ": " + right);
//			System.out.println(ret);
			return record(intComp, ret);
		}	
	}
}
