/* 
 * Kodkod -- Copyright (c) 2005-2007, Emina Torlak
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package kodkod.engine.fol2sat;

import java.util.ArrayList;
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
import kodkod.ast.UnaryIntExpression;
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
import kodkod.util.nodes.AnnotatedNode;

/**
 * Translates an annotated node to boolean representation.
 * @specfield node: AnnotatedNode<? extends Node> // node to translate
 * @specfield interpreter: LeafInterpreter // the interpreter used for translation	 
 * @specfield env: Environment<BooleanMatrix> // current environment
 * @author Emina Torlak
 */
abstract class FOL2BoolTranslator implements ReturnVisitor<BooleanMatrix, BooleanValue, Object, Int> {
	
	/**
	 * Translates the given annotated formula or expression into a boolean
	 * formula or matrix, using the provided interpreter. 
	 * @requires interpreter.relations = AnnotatedNode.relations(annotated)
	 * @return {transl: T | 
	 *           annotated.node in Formula => transl in BooleanValue, 
	 *           annotated.node in Expression => transl in BooleanMatrix, 
	 *           annotated.node in IntExpression => transl in Int}
	 * @throws HigherOrderDeclException - annotated.node contains a higher order declaration
	 * @throws UnboundLeafException - annotated.node refers to an undeclared variable 
	 **/
	@SuppressWarnings("unchecked")
	static final <T> T translate(AnnotatedNode<? extends Node> annotated, LeafInterpreter interpreter) {
		final FOL2BoolCache cache = new FOL2BoolCache(annotated);
		final FOL2BoolTranslator translator = new FOL2BoolTranslator(cache, interpreter) {};
		return (T) annotated.node().accept(translator);
	}

	/**
	 * Translates the given annotated formula into a boolean
	 * accumulator with respect to the given interpreter and logs the translation events to the given logger.  
	 * @requires interpreter.relations = AnnotatedNode.relations(annotated)
	 * @requires annotated.source[annotated.sourceSensitiveRoots()] = Nodes.roots(annotated.source[annotated.node])
	 * @return BooleanAccumulator that is the meaning of the given annotated formula with respect to the given interpreter
	 * @effects log.records' contains the translation events that occurred while generating the returned value
	 * @throws HigherOrderDeclException - annotated.node contains a higher order declaration
	 * @throws UnboundLeafException - annotated.node refers to an undeclared variable 
	 **/
	static final BooleanAccumulator translate(final AnnotatedNode<Formula> annotated, LeafInterpreter interpreter, final TranslationLogger logger) {
		final FOL2BoolCache cache = new FOL2BoolCache(annotated);
		final FOL2BoolTranslator translator = new FOL2BoolTranslator(cache, interpreter) {
			BooleanValue cache(Formula formula, BooleanValue translation) {
				logger.log(formula, translation, super.env);
				return super.cache(formula, translation);
			}	
		};
		final BooleanAccumulator acc = BooleanAccumulator.treeGate(Operator.AND);
		
		for(Formula root : annotated.sourceSensitiveRoots()) { 	
			acc.add(root.accept(translator));
		}
		logger.close();
		return acc;
	}
	
	/**
	 * Translates the given annotated expression into a boolean
	 * matrix that is a least sound upper bound on the expression's
	 * value, given the leaf and variable bindings in the 
	 * the provided interpreter and environment.
	 * @requires interpreter.relations = AnnotatedNode.relations(annotated)
	 * @return a boolean matrix that is a least sound upper bound on the expression's value
	 * @throws HigherOrderDeclException - annotated.node contains a higher order declaration
	 * @throws UnboundLeafException - annotated.node refers to a variable that neither declared nor bound in env
	 **/
	@SuppressWarnings("unchecked")
	static final BooleanMatrix approximate(AnnotatedNode<Expression> annotated, LeafInterpreter interpreter, Environment<BooleanMatrix> env) {
		final FOL2BoolTranslator approximator = new FOL2BoolTranslator(new FOL2BoolCache(annotated), interpreter, env) {
			public final BooleanMatrix visit(BinaryExpression binExpr) {
				if (binExpr.op().equals(BinaryExpression.Operator.DIFFERENCE)) {
					final BooleanMatrix ret = lookup(binExpr);
					return ret!=null ? ret : cache(binExpr, binExpr.left().accept(this));
				} else {
					return super.visit(binExpr);
				}
			}
			public final BooleanMatrix visit(Comprehension cexpr) {
				final BooleanMatrix ret = lookup(cexpr);
				return ret!=null ? ret : cache(cexpr, super.visit((Comprehension)Formula.TRUE.comprehension(cexpr.declarations())));
			}
			public BooleanMatrix visit(IfExpression ifExpr) {
				final BooleanMatrix ret = lookup(ifExpr);
				return ret!=null ? ret : cache(ifExpr, ifExpr.thenExpr().union(ifExpr.elseExpr()).accept(this));
			}
			public BooleanMatrix visit(IntToExprCast castExpr) {
				final BooleanMatrix ret = lookup(castExpr);
				return ret!=null ? ret : cache(castExpr, Expression.INTS.accept(this));
			}	
		};
		return annotated.node().accept(approximator);
	}

	
	/*---------------------------------------------------------*/
	private final LeafInterpreter interpreter;
	/* When visiting the body of a quantified formula or a comprehension, this
	 * environment contains the current values of the enclosing quantified variable(s) */
	private Environment<BooleanMatrix> env;

	private final FOL2BoolCache cache;

	/**
	 * Constructs a new translator that will use the given translation cache
	 * and interpreter to perform the translation.
	 * @effects this.node' = manager.node
	 */   
	private FOL2BoolTranslator(FOL2BoolCache cache,  LeafInterpreter interpreter) {
		this.interpreter = interpreter;
		this.env = Environment.empty();
		this.cache = cache;
	}

	/**
	 * Constructs a new translator that will use the given translation cache,
	 * interpreter and environment to perform the translation.
	 * @effects this.node' = manager.node
	 */   
	private FOL2BoolTranslator(FOL2BoolCache cache,  LeafInterpreter interpreter, Environment<BooleanMatrix> env) {
		this.interpreter = interpreter;
		this.env = env;
		this.cache = cache;
	}

	/**
	 * Retrieves the cached translation for the given node, if any.
	 * Otherwise returns null.
	 * @return the cached translation for the given node, if any.
	 * Otherwise returns null.
	 */
	@SuppressWarnings("unchecked") 
	final <T> T lookup(Node node) {
		return (T) cache.lookup(node, env);
	}

	/**
	 * The translation is cached, if necessary, and returned.
	 * @return translation
	 * @effects the translation may be cached
	 */
	final <T> T cache(Node node, T translation) {
		return cache.cache(node, translation, env);
	}

	/**
	 * The translation is cached, if necessary, and returned.
	 * @return translation
	 * @effects the translation may be cached
	 */
	BooleanValue cache(Formula formula, BooleanValue translation) {
		return cache.cache(formula, translation, env);
	}
	
	/** 
	 * Calls lookup(decls) and returns the cached value, if any.  
	 * If a translation has not been cached, translates decls into a list
	 * of translations of its children,
	 * calls cache(...) on it and returns it.
	 * @return let t = lookup(decls) | 
	 *   some t => t, cache(decl, decls.declarations.expression.accept(this))
	 */
	public final List<BooleanMatrix> visit(Decls decls) {
		List<BooleanMatrix> ret = lookup(decls);
		if (ret!=null) return ret;
		ret = new ArrayList<BooleanMatrix>(decls.size());
		for(Decl decl : decls) {
			ret.add(visit(decl));
		}
		return cache(decls, ret);
	}

	/** 
	 * Calls lookup(decl) and returns the cached value, if any.  
	 * If a translation has not been cached, translates decl.expression,
	 * calls cache(...) on it and returns it.
	 * @return let t = lookup(decl) | 
	 *   some t => t, cache(decl, decl.expression.accept(this))
	 */
	public final BooleanMatrix visit(Decl decl) {
		BooleanMatrix matrix = lookup(decl);
		if (matrix!=null) return matrix;
		if (decl.multiplicity()!=Multiplicity.ONE)
			throw new HigherOrderDeclException(decl);
		return cache(decl, decl.expression().accept(this));
	}

	/**
	 * Calls this.env.lookup(variable) and returns the current binding for the
	 * given variable. If no binding is found, an UnboundLeafException is thrown.
	 * @return this.env.lookup(variable)
	 * @throws UnboundLeafException - no this.env.lookup(variable)
	 */
	public final BooleanMatrix visit(Variable variable) {
		final BooleanMatrix ret = env.lookup(variable);
		if (ret != null) return ret;
		else throw new UnboundLeafException("Unbound variable", variable);
	}

	/**
	 * Returns this.interpreter.interpret(relation).
	 * @return this.interpreter.interpret(relation)
	 */
	public final BooleanMatrix visit(Relation relation) {
		return interpreter.interpret(relation);
	}

	/**
	 * Returns this.interpreter.interpret(constExpr).
	 * @return this.interpreter.interpret(constExpr).
	 */
	public final BooleanMatrix visit(ConstantExpression constExpr) {
		return interpreter.interpret(constExpr);
	}

	/**
	 * Calls lookup(binExpr) and returns the cached value, if any.  
	 * If a translation has not been cached, translates the expression,
	 * calls cache(...) on it and returns it.
	 * @return let t = lookup(binExpr) | some t => t, 
	 *      let op = (binExpr.op).(UNION->or + INTERSECTION->and + DIFFERENCE->difference + OVERRIDE->override + JOIN->dot + PRODUCT->cross) | 
	 *       cache(binExpr, op(binExpr.left.accept(this), binExpr.right.accept(this)))
	 */
	public BooleanMatrix visit(BinaryExpression binExpr) {
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
	 * Calls lookup(unaryExpr) and returns the cached value, if any.  
	 * If a translation has not been cached, translates the expression,
	 * calls cache(...) on it and returns it.
	 * @return let t = lookup(unaryExpr) | some t => t, 
	 *      let op = (unaryExpr.op).(TRANSPOSE->transpose + CLOSURE->closure + REFLEXIVE_CLOSURE->(lambda(m)(m.closure().or(iden))) | 
	 *       cache(unaryExpr, op(unaryExpr.child))
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
	 * @param decls the declarations comprehension
	 * @param param formula the body of the comprehension
	 * @param currentDecl currently processed declaration; should be 0 initially
	 * @param declConstraints the constraints implied by the declarations; should be Boolean.TRUE intially
	 * @param partialIndex partial index into the provided matrix; should be 0 initially
	 * @param matrix boolean matrix that will retain the final results; should be an empty matrix of dimensions universe.size^decls.length initially
	 * @effects the given matrix contains the translation of the comprehension "{ decls | formula }"
	 */
	private final void comprehension(Decls decls, Formula formula, int currentDecl, 
			BooleanValue declConstraints, int partialIndex, BooleanMatrix matrix) {
		final BooleanFactory factory = interpreter.factory();

		if (currentDecl==decls.size()) {
			matrix.set(partialIndex, factory.and(declConstraints, formula.accept(this)));
			return;
		}

		final Decl decl = decls.get(currentDecl);
		final BooleanMatrix declTransl = visit(decl);
		final int position = (int)StrictMath.pow(interpreter.universe().size(), decls.size()-currentDecl-1);
		final BooleanMatrix groundValue = factory.matrix(declTransl.dimensions());
		env = env.extend(decl.variable(), groundValue);
		for(IndexedEntry<BooleanValue> entry : declTransl) {
			groundValue.set(entry.index(), BooleanConstant.TRUE);
			comprehension(decls, formula, currentDecl+1, factory.and(entry.value(), declConstraints), 
					partialIndex + entry.index()*position, matrix);
			groundValue.set(entry.index(), BooleanConstant.FALSE);	
		}
		env = env.parent();
	}

	/**
	 * Calls lookup(cexpr) and returns the cached value, if any.  
	 * If a translation has not been cached, translates the expression,
	 * calls cache(...) on it and returns it.
	 * @return let t = lookup(cexpr) | some t => t, 
	 *      cache(cexpr, translate(cexpr))
	 */
	public BooleanMatrix visit(Comprehension cexpr) {
		BooleanMatrix ret = lookup(cexpr);
		if (ret!=null) return ret;

		ret = interpreter.factory().matrix(Dimensions.square(interpreter.universe().size(), cexpr.declarations().size()));
		comprehension(cexpr.declarations(), cexpr.formula(), 0, BooleanConstant.TRUE, 0, ret);

		return cache(cexpr,ret);
	}

	/**
	 * Calls lookup(ifExpr) and returns the cached value, if any.  
	 * If a translation has not been cached, translates the expression,
	 * calls cache(...) on it and returns it.
	 * @return let t = lookup(ifExpr) | some t => t, 
	 * 			cache(ifExpr, ifExpr.condition.accept(this).choice(ifExpr.then.accept(this), ifExpr.else.accept(this)))
	 */
	public BooleanMatrix visit(IfExpression ifExpr) {
		BooleanMatrix ret = lookup(ifExpr);
		if (ret!=null) return ret;

		final BooleanValue condition = ifExpr.condition().accept(this);
		final BooleanMatrix thenExpr = ifExpr.thenExpr().accept(this);
		final BooleanMatrix elseExpr = ifExpr.elseExpr().accept(this);
		ret = thenExpr.choice(condition, elseExpr);

		return cache(ifExpr,ret);
	}

	/**
	 * Calls lookup(project) and returns the cached value, if any.  
	 * If a translation has not been cached, translates the expression,
	 * calls cache(...) on it and returns it.
	 * @return let t = lookup(project) | some t => t, 
	 * 			cache(project, project.expression.accept(this).project(translate(project.columns))
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
		return cache(constant, BooleanConstant.constant(constant.booleanValue()));
	}

	/**
	 * Translates the given universally quantified formula as follows 
	 * (where A_0...A_|A| stand for boolean variables that represent the 
	 * tuples of the expression A, etc.):
	 * let quantFormula = "all a: A, b: B, ..., x: X | F(a, b, ..., x)" |
	 *     (A_0 && B_0 && ... && X_0 => translate(F(A_0, B_0, ..., X_0))) && ... && 
	 *     (A_|A| && B_|B| && ... && X_|X| => translate(F(A_|A|, B_|B|, ..., X_|X|))
	 * @param decls formula declarations
	 * @param formula the formula body
	 * @param currentDecl currently processed declaration; should be 0 initially
	 * @param declConstraints the constraints implied by the declarations; should be Boolean.FALSE intially
	 * @param acc the accumulator that contains the top level conjunction; should be an empty AND accumulator initially
	 * @effects the given accumulator contains the translation of the formula "all decls | formula"
	 */
	private void all(Decls decls, Formula formula, int currentDecl, BooleanValue declConstraints, BooleanAccumulator acc) {
		if (acc.isShortCircuited()) return;
		final BooleanFactory factory = interpreter.factory();

		if (decls.size()==currentDecl) {
			acc.add(factory.or(declConstraints, formula.accept(this)));
			return;
		}

		final Decl decl = decls.get(currentDecl);
		final BooleanMatrix declTransl = visit(decl);
		final BooleanMatrix groundValue = factory.matrix(declTransl.dimensions());
		env = env.extend(decl.variable(), groundValue);
		for(IndexedEntry<BooleanValue> entry : declTransl) {
			groundValue.set(entry.index(), BooleanConstant.TRUE);
			all(decls, formula, currentDecl+1, factory.or(factory.not(entry.value()), declConstraints), acc);
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
	 * @param decls formula declarations
	 * @param formula the formula body
	 * @param currentDecl currently processed declaration; should be 0 initially
	 * @param declConstraints the constraints implied by the declarations; should be Boolean.TRUE intially
	 * @param acc the accumulator that contains the top level conjunction; should be an empty OR accumulator initially
	 * @effects the given accumulator contains the translation of the formula "some decls | formula"
	 */
	private void some(Decls decls, Formula formula, int currentDecl, BooleanValue declConstraints, BooleanAccumulator acc) {
		if (acc.isShortCircuited()) return;
		final BooleanFactory factory = interpreter.factory();

		if (decls.size()==currentDecl) {
			acc.add(factory.and(declConstraints, formula.accept(this)));
			return;
		}

		final Decl decl = decls.get(currentDecl);
		final BooleanMatrix declTransl = visit(decl);
		final BooleanMatrix groundValue = factory.matrix(declTransl.dimensions());
		env = env.extend(decl.variable(), groundValue);
		for(IndexedEntry<BooleanValue> entry : declTransl) {
			groundValue.set(entry.index(), BooleanConstant.TRUE);
			some(decls, formula, currentDecl+1, factory.and(entry.value(), declConstraints), acc);
			groundValue.set(entry.index(), BooleanConstant.FALSE);	
		}
		env = env.parent();

	}

	/** 
	 * Calls lookup(quantFormula) and returns the cached value, if any.  
	 * If a translation has not been cached, translates the formula,
	 * calls cache(...) on it and returns it.
	 * @return let t = lookup(quantFormula) | some t => t, 
	 *   cache(quantFormula, translate(quantFormula))
	 */
	public final BooleanValue visit(QuantifiedFormula quantFormula) {
		BooleanValue ret = lookup(quantFormula);
		if (ret!=null) return ret;

		final QuantifiedFormula.Quantifier quantifier = quantFormula.quantifier();

		switch(quantifier) {
		case ALL		: 
			final BooleanAccumulator and = BooleanAccumulator.treeGate(Operator.AND);
			all(quantFormula.declarations(), quantFormula.formula(), 0, BooleanConstant.FALSE, and); 
			ret = interpreter.factory().accumulate(and);
			break;
		case SOME	: 
			final BooleanAccumulator or = BooleanAccumulator.treeGate(Operator.OR);
			some(quantFormula.declarations(), quantFormula.formula(), 0, BooleanConstant.TRUE, or); 
			ret = interpreter.factory().accumulate(or);
			break;
		default :
			throw new IllegalArgumentException("Unknown quantifier: " + quantifier);
		}

		return cache(quantFormula,ret);
	}

	/** 
	 * Calls lookup(binFormula) and returns the cached value, if any.  
	 * If a translation has not been cached, translates the formula,
	 * calls cache(...) on it and returns it.
	 * @return let t = lookup(binFormula) | some t => t, 
	 * 	cache(binFormula, binFormula.op(binFormula.left.accept(this), binFormula.right.accept(this))
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
		case IMPLIES	: ret = f.implies(left, right); break;
		case IFF		: ret = f.iff(left, right); break;
		default : 
			throw new IllegalArgumentException("Unknown operator: " + op);
		}

		return cache(binFormula, ret);
	}

	/** 
	 * Calls lookup(not) and returns the cached value, if any.  
	 * If a translation has not been cached, translates the formula,
	 * calls cache(...) on it and returns it.
	 * @return let t = lookup(not) | some t => t, 
	 * 	cache(not, !not.formula.accept(this))
	 */
	public final BooleanValue visit(NotFormula not) {
		BooleanValue ret = lookup(not);
		return ret==null ? 
				cache(not, interpreter.factory().not(not.formula().accept(this))) : ret;
	}

	/**
	 * Calls lookup(compFormula) and returns the cached value, if any.  
	 * If a translation has not been cached, translates the formula,
	 * calls cache(...) on it and returns it.
	 * @return let t = lookup(compFormula) | some t => t, 
	 *      let op = (binExpr.op).(SUBSET->subset + EQUALS->eq) | 
	 *       cache(compFormula, op(compFormula.left.accept(this), compFormula.right.accept(this)))
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
	 * Calls lookup(multFormula) and returns the cached value, if any.  
	 * If a translation has not been cached, translates the formula,
	 * calls cache(...) on it and returns it.
	 * @return let t = lookup(multFormula) | some t => t, 
	 *      let op = (multFormula.mult).(NO->none + SOME->some + ONE->one + LONE->lone) | 
	 *       cache(multFormula, op(multFormula.expression.accept(this)))
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
	 * Calls lookup(pred) and returns the cached value, if any.  
	 * If a translation has not been cached, translates the expression,
	 * calls cache(...) on it and returns it.
	 * @return let t = lookup(pred) | some t => t, 
	 * 	cache(pred, pred.toConstraints().accept(this))
	 */
	public final BooleanValue visit(RelationPredicate pred) {
		BooleanValue ret = lookup(pred);
		return ret != null ? ret : cache(pred, pred.toConstraints().accept(this));
	}

	/** 
	 * Calls lookup(castExpr) and returns the cached value, if any.  
	 * If a translation has not been cached, translates the expression,
	 * calls cache(...) on it and returns it.
	 * @return let t = lookup(castExpr) | some t => t, 
	 * 	cache(castExpr, translate(castExpr))
	 */
	public BooleanMatrix visit(IntToExprCast castExpr) {
		BooleanMatrix ret = lookup(castExpr);
		if (ret!=null) return ret;

		final Int child = castExpr.intExpr().accept(this);
		final BooleanFactory factory =  interpreter.factory();
		final IntSet ints = interpreter.ints();
		
		ret = factory.matrix(Dimensions.square(interpreter.universe().size(), 1));
		
		switch(castExpr.op()) {
		case INTCAST : 	
			for(IntIterator iter = ints.iterator(); iter.hasNext(); ) {
				int i = iter.next();
				int atomIndex = interpreter.interpret(i);
				ret.set(atomIndex, factory.or(ret.get(atomIndex), child.eq(factory.integer(i))));
			}
			break;
		case BITSETCAST : 
			final List<BooleanValue> twosComplement = child.twosComplementBits();
			final int msb = twosComplement.size()-1;
			// handle all bits but the sign bit
			for(int i = 0; i < msb; i++) { 
				int pow2 = 1<<i;
				if (ints.contains(pow2)) { 
					ret.set(interpreter.interpret(pow2), twosComplement.get(i));
				}
			}
			// handle the sign bit
			if (ints.contains(-1<<msb)) {
				ret.set(interpreter.interpret(-1<<msb), twosComplement.get(msb));
			}
			break;
		default : 
			throw new IllegalArgumentException("Unknown cast operator: " + castExpr.op());
		}
		
		return cache(castExpr, ret);
	}	

	/**
	 * @return this.interpreter.factory.integer(intConst.value, this.encoding)
	 */
	public final Int visit(IntConstant intConst) {
		return interpreter.factory().integer(intConst.value());
	}

	/**
	 * Calls lookup(intExpr) and returns the cached value, if any.  
	 * If a translation has not been cached, translates the expression,
	 * calls cache(...) on it and returns it.
	 * @return let t = lookup(intExpr) | some t => t, 
	 * 			cache(intExpr, intExpr.condition.accept(this).choice(intExpr.then.accept(this), intExpr.else.accept(this)))
	 */
	public final Int visit(IfIntExpression intExpr) { 
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
	private final Int sum(BooleanMatrix m, IntIterator iter, int low, int high) {
		if (low > high)
			return interpreter.factory().integer(0);
		else if (low==high) {
			int i = iter.next();
			return interpreter.factory().integer(i, m.get(interpreter.interpret(i)));
		} else {
			final int mid = (low + high) / 2;
			final Int lsum = sum(m, iter, low, mid);
			final Int hsum = sum(m, iter, mid+1, high);
			return lsum.plus(hsum);
		}
	}

	/** 
	 * Calls lookup(intExpr) and returns the cached value, if any.  
	 * If a translation has not been cached, translates the expression,
	 * calls cache(...) on it and returns it.
	 * @return let t = lookup(intExpr) | some t => t, 
	 * 	cache(intExpr, translate(intExpr))
	 */
	public final Int visit(ExprToIntCast intExpr) {
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
	 * Calls lookup(intExpr) and returns the cached value, if any.  
	 * If a translation has not been cached, translates the expression,
	 * calls cache(...) on it and returns it.
	 * @return let t = lookup(intExpr) | some t => t, 
	 * 	cache(intExpr, intExpr.left.accept(this) intExpr.op intExpr.right.accept(this))
	 */
	public final Int visit(BinaryIntExpression intExpr) {
		Int ret = lookup(intExpr);
		if (ret!=null) return ret;
		final Int left = intExpr.left().accept(this);
		final Int right = intExpr.right().accept(this);
		switch(intExpr.op()) {
		case PLUS  		: ret = left.plus(right); break;
		case MINUS 		: ret = left.minus(right); break;
		case MULTIPLY 	: ret = left.multiply(right); break;
		case DIVIDE 	: ret = left.divide(right); break;
		case MODULO		: ret = left.modulo(right); break;
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
	 * Calls lookup(intExpr) and returns the cached value, if any.  
	 * If a translation has not been cached, translates the expression,
	 * calls cache(...) on it and returns it.
	 * @return let t = lookup(intExpr) | some t => t, 
	 * 	cache(intExpr, intExpr.op(intExpr.expression.accept(this)))
	 */
	public final Int visit(UnaryIntExpression intExpr) {
		Int ret = lookup(intExpr);
		if (ret!=null) return ret;
		final Int child = intExpr.expression().accept(this);
		switch(intExpr.op()) {
		case MINUS 	: ret = child.negate(); break;
		case NOT 	: ret = child.not(); break;
		case ABS 	: ret = child.abs(); break;
		case SGN 	: ret = child.sgn(); break;
		default : 
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
	 * @param decls intexpr declarations
	 * @param formula the formula body
	 * @param currentDecl currently processed declaration; should be 0 initially
	 * @param declConstraints the constraints implied by the declarations; should be Boolean.TRUE intially
	 * @param values integer values computed so far
	 */
	private final void sum(Decls decls, IntExpression expr, int currentDecl, BooleanValue declConstraints,
			List<Int> values) {
		final BooleanFactory factory = interpreter.factory();
		if (decls.size()==currentDecl) {
			values.add( expr.accept(this).choice(declConstraints, factory.integer(0)) );
			return;
		}

		final Decl decl = decls.get(currentDecl);
		final BooleanMatrix declTransl = visit(decl);
		final BooleanMatrix groundValue = factory.matrix(declTransl.dimensions());
		env = env.extend(decl.variable(), groundValue);
		for(IndexedEntry<BooleanValue> entry : declTransl) {
			groundValue.set(entry.index(), BooleanConstant.TRUE);
			sum(decls, expr, currentDecl+1, factory.and(entry.value(), declConstraints), values);
			groundValue.set(entry.index(), BooleanConstant.FALSE);	
		}
		env = env.parent();
	}

	/** 
	 * Calls lookup(intExpr) and returns the cached value, if any.  
	 * If a translation has not been cached, translates the expression,
	 * calls cache(...) on it and returns it.
	 * @return let t = lookup(intExpr) | some t => t, 
	 * 	cache(intExpr, translate(intExpr))
	 */
	public final Int visit(SumExpression intExpr) {
		final Int ret = lookup(intExpr);
		if (ret!=null) return ret;
		final List<Int> values = new ArrayList<Int>();
		sum(intExpr.declarations(), intExpr.intExpr(), 0, BooleanConstant.TRUE, values);
		for(int sums = values.size(); sums > 1; sums -= sums/2) { 
			final int max = sums-1;
			for(int i = 0; i < max; i += 2) { 
				values.set(i/2, values.get(i).plus(values.get(i+1)));
			}
			if (max%2==0) { // even max => odd number of entries
				values.set(max/2, values.get(max));
			}
		}
		return cache(intExpr, values.isEmpty() ? interpreter.factory().integer(0) : values.get(0));
	}

	/** 
	 * Calls lookup(intComp) and returns the cached value, if any.  
	 * If a translation has not been cached, translates the formula,
	 * calls cache(...) on it and returns it.
	 * @return let t = lookup(intComp) | some t => t, 
	 * 	cache(intComp, intComp.left.accept(this) intComp.op intComp.right.accept(this))
	 */
	public final BooleanValue visit(IntComparisonFormula intComp) {
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

