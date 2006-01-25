/*
 * DepthFirstReplacer.java
 * Created on Aug 24, 2005
 */
package kodkod.ast;

import java.util.Iterator;
import java.util.List;



/** 
 * A depth first replacer.  The default implementation
 * returns the tree to which it is applied.  Reference 
 * equality is used to determine if two nodes are the same.
 * Subclasses may choose to cache replacements for certain
 * nodes by properly implementing the lookup and cache
 * methods.  The default implementation does not perform
 * any caching.
 * @specfield cache: Node lone->lone Node
 * @author Emina Torlak 
 */
public abstract class DepthFirstReplacer implements Visitor<Expression, Formula, Decls> {
	
	protected DepthFirstReplacer() { }
	
	/**
	 * If the given node has already been visited and its replacement
	 * cached, the cached value is returned.  Otherwise, null is returned.
	 * The default implementation does not perform any caching so this
	 * method simply returns null.
	 * @return this.cache[node]
	 */
	protected <N extends Node> N lookup(N node) {
		return null;
	}
	
	/**
	 * Caches the given replacement for the specified node, if this is 
	 * a caching visitor.  Otherwise does nothing.  The method returns
	 * the replacement node.  The default implemention just returns
	 * the replacement node.
	 * @effects this.cache' = this.cache ++ node->replacement || this.cache' = this.cache
	 * @return replacement
	 */
	protected <N extends Node> N cache(N node, N replacement) {
		return replacement;
	}
	
	/** 
	 * Calls lookup(decls) and returns the cached value, if any.  
	 * If a replacement has not been cached, visits each of the children's 
	 * variable and expression.  If nothing changes, the argument is cached and
	 * returned, otherwise a replacement Decls object is cached and returned.
	 * @return { d: Decls | d.size = decls.size && 
	 *                      all i: [0..d.size) | d.declarations[i].variable = decls.declarations[i].variable.accept(this) && 
	 *                                           d.declarations[i].expression = decls.declarations[i].expression.accept(this) } 
	 */
	public Decls visit(Decls decls) { 
		Decls ret = lookup(decls);
		if (ret==null) {
			final List<Decl> dlist = decls.declarations();
			final Iterator<Decl> diter = dlist.iterator();
			Decls visitedDecls = diter.next().accept(this);
			while (diter.hasNext()) {
				visitedDecls = visitedDecls.and(diter.next().accept(this));
			}
			
			ret = (dlist==visitedDecls.declarations()) ? decls : visitedDecls;
		}
		return cache(decls, ret);
	}
	
	/** 
	 * Calls lookup(decl) and returns the cached value, if any.  
	 * If a replacement has not been cached, visits the declaration's 
	 * variable and expression.  If nothing changes, the argument is cached and
	 * returned, otherwise a replacement Decl object is cached and returned.
	 * @return { d: Declaration |  d.variable = declaration.variable.accept(this) && 
	 *                             d.expression = declaration.expression.accept(this) 
	 */
	public Decl visit(Decl decl) {
		Decl ret = lookup(decl);
		if (ret==null) {
			final Variable variable = (Variable) decl.variable().accept(this);
			final Expression expression = decl.expression().accept(this);
			ret = (variable==decl.variable() && expression==decl.expression()) ?
				  decl : new Decl(variable, expression); 
		}
		return cache(decl,ret);
	}
	
	/** 
	 * Calls lookup(relation) and returns the cached value, if any.  
	 * If a replacement has not been cached, the relation is cached and
	 * returned.
	 * @return relation 
	 */
	public Expression visit(Relation relation) { 
		final Expression ret = lookup(relation);
		return ret==null ? cache(relation,relation) : ret; 
	}
	
	/** 
	 * Calls lookup(variable) and returns the cached value, if any.  
	 * If a replacement has not been cached, the variable is cached and
	 * returned.
	 * @return variable 
	 */
	public Expression visit(Variable variable) { 
		final Expression ret = lookup(variable);
		return ret==null ? cache(variable,variable) : variable; 
	}
	
	/** 
	 * Calls lookup(constExpr) and returns the cached value, if any.  
	 * If a replacement has not been cached, the constExpr is cached and
	 * returned.
	 * @return constExpr 
	 */
	public Expression visit(ConstantExpression constExpr) {
		final Expression ret = lookup(constExpr);
		return ret==null ? cache(constExpr,constExpr) : constExpr;
	}
	
	/** 
	 * Calls lookup(binExpr) and returns the cached value, if any.  
	 * If a replacement has not been cached, visits the expression's 
	 * children.  If nothing changes, the argument is cached and
	 * returned, otherwise a replacement expression is cached and returned.
	 * @return { b: BinaryExpression | b.left = binExpr.left.accept(this) &&
	 *                                 b.right = binExpr.right.accept(this) && b.op = binExpr.op }
	 */
	public Expression visit(BinaryExpression binExpr) {
		Expression ret = lookup(binExpr);
		if (ret==null) {
			final Expression left  = binExpr.left().accept(this);
			final Expression right = binExpr.right().accept(this);
			ret = (left==binExpr.left() && right==binExpr.right()) ?
				  binExpr : new BinaryExpression(left, binExpr.op(), right);
		}
		return cache(binExpr,ret);
	}
	
	/** 
	 * Calls lookup(unaryExpr) and returns the cached value, if any.  
	 * If a replacement has not been cached, visits the expression's 
	 * child.  If nothing changes, the argument is cached and
	 * returned, otherwise a replacement expression is cached and returned.
	 * @return { u: UnaryExpression | b.left = binExpr.left.accept(this) && u.op = binExpr.op }
	 */
	public Expression visit(UnaryExpression unaryExpr) {
		Expression ret = lookup(unaryExpr);
		if (ret==null) {
			final Expression child = unaryExpr.expression().accept(this);
			ret = (child==unaryExpr.expression()) ? 
				  unaryExpr : new UnaryExpression(unaryExpr.op(), child);
		}
		return cache(unaryExpr,ret);
	}
	
	/** 
	 * Calls lookup(comprehension) and returns the cached value, if any.  
	 * If a replacement has not been cached, visits the expression's 
	 * children.  If nothing changes, the argument is cached and
	 * returned, otherwise a replacement expression is cached and returned.
	 * @return { c: Comprehension | c.declarations = comprehension.declarations.accept(this) &&
	 *                              c.formula = comprehension.formula.accept(this) }
	 */
	public Expression visit(Comprehension comprehension) {
		Expression ret = lookup(comprehension);
		if (ret==null) {
			final Decls decls = (Decls)comprehension.declarations().accept(this);
			final Formula formula = comprehension.formula().accept(this);
			ret = (decls==comprehension.declarations() && formula==comprehension.formula()) ? 
				  comprehension : new Comprehension(decls, formula); 
		}
		return cache(comprehension,ret);
	}
	
	
	/** 
	 * Calls lookup(ifExpr) and returns the cached value, if any.  
	 * If a replacement has not been cached, visits the expression's 
	 * children.  If nothing changes, the argument is cached and
	 * returned, otherwise a replacement expression is cached and returned.
	 * @return { i: IfExpression | i.condition = ifExpr.condition.accept(this) &&
	 *                             i.thenExpr = ifExpr.thenExpr.accept(this) &&
	 *                             i.elseExpr = ifExpr.elseExpr.accept(this) }
	 */
	public Expression visit(IfExpression ifExpr) {
		Expression ret = lookup(ifExpr);
		if (ret==null) {
			final Formula condition = ifExpr.condition().accept(this);
			final Expression thenExpr = ifExpr.thenExpr().accept(this);
			final Expression elseExpr = ifExpr.elseExpr().accept(this);
			ret = (condition==ifExpr.condition() && thenExpr==ifExpr.thenExpr() &&
				   elseExpr==ifExpr.elseExpr()) ? 
			      ifExpr : new IfExpression(condition, thenExpr, elseExpr);
		}
		return cache(ifExpr,ret);
	}

	/**
	 * Returns the constant.
	 * @return constant
	 */
	public Formula visit(ConstantFormula constant) {
		return constant;
	}

	/** 
	 * Calls lookup(quantFormula) and returns the cached value, if any.  
	 * If a replacement has not been cached, visits the formula's 
	 * children.  If nothing changes, the argument is cached and
	 * returned, otherwise a replacement formula is cached and returned.
	 * @return { q: QuantifiedFormula | q.declarations = quantFormula.declarations.accept(this) &&
	 *                                  q.formula = quantFormula.formula.accept(this) }
	 */
	public Formula visit(QuantifiedFormula quantFormula) {
		Formula ret = lookup(quantFormula);
		if (ret==null) {
			final Decls decls = (Decls)quantFormula.declarations().accept(this);
			final Formula formula = quantFormula.formula().accept(this);
			ret = (decls==quantFormula.declarations() && formula==quantFormula.formula()) ? 
				  quantFormula : new QuantifiedFormula(quantFormula.quantifier(), decls, formula);
		}
		return cache(quantFormula,ret);
	}
	
	/** 
	 * Calls lookup(binFormula) and returns the cached value, if any.  
	 * If a replacement has not been cached, visits the formula's 
	 * children.  If nothing changes, the argument is cached and
	 * returned, otherwise a replacement formula is cached and returned.
	 * @return { b: BinaryFormula | b.left = binExpr.left.accept(this) &&
	 *                              b.right = binExpr.right.accept(this) && b.op = binExpr.op }
	 */
	public Formula visit(BinaryFormula binFormula) {
		Formula ret = lookup(binFormula);
		if (ret==null) {
			final Formula left  = binFormula.left().accept(this);
			final Formula right = binFormula.right().accept(this);
			ret = (left==binFormula.left() && right==binFormula.right()) ? 
				  binFormula : new BinaryFormula(left, binFormula.op(), right);     
		}
		return cache(binFormula,ret);
	}
	
	/** 
	 * Calls lookup(binFormula) and returns the cached value, if any.  
	 * If a replacement has not been cached, visits the formula's 
	 * child.  If nothing changes, the argument is cached and
	 * returned, otherwise a replacement formula is cached and returned.
	 * @return { n: NotFormula | n.child = not.child.accept(this) }
	 */
	public Formula visit(NotFormula not) {
		Formula ret = lookup(not);
		if (ret==null) {
			final Formula child = not.formula().accept(this);
			ret = (child==not.formula()) ? not : new NotFormula(child);
		}
		return cache(not,ret);
	}
	
	/** 
	 * Calls lookup(compFormula) and returns the cached value, if any.  
	 * If a replacement has not been cached, visits the formula's 
	 * children.  If nothing changes, the argument is cached and
	 * returned, otherwise a replacement formula is cached and returned.
	 * @return { c: ComparisonFormula | c.left = compFormula.left.accept(this) &&
	 *                                  c.right = compFormula.right.accept(this) &&
	 *                                  c.op = compFormula.op }
	 */
	public Formula visit(ComparisonFormula compFormula) {
		Formula ret = lookup(compFormula);
		if (ret==null) {
			final Expression left  = compFormula.left().accept(this);
			final Expression right = compFormula.right().accept(this);
			ret =  (left==compFormula.left() && right==compFormula.right()) ? 
				   compFormula : new ComparisonFormula(left, compFormula.op(), right);
		}
		return cache(compFormula,ret);
	}
	
	/** 
	 * Calls lookup(multFormula) and returns the cached value, if any.  
	 * If a replacement has not been cached, visits the formula's 
	 * children.  If nothing changes, the argument is cached and
	 * returned, otherwise a replacement formula is cached and returned.
	 * @return { m: MultiplicityFormula | m.multiplicity = multFormula.multiplicity &&
	 *                                    m.expression = multFormula.expression.accept(this) }
	 */
	public Formula visit(MultiplicityFormula multFormula) {
		Formula ret = lookup(multFormula);
		if (ret==null) {
			final Expression expression = multFormula.expression().accept(this);
			ret = (expression==multFormula.expression()) ? 
				  multFormula : new MultiplicityFormula(multFormula.multiplicity(), expression);
		}
		return cache(multFormula,ret);
	}
	
	/** 
	 * Calls lookup(pred) and returns the cached value, if any.  
	 * If a replacement has not been cached, visits the formula's 
	 * children.  If nothing changes, the argument is cached and
	 * returned, otherwise a replacement formula is cached and returned.
	 * @return { p: RelationPredicate | p.name = pred.name && p.relation = pred.relation.accept(this) &&
	 *                                  p.name = FUNCTION => p.targetMult = pred.targetMult && 
	 *                                                       p.domain = pred.domain.accept(this) &&
	 *                                                       p.range = pred.range.accept(this),
	 *                                  p.name = TOTAL_ORDERING => p.ordered = pred.ordered.accept(this) &&
	 *                                                             p.first = pred.first.accept(this) &&
	 *                                                             p.last = pred.last.accept(this) }
	 */
	public Formula visit(RelationPredicate pred) {
		Formula ret = lookup(pred);
		if (ret==null) {
			final Relation r = (Relation)pred.relation().accept(this);
			switch(pred.name()) {
			case ACYCLIC :  ret = new RelationPredicate.Acyclic(r); break;
			case FUNCTION :
				final RelationPredicate.Function fp = (RelationPredicate.Function) pred;
				ret = new RelationPredicate.Function(r, fp.domain().accept(this), 
						                                fp.targetMult(), fp.range().accept(this));
				break;
			case TOTAL_ORDERING : 
				final RelationPredicate.TotalOrdering tp = (RelationPredicate.TotalOrdering) pred;
				ret = new RelationPredicate.TotalOrdering(r, (Relation)tp.ordered().accept(this), 
						(Relation)tp.first().accept(this), (Relation)tp.last().accept(this));
				break;
			default :
				throw new IllegalArgumentException("unknown relation predicate: " + pred.name());
			}
			
		}
		return cache(pred,ret);
	}
	
}
