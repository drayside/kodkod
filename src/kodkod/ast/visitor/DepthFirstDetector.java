/**
 * 
 */
package kodkod.ast.visitor;

import kodkod.ast.BinaryExpression;
import kodkod.ast.BinaryFormula;
import kodkod.ast.ComparisonFormula;
import kodkod.ast.Comprehension;
import kodkod.ast.ConstantExpression;
import kodkod.ast.ConstantFormula;
import kodkod.ast.Decl;
import kodkod.ast.Decls;
import kodkod.ast.IfExpression;
import kodkod.ast.IntComparisonFormula;
import kodkod.ast.IntConstant;
import kodkod.ast.MultiplicityFormula;
import kodkod.ast.Node;
import kodkod.ast.NotFormula;
import kodkod.ast.QuantifiedFormula;
import kodkod.ast.Relation;
import kodkod.ast.RelationPredicate;
import kodkod.ast.UnaryExpression;
import kodkod.ast.Cardinality;
import kodkod.ast.Variable;

/**
 * <p>A depth first detector.  Subclasses should override the
 * methods in which detection is performed to return TRUE.
 * For example, a Variable detector could be implemented
 * simply by subclassing this implementation and overriding
 * the {@link #visit(Variable) } method to return TRUE.</p>
 * 
 * @specfield cached: set Node // result of visiting these nodes will be cached
 * @specfield cache: cached -> lone Boolean
 * @author Emina Torlak
 */
public abstract class DepthFirstDetector implements ReturnVisitor<Boolean, Boolean, Boolean, Boolean> {
	
	/**
	 * Constructs a depth first detector 
	 */
	protected DepthFirstDetector() {}
	
	/**
	 * If n has been visited and a value for it cached,
	 * the cached value is returned. Otherwise null is returned.
	 * @return this.cache[n]
	 */
	protected abstract Boolean lookup(Node n) ;
	
	/**
	 * Caches the given value for the specified node, if
	 * this is a caching visitor, and returns Boolean.valueOf(val).
	 * @effects n in this.cached => this.cache' = this.cache ++ n->Boolean.valueOf(val), this.cache' = this.cache
	 * @return Boolean.valueOf(val)
	 */
	protected abstract Boolean cache(Node n, boolean val) ;
	
	/** 
	 * Calls lookup(decls) and returns the cached value, if any.  
	 * If no cached value exists, visits each child, caches the
	 * disjunction of the children's return values and returns it. 
	 * @return let x = lookup(decls) | 
	 *          x != null => x,  
	 *          cache(decls, some d: decls.declarations | d.accept(this)) 
	 */
	public Boolean visit(Decls decls) {
		final Boolean ret = lookup(decls);
		if (ret==null) {
			for(Decl d : decls) {
				if (visit(d))
					return cache(decls, true);
			}
			return cache(decls, false);
		} else {
			return ret;
		}
	}
	
	/** 
	 * Calls lookup(decl) and returns the cached value, if any.  
	 * If no cached value exists, visits each child, caches the
	 * disjunction of the children's return values and returns it. 
	 * @return let x = lookup(decl) | 
	 *          x != null => x,  
	 *          cache(decl, decl.variable.accept(this) || decl.expression.accept(this)) 
	 */
	public Boolean visit(Decl decl) {
		final Boolean ret = lookup(decl);
		if (ret==null) {
			return cache(decl, decl.variable().accept(this) || decl.expression().accept(this));
		} else {
			return ret;
		}
	}
	
	/**
	 * Returns FALSE.
	 * @return FALSE
	 */
	public Boolean visit(Relation relation) {
		return Boolean.FALSE;
	}
	
	/**
	 * Returns FALSE.
	 * @return FALSE
	 */
	public Boolean visit(Variable variable) {
		return Boolean.FALSE;
	}
	
	/**
	 * Returns FALSE.
	 * @return FALSE
	 */
	public Boolean visit(ConstantExpression constExpr) {
		return Boolean.FALSE;
	}
	
	/** 
	 * Calls lookup(binExpr) and returns the cached value, if any.  
	 * If no cached value exists, visits each child, caches the
	 * disjunction of the children's return values and returns it. 
	 * @return let x = lookup(binExpr) | 
	 *          x != null => x,  
	 *          cache(binExpr, binExpr.left.accept(this) || binExpr.right.accept(this)) 
	 */
	public Boolean visit(BinaryExpression binExpr) {
		final Boolean ret = lookup(binExpr);
		if (ret==null) {
			return cache(binExpr, binExpr.left().accept(this) || binExpr.right().accept(this));
		} else {
			return ret;
		}
	}
	
	/** 
	 * Calls lookup(unaryExpr) and returns the cached value, if any.  
	 * If no cached value exists, visits the child, caches its return value and returns it. 
	 * @return let x = lookup(unaryExpr) | 
	 *          x != null => x,  
	 *          cache(unaryExpr, unaryExpr.expression.accept(this)) 
	 */
	public Boolean visit(UnaryExpression unaryExpr) {
		final Boolean ret = lookup(unaryExpr);
		if (ret==null)
			return cache(unaryExpr, unaryExpr.expression().accept(this));
		else 
			return ret;
	}
	
	/** 
	 * Calls lookup(comprehension) and returns the cached value, if any.  
	 * If no cached value exists, visits each child, caches the
	 * disjunction of the children's return values and returns it. 
	 * @return let x = lookup(comprehension) | 
	 *          x != null => x,  
	 *          cache(comprehension, comprehension.declarations.accept(this) || comprehension.formula.accept(this)) 
	 */
	public Boolean visit(Comprehension comprehension) {
		final Boolean ret = lookup(comprehension);
		if (ret==null) {
			return cache(comprehension, comprehension.declarations().accept(this) || 
					comprehension.formula().accept(this));
		} else {
			return ret;
		}
	}
	
	/** 
	 * Calls lookup(ifExpr) and returns the cached value, if any.  
	 * If no cached value exists, visits each child, caches the
	 * disjunction of the children's return values and returns it. 
	 * @return let x = lookup(ifExpr) | 
	 *          x != null => x,  
	 *          cache(ifExpr, ifExpr.condition.accept(this) || ifExpr.thenExpr.accept(this) || ifExpr.elseExpr.accept(this)) 
	 */
	public Boolean visit(IfExpression ifExpr) {
		final Boolean ret = lookup(ifExpr);
		if (ret==null) {
			return cache(ifExpr, ifExpr.condition().accept(this) || 
					ifExpr.thenExpr().accept(this) || ifExpr.elseExpr().accept(this));
		} else {
			return ret;
		}
	}
	
	/**
	 * Returns FALSE.
	 * @return FALSE
	 */
	public Boolean visit(IntConstant intConst) {
		return Boolean.FALSE;
	}
	
	/** 
	 * Calls lookup(intExpr) and returns the cached value, if any.  
	 * If no cached value exists, visits the child, caches its return value and returns it. 
	 * @return let x = lookup(intExpr) | 
	 *          x != null => x,  
	 *          cache(intExpr, intExpr.expression.accept(this)) 
	 */
	public Boolean visit(Cardinality intExpr) {
		final Boolean ret = lookup(intExpr);
		if (ret==null)
			return cache(intExpr, intExpr.expression().accept(this));
		else 
			return ret;
	}
	
	/** 
	 * Calls lookup(intComp) and returns the cached value, if any.  
	 * If no cached value exists, visits each child, caches the
	 * disjunction of the children's return values and returns it. 
	 * @return let x = lookup(intComp) | 
	 *          x != null => x,  
	 *          cache(intComp, intComp.left.accept(this) || intComp.right.accept(this)) 
	 */
	public Boolean visit(IntComparisonFormula intComp) {
		final Boolean ret = lookup(intComp);
		if (ret==null) {
			return cache(intComp, intComp.left().accept(this) || intComp.right().accept(this));
		} else {
			return ret;
		}
	}
	
	/** 
	 * Calls lookup(quantFormula) and returns the cached value, if any.  
	 * If no cached value exists, visits each child, caches the
	 * disjunction of the children's return values and returns it. 
	 * @return let x = lookup(quantFormula) | 
	 *          x != null => x,  
	 *          cache(quantFormula, quantFormula.declarations.accept(this) ||quantFormula.formula.accept(this)) 
	 */
	public Boolean visit(QuantifiedFormula quantFormula) {
		final Boolean ret = lookup(quantFormula);
		if (ret==null) {
			return cache(quantFormula, quantFormula.declarations().accept(this) || 
					quantFormula.formula().accept(this));
		} else {
			return ret;
		}
	}
	
	/** 
	 * Calls lookup(binFormula) and returns the cached value, if any.  
	 * If no cached value exists, visits each child, caches the
	 * disjunction of the children's return values and returns it. 
	 * @return let x = lookup(binFormula) | 
	 *          x != null => x,  
	 *          cache(binFormula, binFormula.left.accept(this) || binFormula.right.accept(this)) 
	 */
	public Boolean visit(BinaryFormula binFormula) {
		final Boolean ret = lookup(binFormula);
		if (ret==null) {
			return cache(binFormula, binFormula.left().accept(this) || binFormula.right().accept(this));
		} else {
			return ret;
		}
	}
	
	/** 
	 * Calls lookup(not) and returns the cached value, if any.  
	 * If no cached value exists, visits the child, caches its return value and returns it. 
	 * @return let x = lookup(not) | 
	 *          x != null => x,  
	 *          cache(not, not.formula.accept(this)) 
	 */
	public Boolean visit(NotFormula not) {
		final Boolean ret = lookup(not);
		if (ret==null)
			return cache(not, not.formula().accept(this));
		else 
			return ret;
	}
	
	/**
	 * Returns FALSE.
	 * @return FALSE
	 */
	public Boolean visit(ConstantFormula constant) {
		return Boolean.FALSE;
	}
	
	/** 
	 * Calls lookup(compFormula) and returns the cached value, if any.  
	 * If no cached value exists, visits each child, caches the
	 * disjunction of the children's return values and returns it. 
	 * @return let x = lookup(compFormula) | 
	 *          x != null => x,  
	 *          cache(compFormula,compFormula.left.accept(this) || compFormula.right.accept(this)) 
	 */
	public Boolean visit(ComparisonFormula compFormula) {
		final Boolean ret = lookup(compFormula);
		if (ret==null) {
			return cache(compFormula, compFormula.left().accept(this) || compFormula.right().accept(this));
		} else {
			return ret;
		}
	}
	
	/** 
	 * Calls lookup(multFormula) and returns the cached value, if any.  
	 * If no cached value exists, visits the child, caches its return value and returns it. 
	 * @return let x = lookup(multFormula) | 
	 *          x != null => x,  
	 *          cache(multFormula, multFormula.expression.accept(this)) 
	 */
	public Boolean visit(MultiplicityFormula multFormula) {
		final Boolean ret = lookup(multFormula);
		if (ret==null)
			return cache(multFormula, multFormula.expression().accept(this));
		else 
			return ret;
	}
	
	/** 
	 * Calls lookup(compFormula) and returns the cached value, if any.  
	 * If no cached value exists, visits each child, caches the
	 * disjunction of the children's return values and returns it. 
	 * @return let x = lookup(predicate) | 
	 *          x != null => x,  
	 *          cache(predicate, some n: predicate.children | n.accept(this)) 
	 */
	public Boolean visit(RelationPredicate predicate) {
		final Boolean ret = lookup(predicate);
		if (ret==null) {
			boolean r = predicate.relation().accept(this);
			if (predicate.name()==RelationPredicate.Name.FUNCTION) {
				final RelationPredicate.Function fp = (RelationPredicate.Function) predicate;
				r = r || fp.domain().accept(this) || fp.range().accept(this);
			} else if (predicate.name()==RelationPredicate.Name.TOTAL_ORDERING) {
				final RelationPredicate.TotalOrdering tp = (RelationPredicate.TotalOrdering) predicate;
				r = r || tp.ordered().accept(this) || tp.first().accept(this) || tp.last().accept(this);
			}
			return cache(predicate, r);
		} else { 
			return ret;
		}
	}
	
}
