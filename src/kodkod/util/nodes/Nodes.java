/* 
 * Kodkod -- Copyright (c) 2005-2008, Emina Torlak
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
package kodkod.util.nodes;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Set;

import kodkod.ast.BinaryExpression;
import kodkod.ast.BinaryFormula;
import kodkod.ast.BinaryIntExpression;
import kodkod.ast.BinaryOperator;
import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.IntExpression;
import kodkod.ast.Node;
import kodkod.ast.visitor.AbstractReplacer;
import kodkod.util.collections.Containers;

/**
 * Provides utility methods for constructing balanced
 * Kodkod trees using associative operators.
 * 
 * @author Emina Torlak
 */
public final class Nodes {
	private Nodes() {}
	
	/**
     * Returns the roots of the given formula.
     * In other words, breaks up the given formula into its conjunctive 
     * components, {f0, ..., fk}, 
     * such that, for all 0<=i<=k, f<sub>i</sub> is not a conjunction  and
     * [[f0 && ... && fk]] <=> [[formula]].  
     * @return subformulas, {f0, ..., fk}, of the given formula such that, for all 0<=i<=k, 
     * f<sub>i</sub> is not a conjuction and [[f0 && ... && fk]] <=> [[formula]].    
     */
	public static Set<Formula> roots(Formula formula) {
	
    	final List<Formula> formulas = new LinkedList<Formula>();
		formulas.add(formula);
		
		int size;
		do {
			size = formulas.size();
			ListIterator<Formula> itr = formulas.listIterator();
			while(itr.hasNext()) {
				Formula f = itr.next();
				if (f instanceof BinaryFormula) {
					BinaryFormula bin = (BinaryFormula) f;
					if (bin.op()==BinaryFormula.Operator.AND) {
						itr.remove();
						itr.add(bin.left());
						itr.add(bin.right());
					}
				}
			}
		} while (formulas.size() > size);
		
		return new LinkedHashSet<Formula>(formulas);
	}
	
	/**
	 * Returns the conjunction of the given arguments, or Formula.TRUE
	 * if args is empty.  The result of this method is the same as calling
	 * Nodes.apply(BinaryFormula.Operator.AND, Formula.TRUE, args).
	 * @return args.isEmpty() => Formula.TRUE else { t: T | [[t]] = AND [[args]] }
	 */
	public static Formula and(Collection<Formula> args) { 
		return apply(BinaryFormula.Operator.AND, Formula.TRUE, args);
	}
	
	/**
	 * Returns the conjunction of the given arguments, or Formula.TRUE
	 * if args is empty.  The result of this method is the same as calling
	 * Nodes.apply(BinaryFormula.Operator.AND, Formula.TRUE, args).
	 * @return args.length = 0 => Formula.TRUE else { t: T | [[t]] = AND [[args]] }
	 */
	public static Formula and(Formula... args) { 
		return apply(BinaryFormula.Operator.AND, Formula.TRUE, args);
	}
	
	/**
	 * Returns the disjunction of the given arguments, or Formula.FALSE
	 * if args is empty.  The result of this method is the same as calling
	 * Nodes.apply(BinaryFormula.Operator.OR, Formula.FALSE, args).
	 * @return args.isEmpty() => Formula.FALSE else { t: T | [[t]] = OR [[args]] }
	 */
	public static Formula or(Collection<Formula> args) { 
		return apply(BinaryFormula.Operator.OR, Formula.FALSE, args);
	}
	
	/**
	 * Returns the disjunction of the given arguments, or Formula.FALSE
	 * if args is empty.  The result of this method is the same as calling
	 * Nodes.apply(BinaryFormula.Operator.OR, Formula.FALSE, args).
	 * @return args.length = 0 => Formula.FALSE else { t: T | [[t]] = OR [[args]] }
	 */
	public static Formula or(Formula... args) { 
		return apply(BinaryFormula.Operator.OR, Formula.FALSE, args);
	}
	
	/**
	 * Returns the result of applying the given binary operator to the specified
	 * arguments, or null if args is empty.  The returned tree is guaranteed to 
	 * be balanced (with respect to the nodes in args).
	 * @requires op.associative()
	 * @requires args can be combined using the given operator
	 * @return args.isEmpty() => null else { t: T | [[t]] = [[op]] [[args]] }
	 */
	public static <T> T apply(BinaryOperator<T,T> op, Collection<T> args) { 
		return apply(op, null, args);
	}
	
	/**
	 * Returns the result of applying the given binary operator to the specified
	 * arguments, or the given defaultValue if args is empty.  The returned tree is guaranteed to 
	 * be balanced (with respect to the nodes in args).
	 * @requires op.associative()
	 * @requires args can be combined using the given operator
	 * @return args.isEmpty() => defaultValue else { t: T | [[t]] = [[op]] [[args]] }
	 */
	public static <T> T apply(BinaryOperator<T,T> op, T defaultValue, Collection<T> args) { 
		if (!op.associative()) throw new IllegalArgumentException(op + " is not associative.");
		return apply(op, defaultValue, args.iterator(), 0, args.size());
	}
	
	/**
	 * Returns the result of applying the given binary operator to the specified
	 * arguments, or null if args is empty.  The returned tree is guaranteed to 
	 * be balanced (with respect to the nodes in args).
	 * @requires op.associative()
	 * @requires args can be combined using the given operator
	 * @return args.length = 0 => null else { t: T | [[t]] = [[op]] [[args]] }
	 */
	public static <T> T apply(BinaryOperator<T,T> op, T... args) { 
		return apply(op, null, args);
	}
	
	/**
	 * Returns the result of applying the given binary operator to the specified
	 * arguments, or the given defaultValue if args is empty.  The returned tree is guaranteed to 
	 * be balanced (with respect to the nodes in args).
	 * @requires op.associative()
	 * @requires args can be combined using the given operator
	 * @return args.length = 0 => defaultValue else { t: T | [[t]] = [[op]] [[args]] }
	 */
	public static <T> T apply(BinaryOperator<T,T> op, T defaultValue, T... args) { 
		if (!op.associative()) throw new IllegalArgumentException(op + " is not associative.");
		return apply(op, defaultValue, Containers.iterate(args), 0, args.length);
	}
	
	/**
	 * Returns the result of applying the given binary operator to the specified
	 * arguments, or the given defaultValue if args is empty.  The returned tree is guaranteed to 
	 * be balanced (with respect to the nodes in args).
	 * @requires op.associative()
	 * @requires args can be combined using the given operator
	 * @requires args.next() can be called end - start times
	 * @return start == end => defaultValue else { t: T | [[t]] = [[op]] [[args]] }
	 */
	private static <T> T apply(BinaryOperator<T,T> op, T defaultValue, Iterator<T> args, int start, int end) { 
		final int length = end - start;

		switch(length) {
		case 0: return defaultValue;
		case 1: return args.next();
		default :
			final int mid = start + (length / 2);
			final T left = apply(op, defaultValue, args, start, mid);
			final T right = apply(op, defaultValue, args, mid, end);
			return op.apply(left, right);
		}
	}
	
	/**
	 * Balances the binary components of the given node and
	 * returns the resulting (semantically equivalent) node.
	 * Balancing preserves sharing relationships between nodes.
	 * @return balanced version of the given node
	 */
	@SuppressWarnings("unchecked")
	public static <N extends Node> N balance(N node) { 
		return (N) node.accept(new Balancer(new AnnotatedNode<N>(node)));
	}
	
	/**
	 * Balances the binary components in a given Kodkod node.
	 * @specfield node: AnnotatedNode<?> // node to be re-balanced
	 * @author Emina Torlak
	 */
	private static final class Balancer extends AbstractReplacer {

		/**
		 * Constructs a new balancer for the given annotated node.
		 */
		protected Balancer(AnnotatedNode<?> annotated) {
			super(annotated.sharedNodes());
		}

		/**
		 * An interface that allows uniform access to homogenous binary nodes.
		 * @author Emina Torlak
		 */
		private static interface Decomposer<N extends Node, B extends N> { 
			abstract N left(B node);
			abstract N right(B node);
			abstract BinaryOperator<N, N> op(B node);
			abstract boolean decomposable(N node);
		}
		
		private final Decomposer<Formula,BinaryFormula> FDECOMP = 
			new Decomposer<Formula,BinaryFormula>() {
				public Formula left(BinaryFormula node) { return node.left(); }
				public BinaryOperator<Formula, Formula> op(BinaryFormula node) { return node.op(); }
				public Formula right(BinaryFormula node) { return node.right(); }
				public boolean decomposable(Formula node) { return node instanceof BinaryFormula; }
		};
		
		private final Decomposer<Expression,BinaryExpression> EDECOMP = 
			new Decomposer<Expression,BinaryExpression>() {
				public Expression left(BinaryExpression node) { return node.left(); }
				public BinaryOperator<Expression, Expression> op(BinaryExpression node) { return node.op(); }
				public Expression right(BinaryExpression node) { return node.right(); }
				public boolean decomposable(Expression node) { return node instanceof BinaryExpression; }
		};
		
		private final Decomposer<IntExpression,BinaryIntExpression> IDECOMP = 
			new Decomposer<IntExpression,BinaryIntExpression>() {
				public IntExpression left(BinaryIntExpression node) { return node.left(); }
				public BinaryOperator<IntExpression, IntExpression> op(BinaryIntExpression node) { return node.op(); }
				public IntExpression right(BinaryIntExpression node) { return node.right(); }
				public boolean decomposable(IntExpression node) { return node instanceof BinaryIntExpression; }
		};
		
		/**
		 * Decomposes the given node with respect to the given 
		 * associative operator, if possible, using the specified decomposer.  Otherwise
		 * returns a List containing the argument node.
		 * @requires op.associative()
		 * @return components of the given node with respect to the given 
		 * associative operator and decomposer. 
		 */
		@SuppressWarnings("unchecked")
		<N extends Node, B extends N> List<N> decompose(N node, BinaryOperator<N,N> op, Decomposer<N, B> decomposer) {
			final List<N> children = new LinkedList<N>();
			children.add(node);
			int size;
			do {
				size = children.size();
				ListIterator<N> itr = children.listIterator();
				while(itr.hasNext()) {
					N child = itr.next();
					if (decomposer.decomposable(child)) {
						B bin = (B) child;
						if (decomposer.op(bin)==op && !cached.contains(bin)) {
							itr.remove();
							itr.add(decomposer.left(bin));
							itr.add(decomposer.right(bin));
						}
					}
				}
			} while (children.size() > size);
			return children;
		}
		
		/**
		 * Visits a composite binary node.
		 * @return balanced version of the given node.
		 */
		@SuppressWarnings("unchecked")
		<N extends Node, B extends N> N visit(B node, Decomposer<N,B> decomposer) { 
			N ret = lookup(node);
			if (ret != null) return ret;
		
			final N left = decomposer.left(node);
			final N right = decomposer.right(node);
			final BinaryOperator<N, N> op = decomposer.op(node);
			
			final List<N> children = new LinkedList<N>();
			
			if (op.associative()) { 
				final List<N> lchildren = decompose(left, op, decomposer);
				final List<N> rchildren = decompose(right, op, decomposer);
				if (Math.abs(lchildren.size() - rchildren.size()) <= 1) { // already balanced
					children.add(left);
					children.add(right);
				} else {
					children.addAll(lchildren);
					children.addAll(rchildren);
				}
			} else {
				children.add(left);
				children.add(right);
			}
			
			for(ListIterator<N> itr = children.listIterator(); itr.hasNext(); ) {
				N child = itr.next();
				itr.set((N)child.accept(this));
			}
			
			if (children.size()==2) { 
				final N vleft = children.get(0);
				final N vright = children.get(1);
				ret = (left==vleft && right==vright) ? node : op.apply(vleft,vright);     
			} else {
				ret = apply(op, children);
			}
			
			return cache((N)node, ret);
		}
		
		/**
		 * {@inheritDoc}
		 * @see kodkod.ast.visitor.AbstractReplacer#visit(kodkod.ast.BinaryFormula)
		 */
		public Formula visit(BinaryFormula formula) { 
			return visit(formula, FDECOMP);
		} 
		
		/**
		 * {@inheritDoc}
		 * @see kodkod.ast.visitor.AbstractReplacer#visit(kodkod.ast.BinaryExpression)
		 */
		public Expression visit(BinaryExpression expr) { 
			return visit(expr, EDECOMP);
		}
		
		/**
		 * {@inheritDoc}
		 * @see kodkod.ast.visitor.AbstractReplacer#visit(kodkod.ast.BinaryIntExpression)
		 */
		public IntExpression visit(BinaryIntExpression intExpr) { 
			return visit(intExpr, IDECOMP);
		}
		
	}
	
}
