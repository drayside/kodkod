package tests;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

import kodkod.ast.Decls;
import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.Relation;
import kodkod.ast.Variable;
import kodkod.engine.Solution;
import kodkod.engine.Solver;
import kodkod.engine.TimeoutException;
import kodkod.engine.satlab.SATFactory;
import kodkod.instance.Bounds;
import kodkod.instance.TupleFactory;
import kodkod.instance.TupleSet;
import kodkod.instance.Universe;

public final class OrdSharing {
private final Relation A, r, ord;
	
	/**
	 * Creates an instance of Toughnut.
	 */
	OrdSharing() {
		this.A = Relation.unary("A");
		this.r = Relation.nary("r",4);
		this.ord = Relation.binary("ord");
	}
	
	private Expression next(Expression e) {
		return e.join(ord);
	}
	
	private Expression prev(Expression e) {
		return ord.join(e);
	}
	
	Formula show1() {
		final Variable x = Variable.unary("x");
		final Variable y = Variable.unary("y");
		final Decls d = x.oneOf(A).and(y.oneOf(A));
		Expression yxr = y.join(x.join(r));
		Formula show = yxr.in(x.join(y.join(r))).forAll(d);
		return show;
	}
	
	Formula show2() {
		final Variable x = Variable.unary("x");
		final Variable y = Variable.unary("y");
		final Decls d = x.oneOf(A).and(y.oneOf(A));
		Expression yxr = y.join(x.join(r));
		Expression pxnxy = (prev(x).union(next(x))).product(y);
		Expression xpyny = x.product(prev(y).union(next(y)));
		Formula show = yxr.one().and(yxr.in(pxnxy.union(xpyny))).forAll(d);
		return show;
	}
	
	Formula show3() {
		final Variable x = Variable.unary("x");
		final Variable y = Variable.unary("y");
		final Decls d = x.oneOf(A).and(y.oneOf(A));
		Expression yxr = y.join(x.join(r));
		Formula show = yxr.one().forAll(d);
		return show;
	}
	
	Formula show4() {
		final Variable x = Variable.unary("x");
		final Variable y = Variable.unary("y");
		final Decls d = x.oneOf(A).and(y.oneOf(A));
		Expression yxr = y.join(x.join(r));
		Expression pxnxy = (prev(x).union(next(x))).product(y);
		Expression xpyny = x.product(prev(y).union(next(y)));
		Formula show = yxr.in(pxnxy.union(xpyny)).forAll(d);
		return show;
	}
	
	Formula show5() {
		final Variable x = Variable.unary("x");
		final Variable y = Variable.unary("y");
		final Decls d = x.oneOf(A).and(y.oneOf(A));
		Expression yxr = y.join(x.join(r));
		Formula show = yxr.one().and(yxr.product(x).product(y).in(r)).forAll(d);
		return show;
	}
	
	Formula show6() {
		final Variable x = Variable.unary("x");
		final Variable y = Variable.unary("y");
		final Decls d = x.oneOf(A).and(y.oneOf(A));
		Expression yxr = y.join(x.join(r));
		Formula show = yxr.one().and(yxr.in(x.product(y))).forAll(d);
		return show;
	}
	
	/**
	 * Returns bounds for an nxn board.
	 * @return bounds for an nxn board.
	 */
	Bounds bounds(int n) {
		assert n > 0;
		final List<String> atoms = new ArrayList<String>(n);
		for(int i = 0; i < n; i++) {
			atoms.add(String.valueOf(i));
		}
		final Universe u = new Universe(atoms);
		final Bounds b = new Bounds(u);
		final TupleFactory f = u.factory();
		
		b.bound/*Exactly*/(A, f.allOf(1));
		
		final TupleSet ordBound = f.noneOf(2);
		for(int i = 0; i < n-1; i++) {
			ordBound.add(f.tuple(String.valueOf(i), String.valueOf(i+1)));
		}
		b.boundExactly(ord, ordBound);
	
		
		b.bound(r, f.allOf(4)); 
		
		return b;
	}
	
	
	public static void main(String[] args) {
		int n = args.length==0 ? 4 : Integer.parseInt(args[0]);
		String command = args.length < 2 ? "show1" : args[1];
		final OrdSharing model = new OrdSharing();
		final Solver solver = new Solver();
		solver.options().setSolver(SATFactory.ZChaff);
		solver.options().setSymmetryBreaking(0);
		
		try {
			final Method method = model.getClass().getDeclaredMethod(command, new Class[0]);
			final Formula formula = (Formula) method.invoke(model, new Object[0]);
			final Bounds bounds = model.bounds(n);
			
			System.out.println(formula);
			//System.out.println(bounds);
			
			Solution sol = solver.solve(formula, bounds);
			System.out.println(sol);
		} catch (SecurityException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (NoSuchMethodException e) {
			System.out.println("Invalid command: " + command);
		} catch (TimeoutException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IllegalArgumentException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IllegalAccessException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (InvocationTargetException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
	}
}

