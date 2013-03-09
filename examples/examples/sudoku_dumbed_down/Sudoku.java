package examples.sudoku_dumbed_down;

import static kodkod.ast.Formula.and;
import static kodkod.ast.Relation.ternary;
import static kodkod.ast.Relation.unary;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import kodkod.ast.Decls;
import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.Relation;
import kodkod.ast.Variable;
import kodkod.engine.Solution;
import kodkod.engine.Solver;
import kodkod.engine.satlab.SATFactory;
import kodkod.instance.Bounds;
import kodkod.instance.Tuple;
import kodkod.instance.TupleFactory;
import kodkod.instance.TupleSet;

public final class Sudoku {
	private final Relation number = unary("number"), grid  = ternary("grid");
	private final Relation[] region;
	
	public Sudoku(final int r) { 
		if (r < 2) throw new IllegalArgumentException("r must be greater than 1:  r=" + r);
		region = new Relation[r];
		for(int i =0; i < r; i++) { 
			region[i] = Relation.unary("r"+(i+1));
		}
	}
	
	public final Relation grid() { return grid; }
	
	public final Relation number() { return number; }
	
	public final Relation region(int i) { return region[i]; }
	
	private final Expression grid(Expression x, Expression y) { 
		return y.join(x.join(grid));
	}
	
	private final Formula complete(Expression rows, Expression cols) {
		return number.in(grid(rows,cols));
	}

	public final Formula slowRules() { 
		final List<Formula> rules = new ArrayList<Formula>(3+region.length*region.length);

		final Variable x = Variable.unary("x"), y = Variable.unary("y");

		rules.add( grid(x,y).one().forAll(x.oneOf(number).and(y.oneOf(number))) );
		rules.add( complete(x, number).forAll(x.oneOf(number)) );
		rules.add( complete(number, y).forAll(y.oneOf(number)) );
	
		for(Relation rx : region) { 
			for(Relation ry: region) { 
				rules.add( complete(rx, ry) );
			}
		}
		return and(rules); 
	}
		
	public final Formula rules() { 
		final List<Formula> rules = new ArrayList<Formula>(3+region.length*region.length);

		final Variable x = Variable.unary("x"), y = Variable.unary("y");
		final Decls decls = x.oneOf(number).and(y.oneOf(number));

		rules.add( grid(x,y).some().forAll(decls) );
		rules.add( grid(x,y).intersection(grid(x, number.difference(y))).no().forAll(decls) );	
		rules.add( grid(x,y).intersection(grid(number.difference(x), y)).no().forAll(decls) );
	
		for(Relation rx : region) { 
			for(Relation ry: region) { 
				rules.add( grid(x, y).intersection(grid(rx.difference(x),ry.difference(y))).no().forAll(x.oneOf(rx).and(y.oneOf(ry))) );
			}
		}
		
		return and(rules); 
	}
	
	public final Formula fastRules() { 
		final List<Formula> rules = new ArrayList<Formula>(3+region.length*region.length);

		final Variable x = Variable.unary("x"), y = Variable.unary("y");
		final Decls decls = x.oneOf(number).and(y.oneOf(number));

		rules.add( grid(x,y).one().forAll(decls) );
		rules.add( grid(x,y).intersection(grid(x, number.difference(y))).no().forAll(decls) );	
		rules.add( grid(x,y).intersection(grid(number.difference(x), y)).no().forAll(decls) );
	
		for(Relation rx : region) { 
			for(Relation ry: region) { 
				rules.add( complete(rx, ry) );
			}
		}
		return and(rules); 
	}
	
	public static final TupleSet defaultPuzzle() { 
		return SudokuParser.parse("600200050018060020003000400000607800402050000000908000504090300020000014300005007");
	}
		
	public final Bounds bounds(TupleSet clues) { 
		final int r = region.length;
		final int n = r*r;
		
		final Bounds bounds = new Bounds(clues.universe());
		final TupleFactory f = bounds.universe().factory();
		
		bounds.boundExactly(number, f.range(f.tuple(1), f.tuple(n)));
		for(int i = 0; i < r; i++) { 
			bounds.boundExactly(region[i], f.range(f.tuple(i*r+1), f.tuple((i+1)*r)));
		}
		
		final TupleSet givens = clues.clone();
		final TupleSet upper = f.area(f.tuple(1, 1, 1), f.tuple(9, 9, 9));
		for(Tuple t : clues) { 
			final int x = (Integer)t.atom(0), y = (Integer)t.atom(1), v = (Integer)t.atom(2);
			for(int i = 1; i<=n; i++ ) {
				if (v!=i) upper.remove(f.tuple(x, y, i));
			}
		}
		
		bounds.bound(grid, givens, upper);
		return bounds;
	}
		
	private void solve(TupleSet clues) { 
		final Solver solver = new Solver();
		
		solver.options().setSolver(SATFactory.MiniSatProver);
		solver.options().setLogTranslation(1);
				
		Formula rules = rules();
        Bounds bounds = bounds(clues);
        Solution sol = solver.solve(rules, bounds);
        System.out.println(sol);
		if (sol.instance()!=null) { 
			System.out.println(SudokuParser.prettyPrint(sol.instance().tuples(grid)));
		} else {
		}
	}
	
	private static void usage() { 
		System.out.println("Usage: java examples.sudoku.Sudoku [-core=<oce|rce|sce|nce>] [puzzle]");
		System.exit(1);
	}
		
	/**
	 * Usage: java examples.sudoku.Sudoku [-core=<oce|rce|sce|nce>] [puzzle]
	 */
	public static void main(String[] args) {
		try {
			final TupleSet clues = args.length==0 ? defaultPuzzle() : SudokuParser.parse(args[args.length-1]);
			System.out.println(clues.universe().size());
			final Map<String,String> opts = SudokuParser.options(args);
			int r = 3; //(int)Math.sqrt(clues.universe().size());
            (new Sudoku(r)).solve(clues);
		} catch (IllegalArgumentException iae) { 
			throw iae;
//			usage();
		}
	}
}



