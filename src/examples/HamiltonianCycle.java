/**
 * 
 */
package examples;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import kodkod.ast.Formula;
import kodkod.ast.Relation;
import kodkod.ast.Variable;
import kodkod.engine.Evaluator;
import kodkod.engine.Solution;
import kodkod.engine.Solver;
import kodkod.engine.config.ConsoleReporter;
import kodkod.engine.satlab.SATFactory;
import kodkod.instance.Bounds;
import kodkod.instance.Instance;
import kodkod.instance.TupleFactory;
import kodkod.instance.TupleSet;
import kodkod.instance.Universe;

/**
 * Reads a graph from a file, formatted as described at
 * http://asparagus.cs.uni-potsdam.de/?action=instances&id=30,
 * and finds a Hamiltonian cycle in it if one exists.
 * 
 * @author Emina Torlak
 */
public final class HamiltonianCycle {

	private final Relation vertex, start, edges, cycle;
	
	/**
	 * Constructs an instance of the encoding.
	 */
	public HamiltonianCycle() {
		this.vertex = Relation.unary("Vertex");
		this.start = Relation.unary("start");
		this.edges = Relation.binary("edges");
		this.cycle = Relation.binary("cycle");
	}
	
	/**
	 * Returns a formula that defines a Hamiltonian cycle.
	 * @return a formula that defines a Hamiltonian cycle
	 */
	public Formula cycleDefinition() {
		final Formula f0 = cycle.function(edges.join(vertex), vertex.join(edges));
		
		final Variable v = Variable.unary("v");
		final Formula f1 = v.in(start.join(cycle.closure())).forAll(v.oneOf(vertex));
	
		return f0.and(f1);
	}
	
	/**
//	 * Returns an alternative formula that defines a Hamiltonian cycle.
//	 * @return a formula that defines a Hamiltonian cycle
//	 */
//	public Formula cycleDefinition2() {
//		final Formula f0 = cycle.function(vertex, vertex);
//		final Formula f1 = vertex.in(cycle.join(vertex));
//		final Formula f2 = vertex.in(vertex.join(cycle));
//		return f0.and(f1).and(f2);
//	}
	
	private static Integer[] parseInts(Matcher m) {
		if (!m.matches())
			throw new IllegalArgumentException("Badly formatted file.");
		final int groups = m.groupCount();
		final Integer[] matches = new Integer[groups];
		for(int i = 0; i < groups; i++)
			matches[i] = Integer.parseInt(m.group(i+1));
		return matches;
	}
	
	/**
	 * Returns Bounds extracted from the graph 
	 * definition in the given file.
	 * @requires file is formatted according to the format
	 * described at http://asparagus.cs.uni-potsdam.de/?action=instances&id=30
	 * @return Bounds extracted from the graph 
	 * definition in the given file.
	 */
	public Bounds bounds(String file) {
		
		try {
			final BufferedReader in = new BufferedReader(new FileReader(file));
			
			// bound(n) is the first line
			String line = in.readLine();
			Pattern p = Pattern.compile("bound\\((\\d+)\\)\\.");
			
			final int n = parseInts(p.matcher(line))[0];
			final List<Integer> atoms = new ArrayList<Integer>(n);
			
			// vtx(i). for the next n lines
			p = Pattern.compile("vtx\\((\\d+)\\)\\.");
			for(int i = 0; i < n; i++) {
				atoms.add(parseInts(p.matcher(in.readLine()))[0]);
			}
			
			final Universe u = new Universe(atoms);
			final TupleFactory f = u.factory();
			final Bounds b = new Bounds(u);
			
			b.boundExactly(vertex, f.allOf(1));
			b.boundExactly(start, f.setOf(atoms.get(0)));
			final TupleSet edgeBound = f.noneOf(2);
			
			// edge(i,j). for the remaining lines
			p = Pattern.compile("edge\\((\\d+),(\\d+)\\)\\.");
			for(String edge = in.readLine(); edge != null; edge = in.readLine()) {
				edgeBound.add(f.tuple((Object[])parseInts(p.matcher(edge))));
			}
			
			b.boundExactly(edges, edgeBound);
			b.bound(cycle, edgeBound);
			
			return b;
			
		} catch (FileNotFoundException e) {
			System.out.println("File not found: " + file);
			System.exit(1);
		} catch (IOException e) {
			System.out.println("Error reading " + file + ":");
			e.printStackTrace();
			System.exit(1);
		}
		
		throw new AssertionError("dead code.");
	}
	
	private static void usage() {
		System.out.println("Usage: java examples.HamiltonianCycle filename");
		System.exit(1);
	}
	
	private final boolean verify(Instance instance) {
		
		final Evaluator eval = new Evaluator(instance);
		System.out.println(eval.evaluate(cycle));
		return eval.evaluate(cycleDefinition());
	}
	
	/**
	 * Usage: examples.HamiltonianCycle filename
	 */
	public static void main(String[] args) {
		if (args.length!=1)
			usage();
		final HamiltonianCycle model = new HamiltonianCycle();
		final Formula f = model.cycleDefinition();
		final Bounds b = model.bounds(args[0]);
		System.out.println(f);
		System.out.println(b);
		final Solver solver = new Solver();
		solver.options().setSolver(SATFactory.MiniSat);
		solver.options().setReporter(new ConsoleReporter());
//		solver.options().setFlatten(false);
		final Solution s = solver.solve(f,b);
		System.out.println(s);
		if (s.instance()!=null) {
			System.out.print("verifying solution ... ");
			System.out.println(model.verify(s.instance()) ? "correct" : "incorrect!");
		}
	}
	
}
