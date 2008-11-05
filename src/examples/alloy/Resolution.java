/**
 * 
 */
package examples.alloy;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.Relation;
import kodkod.ast.Variable;
import kodkod.engine.Solution;
import kodkod.engine.Solver;
import kodkod.engine.fol2sat.HigherOrderDeclException;
import kodkod.engine.fol2sat.UnboundLeafException;
import kodkod.engine.satlab.SATFactory;
import kodkod.engine.ucore.RCEStrategy;
import kodkod.instance.Bounds;
import kodkod.instance.TupleFactory;
import kodkod.instance.TupleSet;
import kodkod.instance.Universe;
import kodkod.util.nodes.PrettyPrinter;

/**
 * A model describing resolution refutation.
 * @author Emina Torlak
 *
 */
public class Resolution {
	private final Relation Literal, Clause, Conflict, Refutation;
	private final Relation sources, resolvents, edges, neg, lits;
	/**
	 * Constructs a new instance of the model.
	 */
	public Resolution() { 
		Literal = Relation.unary("Literal");
		Clause = Relation.unary("Clause");
		Conflict = Relation.unary("Conflict");
		Refutation = Relation.unary("Refutation");
		sources = Relation.binary("sources");
		resolvents = Relation.binary("resolvents");
		edges = Relation.ternary("edges");
		neg = Relation.binary("neg");
		lits = Relation.binary("lits");
	}
	Expression nodes(Expression r) { 
		return sources(r).union(resolvents(r));
	}
	Expression sources(Expression r) { 
		return r.join(sources);
	}
	Expression edges(Expression r) { 
		return r.join(edges);
	}
	Expression resolvents(Expression r) { 
		return r.join(resolvents);
	}
	Expression lits(Expression c) { 
		return c.join(lits);
	}
	Expression neg(Expression lit) { 
		return lit.join(neg);
	}
	
	/**
	 * Returns  declarations of Literal.
	 * @return Literal declarations
	 */
	public Formula literals() { 
		final List<Formula> decls = new ArrayList<Formula>();
		decls.add(neg.function(Literal, Literal));
		decls.add(neg.eq(neg.transpose()));
		decls.add(Expression.IDEN.intersection(neg).no());
		return Formula.and(decls);
	}
	
	/**
	 * Returns  declarations of Clause and Conflict.
	 * @return Clause declarations
	 */
	public Formula clauses() {
		final List<Formula> decls = new ArrayList<Formula>();
		decls.add(lits.in(Clause.product(Literal)));
		decls.add(Conflict.in(Clause));
		decls.add(Conflict.one());
		decls.add(lits(Conflict).no());
		
		final Variable c = Variable.unary("c");
		decls.add(lits(c).some().forAll(c.oneOf(Clause.difference(Conflict))));
		decls.add(lits(c).intersection(neg(lits(c))).no().forAll(c.oneOf(Clause)));
		
		
		return Formula.and(decls);
	}
	
	/** Returns the resolve predicate
	 * @return resolve predicate **/
	public Formula resolve(Expression s1, Expression s2, Expression r) { 
//		pred resolve(s,s': Clause, r: Clause) {
//				let lit = s.lits & s'.lits.neg | 
//					one lit and r.lits = s.lits + s'.lits - (lit + lit.neg)
//			}
		final Expression s1lits = lits(s1), s2lits = lits(s2), rlits = lits(r);
		final Expression lit = s1lits.intersection(neg(s2lits));
		final Formula f0 = lit.one();
		final Formula f1 = rlits.eq(s1lits.union(s2lits).difference(lit.union(neg(lit))));
		return f0.and(f1);
	}
	
	/** 
	 * Returns the declarations of Refutation
	 * @return Refutation constraints
	 */
	public Formula refutations() { 
		final List<Formula> decls = new ArrayList<Formula>();
	
		decls.add(sources.in(Refutation.product(Clause.difference(Conflict))));
		decls.add(resolvents.in(Refutation.product(Clause)));
		decls.add(edges.in(Refutation.product(Clause).product(Clause)));
		
		final Variable ref = Variable.unary("ref");
		
		decls.add(sources(ref).some().forAll(ref.oneOf(Refutation)));
		decls.add(edges(ref).in(nodes(ref).product(resolvents(ref))).forAll(ref.oneOf(Refutation)));
		
		final Variable r = Variable.unary("r");
		decls.add(edges(ref).join(r).some().forAll(ref.oneOf(Refutation).and(r.oneOf(resolvents(ref)))));
		
//		final Variable s1 = Variable.unary("s"), s2 = Variable.unary("s'");
//		final Formula isEdge = s1.union(s2).product(r).in(edges(ref));
//		decls.add(isEdge.iff(resolve(s1,s2,r)).
//				forAll(ref.oneOf(Refutation).and(r.oneOf(resolvents(ref))).
//						and(s1.oneOf(nodes(ref))).and(s2.oneOf(nodes(ref)))));
		
		
		final Variable s = Variable.unary("s");
		final Formula cond = edges(ref).join(r).difference(s).one().
							 and(resolve(s,edges(ref).join(r).difference(s),r));
		final Expression comp = cond.comprehension(s.oneOf(nodes(ref)).and(r.oneOf(resolvents(ref))));
		decls.add(edges(ref).eq(comp).forAll(ref.oneOf(Refutation)));
		
		decls.add(edges(ref).intersection(Expression.IDEN).no().forAll(ref.oneOf(Refutation)));
		decls.add(Conflict.in(resolvents(ref)).forAll(ref.oneOf(Refutation)));
		
		return Formula.and(decls);	
	}
	
	/** Returns all declarations.
	 * 	@return all decls **/
	public Formula decls() { 
		return literals().and(clauses()).and(refutations());
	}
	
	/** Returns the conjunction of decls and "some Refutation" 
	 * @return decls() and some Refutation */
	public Formula show() { 
		return decls().and(edges.some());
	}
	
	/**
	 * Returns the checkSources assertion.
	 * @return checkSources assertion
	 */
	public Formula checkSources() {
		//all r: Refutation | no r.edges.(r.sources)
		final Variable r = Variable.unary("ref");
		final Formula check = edges(r).join(sources(r)).no().forAll(r.oneOf(Refutation));
		return decls().and(check.not());
	}

	/** 
	 * Returns bounds with the given positive number of literals, clauses, and refutations 
	 * @return bounds 
	 */
	public Bounds bounds(int l, int c, int r) { 
		final List<String> atoms = new ArrayList<String>(l+c+r);
		for(int i = 0; i < l; i++) { 
			atoms.add("Lit"+i);
		}
		atoms.add("Conflict");
		for(int i = 1; i < c; i++) { 
			atoms.add("Clause"+i);
		}
		for(int i = 0; i < r; i++) { 
			atoms.add("Refutation"+i);
		}
		final Universe u = new Universe(atoms);
		final TupleFactory f = u.factory();
		final Bounds b = new Bounds(u);
	
		final TupleSet cbound = f.range(f.tuple("Conflict"), f.tuple("Clause"+(c-1)));
		final TupleSet lbound = f.range(f.tuple("Lit0"), f.tuple("Lit"+(l-1)));
		final TupleSet rbound = f.range(f.tuple("Refutation0"), f.tuple("Refutation"+(r-1)));
		b.bound(Clause, cbound);
		b.boundExactly(Conflict, f.setOf("Conflict"));
		b.bound(Literal, lbound);
		b.bound(Refutation, rbound);
		b.bound(neg, lbound.product(lbound));
		b.bound(lits, cbound.product(lbound));
		b.bound(sources, rbound.product(cbound));
		b.bound(resolvents, rbound.product(cbound));
		b.bound(edges, rbound.product(cbound.product(cbound)));
	
//		b.boundExactly(Clause, cbound);
//		b.boundExactly(Conflict, f.setOf("Conflict"));
//		b.boundExactly(Literal, lbound);
//		b.boundExactly(Refutation, rbound);
//		b.boundExactly(neg, f.setOf(f.tuple("Lit0","Lit1"),f.tuple("Lit1","Lit0")));
//		b.boundExactly(lits, f.setOf(f.tuple("Clause1","Lit0"),f.tuple("Clause2","Lit1")));
//		b.boundExactly(sources, f.setOf(f.tuple("Refutation0","Clause1"), f.tuple("Refutation0","Clause2")));
//		b.boundExactly(resolvents, f.setOf(f.tuple("Refutation0","Conflict")));
//		b.boundExactly(edges, f.setOf(f.tuple("Refutation0","Clause1","Conflict"),
//									  f.tuple("Refutation0","Clause2","Conflict")));
		
		return b;
	}
	
	private static void usage() {
		System.out.println("java examples.Resolution <literals> <clauses> <refutations>");
		System.exit(1);
	}
	
	/**
	 * Checks that the given proof of unsatisfiablity for the given problem is miminal.
	 * This method assumes that the given proof is correct.
	 */
	private static void checkMinimal(Solver solver, Bounds bounds, Set<Formula> core) {
		System.out.print("checking minimality ... ");
		final long start = System.currentTimeMillis();
		final Set<Formula> minCore = new LinkedHashSet<Formula>(core);
		for(Iterator<Formula> itr = minCore.iterator(); itr.hasNext();) {
			Formula f = itr.next();
			Formula noF = Formula.TRUE;
			for( Formula f1 : minCore ) {
				if (f!=f1)
					noF = noF.and(f1);
			}
			if (solver.solve(noF, bounds).instance()==null) {
				itr.remove();
			}			
		}
		final long end = System.currentTimeMillis();
		if (minCore.size()==core.size()) {
			System.out.println("minimal (" + (end-start) + " ms).");
		} else {
			System.out.println("not minimal (" + (end-start) + " ms). The minimal core has these " + minCore.size() + " formulas:");
			for(Formula f : minCore) {
				System.out.println(" " + f);
			}
		}
	}
	
	/**
	 * Usage: java examples.Resolution <literals> <clauses> <refutations>
	 */
	public  static void main(String[] args) {
		if (args.length < 3)
			usage();
		try {
			final int l = Integer.parseInt(args[0]);
			final int c = Integer.parseInt(args[1]);
			final int r = Integer.parseInt(args[2]);
			
			final Resolution model = new Resolution();
			
			final Bounds b = model.bounds(l,c,r);
			final Solver solver = new Solver();
			
			final Formula f = model.show();
						
			solver.options().setSolver(SATFactory.MiniSatProver);
			solver.options().setLogTranslation(1);
			
			
//			System.out.println("running show:");
//			System.out.println(PrettyPrinter.print(f,1));
			
			Solution s = solver.solve(f, b);
			if (s.outcome()==Solution.Outcome.UNSATISFIABLE) {
				System.out.println("constraints: " + s.proof().log().roots().size());
//				for(Iterator<TranslationRecord> itr = s.proof().log().replay(); itr.hasNext(); ) { 
//					TranslationRecord rec = itr.next();
//					if (s.proof().log().roots().contains(rec.node()))
//						System.out.println(rec);
//				}
				s.proof().minimize(new RCEStrategy(s.proof().log()));
				System.out.println("min core (" + s.proof().highLevelCore().size() + "):");
				for (Formula node : s.proof().highLevelCore()) {
					System.out.println(PrettyPrinter.print(node, 1));
				}
				checkMinimal(solver, b, s.proof().highLevelCore());
			} else if (s.outcome()==Solution.Outcome.TRIVIALLY_UNSATISFIABLE){
				System.out.println("core is:");
				for (Formula node : s.proof().highLevelCore()) {
					System.out.println(PrettyPrinter.print(node, 1));
				}
//				for(Iterator<TranslationRecord> itr = s.proof().core(); itr.hasNext();) {
//					TranslationRecord rec = itr.next();
//					System.out.println(rec);
//				}
			} else {
				System.out.println(s);
			}
	
		} catch (NumberFormatException nfe) {
			usage();
		} catch (HigherOrderDeclException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (UnboundLeafException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} 
	}
}
