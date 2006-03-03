package kodkod.engine.fol2sat;

import java.util.Map;

import kodkod.ast.Node;
import kodkod.ast.Relation;
import kodkod.engine.satlab.SATSolver;
import kodkod.instance.Bounds;
import kodkod.instance.Instance;
import kodkod.instance.TupleFactory;
import kodkod.instance.TupleSet;
import kodkod.util.IntIterator;
import kodkod.util.IntRange;
import kodkod.util.IntSet;
import kodkod.util.Ints;

/**
 * Stores the translation of a {@link kodkod.ast.Formula kodkod formula}
 * to CNF. 
 * 
 * @specfield formula: Formula // the formula that was translated
 * @specfield bounds: Bounds // the bounds used to obtain the CNF from the formula 
 * @specfield solver: SATSolver // a SATSolver containing the CNF representation of the formula
 * @specfield options: Options // the options object used to control translation parameters
 * @author Emina Torlak
 */
public final class Translation {
	private final Bounds bounds;
	private final SATSolver solver;
	/* maps a relation to the intrange whose minimum and maximum values represent
	 * the minimum and maximum literal of the variables allocated to represent
	 * that relation. */
	private final Map<Relation, IntRange> rel2lit; 
	private final int maxLit;
	
	/**
	 * Constructs a new Translation object for the given solver and mapping
	 * from Relations to literals.
	 * @requires maxLit = max(rel2lit[Relation].max)
	 * @effects this.solver' = solver && this.bounds' = bounds
	 */
	Translation(SATSolver solver, Bounds bounds, Map<Relation, IntRange> rel2lit, int maxLit) {	
		this.solver = solver;
		this.bounds = bounds;
		this.rel2lit = rel2lit;
		this.maxLit = maxLit;
	}

	/**
	 * Returns a SATSolver object initialized with the CNF encoding of this.formula 
	 * and the timeout and random seed values specified by this.options.  Satisfiability
	 * of the formula can be checked by calling {@link kodkod.engine.satlab.SATSolver#solve()}.
	 * @return {s: SATSolver | [[s.clauses]] = [[this.formula]] && s.timeout() = this.options.timeout() && 
	 *                         s.seed() = this.options.seed() } 
	 */
	public SATSolver cnf() {
		return solver;
	}
	
	/**
	 * If this.solver.solve() is true, returns 
	 * an interpretation of the cnf solution as a 
	 * mapping from Relations to sets of Tuples.  The Relations
	 * mapped by the returned instance are either leaves
	 * of this.formula with different lower and upper
	 * bounds (i.e. {r: this.formula.*children & Relation | 
	 * this.bounds.upperBound[r] != this.bounds.lowerBound[r]}), 
	 * or skolem constants.
	 * @return an interpretation of the cnf solution as
	 * a mapping from Relations to sets of Tuples.
	 * @throws IllegalStateException - this.solver.solve() has not been called or the 
	 * outcome of the last call was not <code>true</code>.
	 */
	public Instance interpret() {
		final TupleFactory f = bounds.universe().factory();
		final Instance instance = new Instance(bounds.universe());
//		System.out.println(rel2lit);
		final IntSet model = solver.variablesThatAre(true, 1, StrictMath.min(maxLit, solver.numberOfVariables()));
		for(Relation r : bounds) {
			TupleSet lower = bounds.lowerBound(r);
			IntSet indeces = Ints.bestSet(lower.capacity());
			indeces.addAll(lower.indexView());
			IntRange vars = rel2lit.get(r);
			if (vars!=null) {
				int lit = vars.min();
				for(IntIterator iter = bounds.upperBound(r).indexView().iterator(); iter.hasNext();) {
					final int index = iter.nextInt();
					if (!indeces.contains(index) && model.contains(lit++)) 
						indeces.add(index);
				}
			}
			instance.add(r, f.setOf(r.arity(), indeces));
		}
		
//		System.out.println(model);
		return instance;
	}
	
	/**
	 * Returns the number of primary variables allocated 
	 * during translation.  Primary variables represent
	 * the tuples of Relations that are either leaves
	 * of this.formula with different lower and upper
	 * bounds (i.e. {r: this.formula.*children & Relation | 
	 * this.bounds.upperBound[r] != this.bounds.lowerBound[r]}), 
	 * or skolem constants.
	 * @return the number of primary variables allocated
	 * during translation.
	 */
	public int numberOfPrimaryVariables() {
		return maxLit;
	}
	
	/**
	 * Returns the mapping from this.formula's
	 * descendents to the CNF variables that comprise their translations, 
	 * provided that this.options.trackVars is set to true.  Otherwise returns null.
	 * @return a mapping from this.formula.*children to the CNF
	 * variables that comprise their translations.
	 */
	public Map<Node, IntSet> variableUsage() {
		return null;
	}
}
