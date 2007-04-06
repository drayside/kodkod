/**
 * 
 */
package kodkod.engine.config;


import kodkod.ast.Decl;
import kodkod.ast.Formula;
import kodkod.ast.Relation;
import kodkod.engine.bool.BooleanFormula;
import kodkod.instance.Bounds;

/**
 * Enables passing of messages between the kodkod engine
 * and the client about the following stages of the analysis:
 * <ol>
 * <li>bounds optimization (symmetry detection and breaking of predicate symmetries)</li>
 * <li>formula optimization (predicate inlining and skolemization)</li>
 * <li>translation to a boolean circuit</li>
 * <li>symmetry breaking predicate (SBP) generation</li>
 * <li>circuit flattening</li>
 * <li>translation to cnf</li>
 * <li>running a sat solver on the generated cnf</li>
 * </ol>
 * Some of these stages may not be executed, depending on the 
 * {@link Options options} used for analysis.  
 * @author Emina Torlak
 */
public interface Reporter {

	/**
	 * Reports that bounds optimization is in progress (stage 1).
	 */
	public void optimizingBounds();
	
	/**
	 * Reports that formula optimization is in progress (stage 2).
	 */
	public void optimizingFormula();
	
	/**
	 * Reports that the given declaration is being skolemized using the 
	 * given skolem relation.
	 */
	public void skolemizing(Decl decl, Relation skolem);
	
	/**
	 * Reports that the analysis of the given (optimized) formula
	 * and bounds is in stage 3.  The given bounds are not mutated.
	 * @effects bounds' = bounds
	 */
	public void translatingToBoolean(Formula formula, Bounds bounds);
	
	/**
	 * Reports that the analysis is in stage 4.
	 */
	public void generatingSBP();

	/**
	 * Reports that the stage 5 of the analysis is
	 * being performed on the given boolean formula.
	 */
	public void flattening(BooleanFormula circuit);
	
	/**
	 * Reports that the given (optimized)
	 * circuit is being translated to CNF (stage 6 of the analysis).
	 */
	public void translatingToCNF(BooleanFormula circuit);
	
	/**
	 * Reports that the cnf generated in stage 7, consisting of the
	 * given number of variables and clauses, is being analyzed by
	 * a sat solver (stage 8 of the analysis).
	 * @param primaryVars TODO
	 */
	public void solvingCNF(int primaryVars, int vars, int clauses);
}
