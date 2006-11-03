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
 * Enables the passing of messages between the kodkod engine
 * and the client about the following stages of the analysis:
 * <ol>
 * <li>syntactic analysis of the formula</li>
 * <li>symmetry detection</li>
 * <li>skolemization</li>
 * <li>translation to boolean</li>
 * <li>symmetry breaking</li>
 * <li>circuit optimization</li>
 * <li>translation to cnf</li>
 * <li>running a sat solver on the generated cnf</li>
 * </ol>
 * Some of these stages may not be executed, depending on the 
 * {@link Options options} used for analysis.  
 * @author Emina Torlak
 */
public interface Reporter {

	/**
	 * Reports that the detection of syntactically
	 * shared nodes is in progress (stage 1).
	 */
	public void collectingStructuralInfo();
	
	/**
	 * Reports that the given the analysis is in stage 2.
	 */
	public void detectingSymmetries();
	
	/**
	 * Reports that the given declaration is being skolemized using the 
	 * given skolem relation in stage 3.
	 */
	public void skolemizing(Decl decl, Relation skolem);
	
	/**
	 * Reports that the analysis of the given (optimized) formula
	 * and bounds is in stage 4.  The given bounds are not mutated.
	 * @effects bounds' = bounds
	 */
	public void translatingToBoolean(Formula formula, Bounds bounds);
	
	/**
	 * Reports that the analysis is in stage 5.
	 */
	public void breakingSymmetries();

	/**
	 * Reports that the stage 6 of the analysis is
	 * being performed on the given boolean formula.
	 */
	public void flattening(BooleanFormula circuit);
	
	/**
	 * Reports that the given (optimized)
	 * circuit is being translated to CNF (stage 7 of the analysis).
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
