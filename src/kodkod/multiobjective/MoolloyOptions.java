/**
 * 
 */
package kodkod.multiobjective;

/**
 * @author hce
 * Stores and provides options which are set by the user
 * (using the interface or similar) about the solving.
 * Examples are: "find all solutions per Pareto point" or
 * "use symmetry breaking"
 */
public class MoolloyOptions {
	
	private boolean allSolutionsPerPoint = false;
	
	public void setAllSolutionsPerPoint(boolean value) {
		allSolutionsPerPoint = value;
	}
	
	public boolean getAllSolutionsPerPoint() {
		return allSolutionsPerPoint;
	}

}
