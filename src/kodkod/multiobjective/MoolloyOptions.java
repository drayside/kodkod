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
	
	private boolean useSymmetryBreaking = false;
	
	private boolean UseAdaptableMinimumImprovement = true;
	
	public void setAllSolutionsPerPoint(boolean value) {
		allSolutionsPerPoint = value;
	}
	
	public boolean getAllSolutionsPerPoint() {
		return allSolutionsPerPoint;
	}
	
	public void setUseSymmetryBreaking(boolean value) {
		useSymmetryBreaking = value;
	}
	
	public boolean getUseSymmetryBreaking() {
		return useSymmetryBreaking;
	}

	public void setUseAdaptableMinimumImprovement(
			Boolean value) {
		UseAdaptableMinimumImprovement = value;		
	}
	
	public boolean getUseAdaptableMinimumImprovement(){
		return UseAdaptableMinimumImprovement;
	}

}
