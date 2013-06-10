package kodkod.multiobjective.api;

import kodkod.engine.config.Options;

public final class MultiObjectiveOptions implements Cloneable {
	
	final Options kodkodOptions;
	
	private boolean allSolutionsPerPoint = true;
	
	public MultiObjectiveOptions clone() {
		final MultiObjectiveOptions c = new MultiObjectiveOptions(kodkodOptions);
		c.setAllSolutionsPerPoint(allSolutionsPerPoint);
		
		return c;
	}
	
	public String toString() {
		StringBuilder b = new StringBuilder();
		b.append("Kodkod");
		b.append(kodkodOptions.toString());
		b.append("\nMultiObjectiveOptions:");
		b.append("\n allSolutionsPerPoint: ");
		b.append(allSolutionsPerPoint);
		return b.toString();
	}
	
	public MultiObjectiveOptions() {
		kodkodOptions = new Options();
	}
	
	public MultiObjectiveOptions( Options options ) {
		
		if (options == null) {
			throw new NullPointerException();
		}
		
		kodkodOptions = options;
	}

	public Options getKodkodOptions() {
		return kodkodOptions;
	}
	
	// [TeamAmalgam] - Adding for Alloy support
	// We can't get rid of this because it gets called, even though nothing
	// tries to read flatten'. So for our purposes, this will just be stubbed out.
	/**
	 * Sets the flattening option to the given value.
	 * @ensures this.flatten' = flatten
	 * @throws IllegalArgumentException - this.logTranslation>0 && flatten
	 */
	public void setFlatten(boolean flatten) {
		if (kodkodOptions.logTranslation()>0 && flatten)
			throw new IllegalStateException("logTranslation enabled:  flattening must be off.");
	}

	/**
	 * Returns whether all solutions for a given Pareto point should be enumerated,
	 * only meaningful when using Moolloy.
	 * @return this.allSolutionsPerPoint
	 */
	public Boolean allSolutionsPerPoint(){
		return allSolutionsPerPoint;
	}

	/**
	 * Sets whether all solutions for a given Pareto point should be enumerated,
	 * only meaningful when using Moolloy.
	 * @ensures this.allSolutionsPerPoint' = allSolutionsPerPoint
	 */
	public void setAllSolutionsPerPoint(boolean allSolutionsPerPoint){
		this.allSolutionsPerPoint = allSolutionsPerPoint;
	}
}
