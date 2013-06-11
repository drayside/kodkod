package kodkod.multiobjective.Testmodels;

import java.util.TreeSet;

import kodkod.ast.Formula;
import kodkod.instance.Bounds;
import kodkod.multiobjective.api.MultiObjectiveProblem;
import kodkod.multiobjective.api.Objective;

public interface MooProblem {
	
	public Bounds getBounds();
	public TreeSet<Objective> getObjectives();
	public Formula getFormula();
	public int getbidwidth();
	public MultiObjectiveProblem getProblem();
}
