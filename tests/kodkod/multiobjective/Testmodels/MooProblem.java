package kodkod.multiobjective.Testmodels;

import java.util.TreeSet;

import kodkod.ast.Formula;
import kodkod.instance.Bounds;
import kodkod.multiobjective.MultiObjectiveProblem;
import kodkod.multiobjective.Objective;

public interface MooProblem {
	
	public Bounds getBounds();
	public TreeSet<Objective> getObjectives();
	public Formula getFormula();
	public int getBitWidth();
	public MultiObjectiveProblem getProblem();
}
