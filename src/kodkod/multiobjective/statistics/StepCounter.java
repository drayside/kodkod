package kodkod.multiobjective.statistics;

import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.util.Iterator;
import java.util.Vector;

public class StepCounter {

	Vector<Integer> steps;
	int index;

	public StepCounter() {
		//set the Vector size to 1000
		steps = new Vector<Integer>(1000);
		index = 0;
		steps.add(index, 0);
	}

	/**
	 * Starts the counting at a the next index.
	 * Use this method before you start discovering a new base point.
	 */
	public void nextIndex() {
		index++;
		steps.add(index, 0);
	}

	/**
	 * Counts a current step.
	 * Use this method while working your way up to the Pareto front
	 */
	public void countStep() {
		//current value
		int v  = steps.get(index);
		//increment it
		v++;
		steps.set(index, v);
	}

	public String toString() {
		StringBuilder result = new StringBuilder("");

		for(Iterator<Integer> i = steps.iterator(); i.hasNext();){
			result.append(i.next());
			if (i.hasNext()) {
				result.append(",");
			}
		}
		return result.toString();
	}

	public void writeDataFile(String filePath) throws IOException {
		Writer fw = null;
		int point = 1;
		try {
			fw = new FileWriter(filePath);
			fw.append("# Step counting file\n");
			for(Iterator<Integer> i = steps.iterator(); i.hasNext();){
				fw.append(point + " " + i.next() + "\n");
				point++;
			}
		}
		finally {
			if(fw != null)
				fw.close();
		}
	}

}
