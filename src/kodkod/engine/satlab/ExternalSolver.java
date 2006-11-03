/**
 * 
 */
package kodkod.engine.satlab;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.RandomAccessFile;
import java.util.BitSet;

/**
 * An implementation of a wrapper for an external SAT solver, 
 * executed in a separate process.
 * @author Emina Torlak
 */
final class ExternalSolver implements SATSolver {
	private final StringBuilder buffer;
	private final int capacity = 8192;
	private final String executable, options, temp;
	private final RandomAccessFile cnf;
	private final BitSet solution;
	private volatile Boolean sat;
	private volatile int vars, clauses;
	
	/**
	 * Constructs an ExternalSolver that will execute the specified binary
	 * with the given options on a tempFile with the specified name.  The tempFile
	 * will be initialized to contain all clauses added to this solver via the 
	 * {@link #addClause(int[])} method.
	 */
	ExternalSolver(String executable, String options, String tempFile) {
		try {
			this.cnf = new RandomAccessFile(tempFile,"rw");
			this.cnf.setLength(0);
			// get enough space into the buffer for the cnf header, which will be written last
			this.buffer = new StringBuilder();
			for(int i = headerLength(); i > 0; i--) {
				buffer.append(" ");
			}
			buffer.append("\n");
			this.sat = null;
			this.solution = new BitSet();
			this.vars = 0;
			this.clauses = 0;
			this.executable = executable;
			this.options = options;
			this.temp = tempFile;
		} catch (FileNotFoundException e) {
			throw new SATAbortedException(e);
		} catch (IOException e) {
			throw new SATAbortedException(e);
		} 
		
	}
	
	/**
	 * Returns the length, in characters, of the longest possible header
	 * for a cnf file: p cnf Integer.MAX_VALUE Integer.MAX_VALUE
	 * @return the length, in characters, of the longest possible header
	 * for a cnf file: p cnf Integer.MAX_VALUE Integer.MAX_VALUE
	 */
	private static final int headerLength() {
		return String.valueOf(Integer.MAX_VALUE).length()*2 + 8;
	}
	
	/**
	 * Flushes the contents of the string buffer to the cnf file.
	 */
	private final void flush(){ 
		try {
			cnf.writeBytes(buffer.toString());
		} catch (IOException e) {
			throw new SATAbortedException(e);
		}
		buffer.setLength(0);
	}
	
	/**
	 * @see kodkod.engine.satlab.SATSolver#addClause(int[])
	 */
	public void addClause(int[] lits) {
		if (lits.length>0) {
			clauses++;
			if (buffer.length()>capacity) flush();
			for(int lit: lits) {
				buffer.append(lit);
				buffer.append(" ");
			}
			buffer.append("0\n");
		}
	}

	/**
	 * @see kodkod.engine.satlab.SATSolver#addVariables(int)
	 */
	public void addVariables(int numVars) {
		if (numVars < 0)
			throw new IllegalArgumentException("vars < 0: " + numVars);
		vars += numVars;
	}

	/**
	 * @see kodkod.engine.satlab.SATSolver#free()
	 */
	public synchronized void free() {
		try {
			cnf.close();
		} catch (IOException e) {
			// do nothing
		}
	}

	
	/**
	 * Releases the resources used by this external solver.
	 */
	protected final void finalize() throws Throwable {
		super.finalize();
		free();
	}
	
	/**
	 * @see kodkod.engine.satlab.SATSolver#numberOfClauses()
	 */
	public int numberOfClauses() {
		return clauses;
	}

	/**
	 * @see kodkod.engine.satlab.SATSolver#numberOfVariables()
	 */
	public int numberOfVariables() {
		return vars;
	}
	
	/**
	 * @effects |lit| <= this.vars && lit != 0 => this.solution'.set(|lit|, lit>0)
	 * @throws RuntimeException - lit=0 || |lit|>this.vars
	 */
	private final void updateSolution(int lit) {
		int abs = StrictMath.abs(lit);
		if (abs<=vars && abs>0)
			solution.set(abs-1, lit>0);
		else
			throw new RuntimeException("invalid variable value: |" + lit + "| !in [1.."+vars+"]");
	}
	
	/**
	 * @see kodkod.engine.satlab.SATSolver#solve()
	 */
	public boolean solve() throws SATAbortedException {
		if (sat==null) {
			flush();
			Process p = null;
			try {
				cnf.seek(0);
				cnf.writeBytes("p cnf " + vars + " " + clauses);
				cnf.close();
				
				final String[] command;
				if (options.length()==0)
					command = new String[]{executable,  temp};
				else 
					command = new String[]{executable, options, temp};
				p = Runtime.getRuntime().exec(command);
				p.waitFor();
				final BufferedReader out = new BufferedReader(new InputStreamReader(p.getInputStream()));
				String line = null;
				while((line = out.readLine()) != null) {
					String[] tokens = line.split("\\s");
					int tlength = tokens.length;
					if (tlength>0) {
						if (tokens[0].compareToIgnoreCase("s")==0) {
							if (tlength==2) {
								if (tokens[1].compareToIgnoreCase("SATISFIABLE")==0) {
									sat = Boolean.TRUE;
									continue;
								} else if (tokens[1].compareToIgnoreCase("UNSATISFIABLE")==0) {
									sat = Boolean.FALSE;
									continue;
								} 
							}
							throw new RuntimeException("invalid " + executable + " output line: " + line);
						} else if (tokens[0].compareToIgnoreCase("v")==0) {
							int last = tlength-1;
							for(int i = 1; i < last; i++) {
								updateSolution(Integer.parseInt(tokens[i]));
							}
							int lit = Integer.parseInt(tokens[last]);
							if (lit!=0) updateSolution(lit);
							else break; // last variable line
						} // not a solution line or a variable line, so ignore it.
					}
				}
				try { out.close(); } catch (IOException e) { } // do nothing, we are done 
				if (sat==null || Boolean.TRUE.equals(sat) && solution.isEmpty()) {
					throw new RuntimeException("inconsistent " + executable + " output");
				}
			} catch (IOException e) {
				throw new SATAbortedException(e);
			} catch (InterruptedException e) {
				p.destroy(); // p cannot be null if this exception was thrown
				throw new SATAbortedException(e);
			} catch (NumberFormatException e) {
				throw new RuntimeException("invalid "+ executable +" output", e);
			}
		}
		return sat;
	}

	/**
	 * @see kodkod.engine.satlab.SATSolver#valueOf(int)
	 */
	public boolean valueOf(int variable) {
		if (!Boolean.TRUE.equals(sat))
			throw new IllegalStateException();
		if (variable < 1 || variable > vars)
			throw new IllegalArgumentException(variable + " !in [1.." + vars+"]");
		return solution.get(variable-1);
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return executable + " " + options;
	}
}
