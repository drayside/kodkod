package kodkod.engine.fol2sat;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;

import kodkod.ast.Formula;
import kodkod.ast.Node;
import kodkod.ast.Variable;
import kodkod.engine.bool.BooleanMatrix;
import kodkod.engine.bool.BooleanValue;
import kodkod.engine.fol2sat.TranslationLog.Record;
import kodkod.instance.Bounds;
import kodkod.instance.TupleFactory;
import kodkod.instance.TupleSet;
import kodkod.util.collections.Containers;
import kodkod.util.collections.FixedMap;
import kodkod.util.ints.IntSet;
import kodkod.util.ints.Ints;

/**
 * A file-based translation logger that logs translation events
 * to a temporary file.
 * @author Emina Torlak
 */
final class FileLogger extends TranslationLogger {
	
	private final FixedMap<Node, Variable[]> logMap;
//	private final Object[] seen;
	private final File file;
	private final int[] tempVals;
	private DataOutputStream out;
	private final TupleFactory factory;
	
	/**
	 * Constructs a new file logger from the given annotated formula.
	 * @effects this.formula' = annotated.source[annotated.node] && this.transforms' = ~(annotated.source) &&
	 * this.bounds' = bounds
	 */
	FileLogger(final AnnotatedNode<Formula> annotated, Bounds bounds) {
		
		this.factory = bounds.universe().factory();
		try {
			this.file = File.createTempFile("kodkod", ".log");
			this.out = new DataOutputStream(new BufferedOutputStream(new FileOutputStream(file)));
		} catch (IOException e1) {
			throw new RuntimeException(e1);
		}
		
		final Map<Node,Set<Variable>> freeVarMap = freeVars(annotated);
		final Variable[] empty = new Variable[0];
	
		this.logMap = new FixedMap<Node, Variable[]>(freeVarMap.keySet());	
//		this.seen = new Object[freeVarMap.keySet().size()];
		
		int index = 0, maxFreeVars = 0; //, usize = bounds.universe().size();
		for(Map.Entry<Node, Variable[]> e : logMap.entrySet()) {
			Set<Variable> vars = freeVarMap.get(e.getKey());
			int size = vars.size();
			if (size==0) {
				e.setValue(empty);
			} else {
				e.setValue(Containers.identitySort(vars.toArray(new Variable[size])));
//				if (size==1) {
//					seen[index] = new IntBitSet(usize);
//				} else {
//					IntSet[] sets = new IntSet[size];
//					for(int i = 0; i < size; i++) {
//						sets[i] = new IntBitSet(usize);
//					}
//					seen[index] = sets;
//				}
				if (size > maxFreeVars)
					maxFreeVars = size;
			}
			index++;
		}
				
		this.tempVals = new int[maxFreeVars];
	}
	
	/**
	 * Returns a map from the sources of all formulas in the given annotated node to their
	 * free variables. 
	 * @return a map from the sources of all formulas in the given annotated node to their
	 * free variables.
	 */
	private static Map<Node,Set<Variable>> freeVars(final AnnotatedNode<Formula> annotated) {
		final Map<Node,Set<Variable>> freeVarMap = new IdentityHashMap<Node,Set<Variable>>();
		annotated.node().accept(new FreeVariableCollector(annotated.sharedNodes()) {
			protected Set<Variable> cache(Node node, Set<Variable> freeVars) {
				if (node instanceof Formula) {
					final Node source = annotated.sourceOf(node);
					Set<Variable> sourceVars = freeVarMap.get(source);
					if (sourceVars==null) {
						sourceVars = newSet();
						sourceVars.addAll(freeVars);
						freeVarMap.put(source, sourceVars);
					} else {
						sourceVars.addAll(freeVars);
					}
				}
				return super.cache(node, freeVars);
			}
		});
		return freeVarMap;
	}
	
	
	
	/**
	 * @see kodkod.engine.fol2sat.TranslationLogger#close()
	 */
	@Override
	void close() {
		try {
			if (out!=null) { out.close();  }
		} catch (IOException e1) { 	
			/* unused */
		} finally { out = null; }
	}
	
	/**
	 * Update the log with the given index, boolean value label, and the
	 * first numFreeVars values in this.tempVals. 
	 * @effects out.writeInt(index) && out.writeInt(label) &&
	 *  all v: [0..numFreeVars) | out.writeInt(this.tempVals[i])
	 */
	private void write(int index, int label, int numFreeVars) {
		try {
			out.writeInt(index);
			out.writeInt(label);
			for(int i = 0 ; i < numFreeVars; i++) {
				out.writeInt(tempVals[i]);
			}
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}
	
	/**
	 * Fills the first logMap.get(index).length entries of the tempVals array with the bindings
	 * for the corresonding variables in the given environment.  
	 * @requires let vars = logMap.get(index).length | 
	 *  all int i: [0..vars.length) | one env.lookup(vars[i]).denseIndices()
	 * @effects let vars = logMap.get(index).length | 
	 *  all int i: [0..vars.length) | this.tempVals[i] = env.lookup(vars[i]).denseIndices().min()
	 * @return logMap.get(index).length
	 */
	private int lookupVars(int index, Environment<BooleanMatrix> env) {
		final Variable[] vars = logMap.get(index);
		for(int i = 0; i < vars.length; i++) {
			tempVals[i] = env.lookup(vars[i]).denseIndices().min();
		}
		return vars.length;
	}
	
	/**
	 * @see kodkod.engine.fol2sat.TranslationLogger#log(kodkod.ast.Formula, kodkod.engine.bool.BooleanValue, kodkod.engine.fol2sat.Environment)
	 */
	@Override
	void log(Node n, BooleanValue v, Environment<BooleanMatrix> env) {
		if (out==null) throw new IllegalStateException();
	
		final int index = logMap.indexOf(n);
		if (index < 0) throw new IllegalArgumentException();
		
		final int numFreeVars = lookupVars(index, env);
		write(index, v.label(), numFreeVars);
//		switch(numFreeVars) {
//		case 0 : // a node with no free variables
//			if (seen[index]==null) { // we haven't seen it before ...
//				seen[index] = Ints.EMPTY_SET;
//				write(index, v.label(), 0);
//			}
//			break;
//		case 1 : // a node with one free variable
//			if (((IntSet)seen[index]).add(tempVals[0])) { // unseen binding
//				write(index, v.label(), 1);
//			} 
//			break;
//		default : // a node with more than one free variable
//			final IntSet[] seenBindings = (IntSet[]) seen[index];
//			boolean unseen = false;
//			for(int i = 0; i < numFreeVars; i++) {
//				if (seenBindings[i].add(tempVals[i])) { // unseen binding
//					unseen = true;
//				}
//			}
//			if (unseen)
//				write(index, v.label(), numFreeVars);
//		}
		
	}

	/**
	 * @see kodkod.engine.fol2sat.TranslationLogger#log()
	 */
	@Override
	TranslationLog log() {
		return new FileLog(file, logMap, factory);
	}
	
	/**
	 * @see java.lang.Object#finalize()
	 */
	protected final void finalize() {
		close();
	}

	/**
	 * A mutable translation record.
	 * @author Emina Torlak
	 */
	private static final class MutableRecord extends Record {
		Node n = null; 
		int literal = 0;
		Map<Variable,TupleSet> env = null;
		
		public Map<Variable, TupleSet> env() { return env;	}
		public int literal() { return literal;	}
		public Node node() { return n; }
		void setAll(Node n, int literal, Map<Variable,TupleSet> env) {
			this.n = n;
			this.literal = literal;
			this.env = env;
		}
		Record setAll(MutableRecord other) {
			setAll(other.n, other.literal, other.env);
			other.setAll(null,0,null);
			return this;
		}
	}
	
	/**
	 * A file-based translation log, written by a FileLogger.
	 * @author Emina Torlak
	 */
	private static final class FileLog extends TranslationLog {
	    private final FixedMap<Node, Variable[]> logMap;
	    private final File file;
	    private final TupleFactory factory;
	    /**
	     * Constructs a new file log using the provided fixed map, file, and tuplefactory.
	     * @requires the file was written by a FileLogger using the given map
	     */
	    FileLog(File file, FixedMap<Node, Variable[]> logMap, TupleFactory factory) {
	    	this.logMap = logMap;
	    	this.file = file;
	    	this.factory = factory;
	    }
	    
	    protected final void finalize() {
	    	file.delete();
	    }
	    
		public Iterator<Record> replay(final IntSet literals) {
			try {	
				return new Iterator<Record>() {
					final DataInputStream in = new DataInputStream(new BufferedInputStream(new FileInputStream(file)));
					final MutableRecord current = new MutableRecord(), next = new MutableRecord();
					long remaining = file.length();
								
					public boolean hasNext() {
						while(remaining > 0 && next.n == null) {
							try {
								final long indexLiteral = in.readLong();
								final int literal = (int) (indexLiteral);
								final int index = (int) (indexLiteral>>>32);
								final Variable[] freeVars = logMap.get(index);
								final int varBytes = freeVars.length << 2;
								if (literals.contains(literal)) {			
									final Map<Variable,TupleSet> env= new FixedMap<Variable,TupleSet>(freeVars);
									for(int i = 0; i < freeVars.length; i++) {
										env.put(freeVars[i], factory.setOf(1,Ints.singleton(in.readInt())));
									}							
									next.setAll(logMap.keyAt(index), literal, env);							
								} else {
									for(int skip = in.skipBytes(varBytes); skip < varBytes; skip++) {
										in.readByte();
									}
								}
								remaining -= (8 + varBytes);
								
							} catch (IOException e) {
								throw new RuntimeException(e);
							}
						}
						return next.n != null;
					}

					public Record next() {
						if (!hasNext()) throw new NoSuchElementException();
						return current.setAll(next);
					}

					public void remove() {	throw new UnsupportedOperationException(); }
					
					protected final void finalize() {
						try { in.close(); } catch (IOException e) { /* unused */ }
					}
				};
			} catch (FileNotFoundException e) {
				throw new RuntimeException(e);
			}	
		}		
	}
}
