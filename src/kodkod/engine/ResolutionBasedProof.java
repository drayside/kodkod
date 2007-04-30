package kodkod.engine;

import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import kodkod.ast.Node;
import kodkod.ast.Variable;
import kodkod.ast.visitor.AbstractVoidVisitor;
import kodkod.engine.fol2sat.RecordFilter;
import kodkod.engine.fol2sat.TranslationLog;
import kodkod.engine.fol2sat.TranslationRecord;
import kodkod.engine.satlab.Clause;
import kodkod.engine.satlab.ReductionStrategy;
import kodkod.engine.satlab.ResolutionTrace;
import kodkod.engine.satlab.SATProver;
import kodkod.instance.TupleSet;
import kodkod.util.collections.IdentityHashSet;
import kodkod.util.ints.IntSet;
import kodkod.util.ints.IntTreeSet;

/**
 * A proof of unsatisfiability based on a {@linkplain ResolutionTrace resolution trace} produced
 * by a {@linkplain SATProver SATProver}.
 * 
 * @author Emina Torlak
 */
final class ResolutionBasedProof extends Proof {
	private SATProver solver;
	private RecordFilter coreFilter;
	/**
	 * Constructs a new ResolutionRefutation that will extract the 
	 * unsatisfiable core for log.formula from the given solver.  
	 * @requires solver.solve() has been called and it returned false.
	 * @requires log.formula is the formula whose translation
	 * resulted in the given SATProver
	 * @effects this.formula' = log.formula
	 */
	ResolutionBasedProof(SATProver solver, TranslationLog log) {
		super(log);
		this.solver = solver;
		this.coreFilter = null;
	}
	
	/**
	 * Returns the magnitude of the literal with the greatest
	 * absolute value in the given clause.  This number uniquely
	 * ties each clause to its corresponding  subformula of this.formula.
	 * @return the magnitude of the literal with the greatest
	 * absolute value in the given clause
	 */
	private static int idLiteral(Clause clause) {
		final IntSet literals = clause.literals();
		return StrictMath.max(StrictMath.abs(literals.min()), StrictMath.abs(literals.max()));
	}
	
	/**
	 * Returns the set of literals that identify the nodes in the unsatisfiable core.
	 * @return set of literals that identify the nodes in the unsatisfiable core
	 * @see kodkod.engine.fol2sat.TranslationRecord#literal()
	 */
	private IntSet coreIdLiterals() {
		final IntSet idLits = new IntTreeSet();
		
		for(Clause clause : solver.proof().core()) {
//				System.out.println(clause);
			idLits.add(idLiteral(clause));
		}
		return idLits;
	}
	
	/**
	 * Returns the connected core based on the given set of 
	 * id literals.  
	 * @requires coreLiterals = coreIdLiterals()
	 * @return let coreNodes = (this.log.records & literal.{i: int | abs(i) in coreLiterals}).node | 
	 *  {n: coreNodes  | some s: set coreNodes | 
	 *   n + this.log.formula in s and (s - this.log.formula).~children in s }
	 */
	private Set<Node> connectedCore(final IntSet coreLiterals) {
		final Set<Node> coreNodes = new IdentityHashSet<Node>();
		final RecordFilter filter = new RecordFilter() {
			public boolean accept(Node node, int literal, Map<Variable,TupleSet> env) {
				return coreLiterals.contains(StrictMath.abs(literal));
			}
		};
		for(Iterator<TranslationRecord> iter = log().replay(filter); iter.hasNext(); ) {
			coreNodes.add(iter.next().node());
		}
		final Set<Node> connected = new IdentityHashSet<Node>();
		final AbstractVoidVisitor traverser = new AbstractVoidVisitor() {
			final Set<Node> visited = new IdentityHashSet<Node>();
			/**
			 * Returns true if the given node has been visited before or if 
			 * it is not contained in this.nodes set.  Otherwise adds 
			 * the node to the connected set and returns false.
			 * @effects this.visited' = this.visited + n
			 * @effects n !in this.visited && n in coreNodes => 
			 *  connected' = connected + n else connected' = connected
			 * @return n in visited || n !in coreNodes
			 */
			protected boolean visited(Node n) {
				if (visited.add(n) && coreNodes.contains(n)) {
					connected.add(n);
					return false;
				}
				return true;
			}
		};
		log().formula().accept(traverser);
		return connected;
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.Proof#core()
	 */
	public final Iterator<TranslationRecord> core() { 
		if (coreFilter == null) {
			coreFilter = new RecordFilter() {
				final IntSet coreLiterals = coreIdLiterals();
				final Set<Node> coreNodes = connectedCore(coreLiterals);
				public boolean accept(Node node, int literal, Map<Variable,TupleSet> env) {
					return coreNodes.contains(node) && coreLiterals.contains(StrictMath.abs(literal));
				}
			};
		}
		return log().replay(coreFilter); 
	}
	
	/**
	 * {@inheritDoc}
	 * @see kodkod.engine.Proof#minimize(kodkod.engine.satlab.ReductionStrategy)
	 */
	public void minimize(ReductionStrategy strategy) {
		solver.proof(strategy);
		coreFilter = null;
	}
}