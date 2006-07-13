package examples;

import kodkod.ast.Formula;
import kodkod.ast.Relation;

/**
 * A toy implementation of the Kemeny rank aggregation function.
 * 
 * @author Emina Torlak
 */
public final class RankAggregation {
	private final Relation[] rankings;
	private final Relation kemeny, kfirst, klast, elts;
	
	/**
	 * Constructs a RankAggregation model for finding the Kemeny rank
	 * for the given number of rankings.
	 */
	public RankAggregation(int numRankings) {
		assert numRankings > 0;
		this.rankings = new Relation[numRankings];
		for(int i = 0; i < numRankings; i++)
			rankings[i] = Relation.binary("r"+i);
		this.kemeny = Relation.binary("kemeny");
		this.kfirst = Relation.unary("kfirst");
		this.klast = Relation.unary("klast");
		this.elts = Relation.unary("elts");
	}

	/**
	 * Returns a formula stating that the kemeny relation imposes a total
	 * on the universe.
	 * @return a formula stating that the kemeny relation imposes a total
	 * on the universe.
	 */
	public Formula declarations() {
		return kemeny.totalOrder(elts, kfirst, klast);
	}
	
	/**
	 * Returns a formula stating that the kemeny relation is locally minimal.
	 * @return a formula stating that the kemeny relation is locally minimal.
	 */
	public Formula locallyMinimal() {
		return null;
	}
	
}	
