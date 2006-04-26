package examples;

import kodkod.ast.Formula;
import kodkod.ast.Relation;
import kodkod.ast.Variable;

/**
 * KodKod encoding of shakehands.als.
 * 
 * @author Emina Torlak
 */
 final class Handshake {
	private final Relation Person, /*Hilary, Jocelyn,*/ shaken, spouse;
	
	Handshake() {
		Person = Relation.unary("Person");
//		Hilary = Relation.unary("Hilary");
//		Jocelyn = Relation.unary("Jocelyn");
		shaken = Relation.binary("shaken");
		spouse = Relation.binary("spouse");
	}
	
	/**
	 * Returns the declarations
	 * @return
	 * <pre>
	 * sig Person {spouse: Person, shaken: set Person}
	 * one sig Jocelyn, Hilary extends Person {}
	 * </pre>
	 */
	Formula declarations() {
		return spouse.function(Person, Person);
	}
	
	/**
	 * @return
	 * <pre>
	 * fact ShakingProtocol {
	 *  // nobody shakes own or spouse's hand
	 *  all p: Person | no (p + p.spouse) & p.shaken
	 *  // if p shakes q, q shakes p
	 *  all p, q: Person | p in q.shaken => q in p.shaken
	 * }
	 * </pre>
	 */
	Formula shakingProtocol() {
		final Variable p = Variable.unary("p");
		final Variable q = Variable.unary("q");
		final Formula f1 = p.union(p.join(spouse)).intersection(p.join(shaken)).no().forAll(p.oneOf(Person));
		final Formula f2 = p.in(q.join(shaken)).implies(q.in(p.join(shaken))).forAll(p.oneOf(Person).and(q.oneOf(Person)));
		return f1.and(f2);
	}
	
	/**
	 * @return 
	 * <pre>
	 * fact Spouses {
	 *  all disj p, q: Person {
	 *   // if q is p's spouse, p is q's spouse
	 *   p.spouse = q => q.spouse = p
	 *   // no spouse sharing
	 *	 p.spouse != q.spouse
	 *  }
	 *  all p: Person {
	 * 	 // a person is his or her spouse's spouse
	 *	 p.spouse.spouse = p
	 *	 // nobody is his or her own spouse
	 *	 p != p.spouse
	 *	}
	 * }
	 * </pre>
	 */
	Formula spouses() {
		final Variable p = Variable.unary("p");
		final Variable q = Variable.unary("q");
		final Formula f1 = p.join(spouse).eq(q).implies(q.join(spouse).eq(p));
		final Formula f2 = p.join(spouse).eq(q.join(spouse)).not();
		final Formula f3 = p.intersection(q).no().implies(f1.and(f2)).forAll(p.oneOf(Person).and(q.oneOf(Person)));
		final Formula f4 = p.join(spouse).join(spouse).eq(p).and(p.eq(p.join(spouse)).not()).forAll(p.oneOf(Person));
		return f3.and(f4);
	}
	
	/**
	 * @return 
	 * pred Puzzle() {
	 *  // everyone but Jocelyn has shaken a different number of hands
	 *  all disj p,q: Person - Jocelyn | #p.shaken != #q.shaken
	 *  // there are 10 persons
	 *  //#Person = 10
	 *  // Hilary's spouse is Jocelyn
	 *  Hilary.spouse = Jocelyn
	 * }
	 */
//	Formula puzzle() {
//		final Variable p = Variable.unary("p");
//		final Variable q = Variable.unary("q");
//		
//	}
	
}
