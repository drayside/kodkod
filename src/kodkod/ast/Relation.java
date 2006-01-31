/*
 * Relation.java
 * Created on May 3, 2005
 */
package kodkod.ast;

import kodkod.ast.visitor.ReturnVisitor;
import kodkod.ast.visitor.VoidVisitor;




/** 
 * Represents a relation.  A relation is a leaf expression.  
 * Two relations are the same if and only if they
 * refer to the same object.  That is, r1.equals(r2) <=> r1 == r2.  Each
 * variable has a name, which is basically a comment for the purpose of 
 * printing, viewing, etc.  The name has no meaning otherwise.
 * 
 * Four methods for creating commonly used predicates over binary relations
 * are provided: {@link #function(Expression, Expression)}, {@link #functional(Expression, Expression)},
 * {@link #acyclic()}, and {@link #totalOrder(Relation, Relation, Relation)}.  Using
 * these methods to generate desired predicates will result in faster constraint solving
 * than creating the same predicates via other API calls. 
 * 
 * @specfield name: String
 * @specfield arity: int
 * @invariant no children
 * @author Emina Torlak 
 */
public class Relation extends LeafExpression {
	/**
	 * Constructs a relation with the specified name and arity.
	 * @effects this.name' = name && this.arity' = arity 
	 * @throws IllegalArgumentException - arity < 1 
	 */
	private Relation(String name, int arity) {
		super(name,arity);
	}
	
	/**
	 * Returns a new relation with the given name and arity.
	 * @return {r: Relation | r.arity = arity && r.name = name }
	 * @throws IllegalArgumentException - arity < 1 
	 */
	public static Relation nary(String name, int arity) {
		return new Relation(name,arity);
	}	

	/**
	 * Returns a new unary relation with the given name.  
	 * The effect of this method is the same as calling Relation.nary(name,1).
	 * @return {r: Relation | r.arity = 1 && r.name = name }
	 */
	public static Relation unary(String name) {
		return new Relation(name,1);
	}
	
	/**
	 * Returns a new binary relation with the given name.
	 * The effect of this method is the same as calling Relation.nary(name,2). 
	 * @return {r: Relation | r.arity = 2 && r.name = name }
	 */
	public static Relation binary(String name) {
		return new Relation(name, 2);
	}
	
	/**
	 * Returns a ternary relation with the specified name.
	 * @return {r: Relation | r.name = name && r.arity = 3}
	 */
	public static Relation ternary(String name) {
		return new Relation(name,3);
	}
	
	
	/**
     * Accepts the given visitor and returns the result.
     * @see kodkod.ast.Node#accept(kodkod.ast.visitor.ReturnVisitor)
     */
	public <E, F, D> E accept(ReturnVisitor<E, F, D> visitor) {
		return visitor.visit(this);
	}
	
	/**
     * Accepts the given visitor.
     * @see kodkod.ast.Node#accept(kodkod.ast.visitor.VoidVisitor)
     */
    public void accept(VoidVisitor visitor) {
        visitor.visit(this);
    }
    
	/**
     * Returns a formula stating that this relation is acyclic.
     * @return {f: Formula | f <=> no ^this & iden}
     * @throws IllegalArgumentException - this.arity != 2
     */
    public Formula acyclic() {
    		return new RelationPredicate.Acyclic(this);
    }
    
    /**
     * Returns a formula stating that this relation is a total function
     * with the specified domain and range.
     * @return {f: Formula | f <=> this in domain->range && all v: domain | one v.this }
     * @throws NullPointerException - domain = null || range = null
     * @throws IllegalArgumentException - domain.arity != 1 || range.arity != 1
     * @throws IllegalArgumentException - this.arity != 2
     */
    public Formula function(Expression domain, Expression range) {
    		return new RelationPredicate.Function(this, domain, Multiplicity.ONE, range);
    }
    
    /**
     * Returns a formula stating that this relation is a partial function
     * with the specified domain and range.
     * @return {f: Formula | f <=> this in domain->range && all v: domain | lone v.this }
     * @throws NullPointerException - domain = null || range = null
     * @throws IllegalArgumentException - domain.arity != 1 || range.arity != 1
     * @throws IllegalArgumentException - this.arity != 2
     */
    public Formula functional(Expression domain, Expression range) {
    		return new RelationPredicate.Function(this, domain, Multiplicity.LONE, range);
    }
    
    /**
     * Returns a formula stating that this relation imposes a total ordering
     * over the atoms in the set <code>ordered</code>, and that thet first and
     * last elements in the ordering are given by the relations <code>first</code>
     * and <code>last</code>.
     * @return {f: Formula | f <=> one first && one last && last in ordered &&
     *                             no this.first && no last.this &&  
     *                             ordered = first.*this && 
     *                             all e: ordered - last | one e.this }
     * @throws NullPointerException - any of the arguments are null
     * @throws IllegalArgumentException - any of the argument relations' arities are greater than one
     * @throws IllegalArgumentException - this.arity != 2
     */
    public Formula totalOrder(Relation ordered, Relation first, Relation last) {
    		return new RelationPredicate.TotalOrdering(this, ordered, first, last);
    }
	
	
}
