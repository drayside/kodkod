package examples;

import java.util.ArrayList;
import java.util.List;

import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.Relation;
import kodkod.ast.Variable;
import kodkod.engine.Solution;
import kodkod.engine.Solver;
import kodkod.engine.TimeoutException;
import kodkod.engine.satlab.SATFactory;
import kodkod.instance.Bounds;
import kodkod.instance.TupleFactory;
import kodkod.instance.Universe;

/**
 * KK encoding of mondex/a.als.
 * 
 * @author Emina Torlak
 */
final class AbstractWorldDefinitions {
	// sigs
	private final Relation Coin, AbPurse, NAME, TransferDetails, AbWorld0,
			AbWorld, AIN, aNullIn, transfer, AOUT, aNullOut;

	// binary relations
	private final Relation balance, lost, from, to, value, details;

	// ternary relations
	private final Relation abAuthPurse;

	/**
	 * Constructs an instance of the AbstractWorldDefinitions module.
	 */
	public AbstractWorldDefinitions() {
		Coin = Relation.unary("Coin");
		AbPurse = Relation.unary("AbPurse");
		NAME = Relation.unary("NAME");
		TransferDetails = Relation.unary("TransferDetails");
		AbWorld0 = Relation.unary("AbWorld0");
		AbWorld = Relation.unary("AbWorld");
		AIN = Relation.unary("AIN");
		aNullIn = Relation.unary("aNullIn");
		AOUT = Relation.unary("AOUT");
		aNullOut = Relation.unary("aNullOut");
		transfer = Relation.unary("transfer");
		balance = Relation.binary("balance");
		lost = Relation.binary("lost");
		from = Relation.binary("from");
		to = Relation.binary("to");
		value = Relation.binary("value");
		details = Relation.binary("details");
		abAuthPurse = Relation.ternary("abAuthPurse");
	}

	/**
	 * Returns the declarations of the sigs Coin, AbPurse, NAME, TransferDetails
	 * and their fields.
	 * 
	 * @return
	 * 
	 * <pre>
	 *      sig Coin {}
	 *      sig AbPurse { balance, lost : set Coin }
	 *      fact {
	 *     	no disj p1, p2 : AbPurse {
	 *     	 p1.balance = p2.balance
	 *     	 p1.lost = p2.lost
	 *     	 }
	 *     	}
	 *     sig NAME {}
	 *     sig TransferDetails { from, to : NAME, value : set Coin}
	 * </pre>
	 */
	public Formula decls0() {
		final Formula f0 = balance.in(AbPurse.product(Coin));
		final Formula f1 = lost.in(AbPurse.product(Coin));
		final Variable p1 = Variable.unary("p1"), p2 = Variable.unary("p2");
		final Formula f2 = p1.join(balance).eq(p2.join(balance)).and(
				p1.join(lost).eq(p2.join(lost)));
		final Formula f3 = f2.implies(p1.eq(p2)).forAll(
				p1.oneOf(AbPurse).and(p2.oneOf(AbPurse)));
		final Formula f4 = from.function(TransferDetails, NAME);
		final Formula f5 = to.function(TransferDetails, NAME);
		final Formula f6 = value.in(TransferDetails.product(Coin));
		return f0.and(f1).and(f3).and(f4).and(f5).and(f6);
	}

	/**
	 * Computes the Abstract predicate.
	 * 
	 * @return
	 * 
	 * <pre>
	 *      pred Abstract (abAuthPurse : NAME -&gt; AbPurse) {
	 *     	all n : NAME | lone n.abAuthPurse
	 *     	no disj n1, n2 : NAME | some n1.abAuthPurse.(balance + lost) &amp; n2.abAuthPurse.(balance + lost)
	 *     	no p : AbPurse {
	 *     	 p in NAME.abAuthPurse
	 *     	 some p.balance &amp; p.lost
	 *     	 }
	 *      }
	 * </pre>
	 */
	public Formula abstractWorld(Expression/* NAME -> AbPurse */abAuthPurse) {
		final Variable n = Variable.unary("n"), n1 = Variable.unary("n1"), n2 = Variable
				.unary("n2");
		final Variable p = Variable.unary("p");
		final Formula f0 = n.join(abAuthPurse).lone().forAll(n.oneOf(NAME));
		final Expression money = balance.union(lost);
		final Formula f1 = n1.join(abAuthPurse).join(money).intersection(
				n2.join(abAuthPurse).join(money)).some();
		final Formula f2 = f1.implies(n1.eq(n2)).forAll(
				n1.oneOf(NAME).and(n2.oneOf(NAME)));
		final Formula f3 = p.in(NAME.join(abAuthPurse));
		final Formula f4 = p.join(balance).intersection(p.join(lost)).some();
		final Formula f5 = (f3.and(f4)).not().forAll(p.oneOf(AbPurse));
		return f0.and(f2).and(f5);
	}

	/**
	 * Returns the declarations of AbWorld0, AbWorld, and their fields.
	 * 
	 * @return
	 * 
	 * <pre>
	 *      sig AbWorld0 {
	 *     	 abAuthPurse : NAME -&gt; AbPurse
	 *      }
	 *      fact {
	 *     	 no disj a1, a2 : AbWorld0 | a1.abAuthPurse = a2.abAuthPurse
	 *     	}
	 *     	 sig AbWorld extends AbWorld0 {} {
	 *     	  Abstract (abAuthPurse)
	 *     	 }
	 * </pre>
	 */
	public Formula decls1() {
		final Formula f0 = abAuthPurse.in(AbWorld0.product(NAME).product(
				AbPurse));
		final Variable a1 = Variable.unary("a1"), a2 = Variable.unary("a2");
		final Formula f1 = a1.join(abAuthPurse).eq(a2.join(abAuthPurse))
				.implies(a1.eq(a2));
		final Formula f2 = f1
				.forAll(a1.oneOf(AbWorld0).and(a2.oneOf(AbWorld0)));
		final Formula f3 = AbWorld.in(AbWorld0);
		final Variable abwthis = Variable.unary("this");
		final Formula f4 = abstractWorld(abwthis.join(abAuthPurse)).forAll(
				abwthis.oneOf(AbWorld));
		return f0.and(f2).and(f3).and(f4);
	}

	/**
	 * Computes the purses function.
	 * 
	 * @return
	 * 
	 * <pre>
	 *      fun purses (w : AbWorld) : set AbPurse {
	 *     	 NAME.(w.abAuthPurse)
	 *     	}
	 * </pre>
	 */
	public Expression purses(Expression/* AbWorld */w) {
		return NAME.join(w.join(abAuthPurse));
	}

	/**
	 * Computes the names function.
	 * 
	 * @return
	 * 
	 * <pre>
	 *      fun names (w : AbWorld) : set NAME {
	 *     	 w.abAuthPurse.AbPurse
	 *     	 }
	 * </pre>
	 */
	public Expression names(Expression/* AbWorld */w) {
		return w.join(abAuthPurse).join(AbPurse);
	}

	/**
	 * Computes the totalBalance function.
	 * 
	 * @return
	 * 
	 * <pre>
	 *      fun totalBalance (w : AbWorld) : set Coin {
	 *     	 purses (w) . balance
	 *     	 }
	 * </pre>
	 */
	public Expression totalBalance(Expression/* AbWorld */w) {
		return purses(w).join(balance);
	}

	/**
	 * Computes the totalLost function.
	 * 
	 * @return
	 * 
	 * <pre>
	 *      fun totalLost (w : AbWorld) : set Coin {
	 *     	 purses (w) . lost
	 *     	 }
	 * </pre>
	 */
	public Expression totalLost(Expression/* AbWorld */w) {
		return purses(w).join(lost);
	}

	/**
	 * Computes the NoValueCreation predicate.
	 * 
	 * @return
	 * 
	 * <pre>
	 *      pred NoValueCreation (w, w' : AbWorld) {
	 *     	 totalBalance (w') in totalBalance (w)
	 *     	 }
	 * </pre>
	 */
	public Formula noValueCreation(Expression/* AbWorld */w,
			Expression/* AbWorld */wprime) {
		return totalBalance(wprime).in(totalBalance(w));
	}

	/**
	 * Computes the allValueAccounted predicate.
	 * 
	 * @return
	 * 
	 * <pre>
	 *     	 pred AllValueAccounted (w, w' : AbWorld) {
	 *     	 totalBalance (w') + totalLost (w') = totalBalance (w) + totalLost (w)
	 *     	 }
	 * </pre>
	 */
	public Formula allValueAccounted(Expression/* AbWorld */w,
			Expression/* AbWorld */wprime) {
		return totalBalance(wprime).union(totalLost(wprime)).eq(
				totalBalance(w).union(totalLost(w)));
	}

	/**
	 * Returns the declarations of AIN, AOUT, transfer, etc.
	 * 
	 * @return
	 * 
	 * <pre>
	 *      abstract sig AIN {}
	 *      one sig aNullIn extends AIN {}
	 *     	sig transfer extends AIN {details : TransferDetails}
	 *     
	 *     	fact {
	 *     	 no disj t1, t2 : transfer {
	 *     	  t1.details.from = t2.details.from
	 *     	  t1.details.to = t2.details.to
	 *     	  t1.details.value = t2.details.value
	 *     	 }
	 *     	}
	 *     
	 *      abstract sig AOUT {}
	 *     	one sig aNullOut extends AOUT {}
	 * </pre>
	 */
	public Formula decls2() {
		final Formula f0 = aNullIn.one().and(aNullIn.union(transfer).eq(AIN));
		final Formula f1 = aNullIn.intersection(transfer).no();
		final Formula f2 = details.function(transfer, TransferDetails);
		final Variable t1 = Variable.unary("t1"), t2 = Variable.unary("t2");
		final Formula f3 = t1.join(details).join(from).eq(
				t2.join(details).join(from));
		final Formula f4 = t1.join(details).join(to).eq(
				t2.join(details).join(to));
		final Formula f5 = t1.join(details).join(value).eq(
				t2.join(details).join(value));
		final Formula f6 = (f3.and(f4).and(f5)).implies(t1.eq(t2)).forAll(
				t1.oneOf(transfer).and(t2.oneOf(transfer)));
		final Formula f7 = aNullOut.one().and(aNullOut.eq(AOUT));
		return f0.and(f1).and(f2).and(f6).and(f7);
	}

	/**
	 * Computes the Authentic predicate.
	 * 
	 * @return
	 * 
	 * <pre>
	 *      pred Authentic (w : AbWorld, name_in : NAME) {
	 *     	 name_in in names (w)
	 *     	}
	 * </pre>
	 */
	public Formula authentic(Expression/* AbWorld */w,
			Expression/* NAME */name_in) {
		return name_in.in(names(w));
	}

	/**
	 * Computes the SufficientFundsProperty predicate.
	 * 
	 * @return
	 * 
	 * <pre>
	 *      pred SufficientFundsProperty (w : AbWorld, details_in : TransferDetails) {
	 *     	 some p : AbPurse {
	 *     	  (details_in.from).(w.abAuthPurse) = p
	 *     	  details_in.value in p.balance
	 *     	 }
	 *     }
	 * </pre>
	 */
	public Formula sufficientFundsProperty(Expression/* AbWorld */w,
			Expression/* TransferDetails */details_in) {
		final Variable p = Variable.unary("p");
		final Formula f0 = (details_in.join(from)).join(w.join(abAuthPurse))
				.eq(p);
		final Formula f1 = details_in.join(value).in(p.join(balance));
		return (f0.and(f1)).forSome(p.oneOf(AbPurse));
	}

	/**
	 * Computes the AbOp predicate.
	 * 
	 * @return
	 * 
	 * <pre>
	 *      pred AbOp (w, w' : AbWorld, a_in : AIN, a_out : AOUT) {
	 *     	 a_out = aNullOut
	 *     	}
	 * </pre>
	 */
	public Formula abOp(Expression/* AbWorld */w,
			Expression/* AbWorld */wprime, Expression/* AIN */a_in,
			Expression/* AOUT */a_out) {
		return a_out.eq(aNullOut);
	}

	/**
	 * Computes the AbIgnore predicate.
	 * 
	 * @return
	 * 
	 * <pre>
	 *      pred AbIgnore (w, w' : AbWorld, a_in : AIN, a_out : AOUT) {
	 *     	 AbOp (w, w', a_in, a_out)
	 *     	 w.abAuthPurse = w'.abAuthPurse
	 *     }
	 * </pre>
	 */
	public Formula abIgnore(Expression/* AbWorld */w,
			Expression/* AbWorld */wprime, Expression/* AIN */a_in,
			Expression/* AOUT */a_out) {
		return abOp(w, wprime, a_in, a_out).and(
				w.join(abAuthPurse).eq(wprime.join(abAuthPurse)));
	}

	/**
	 * Computes the AbWorldSecureOp predicate.
	 * 
	 * @return
	 * 
	 * <pre>
	 *      pred AbWorldSecureOp (
	 *     	 w, w' : AbWorld, a_in : AIN, a_out : AOUT, details_in : TransferDetails
	 *     	 ) {
	 *     	 AbOp (w, w', a_in, a_out)
	 *     	 a_in in transfer
	 *     	 (NAME - details_in.from - details_in.to) &lt;: w.abAuthPurse =
	 *     	 (NAME - details_in.from - details_in.to) &lt;: w'.abAuthPurse
	 *     	 }
	 * </pre>
	 */
	public Formula abWorldSecureOp(Expression/* AbWorld */w,
			Expression/* AbWorld */wprime, Expression/* AIN */a_in,
			Expression/* AOUT */a_out,
			Expression/* TransferDetails */details_in) {
		final Formula f0 = abOp(w, wprime, a_in, a_out).and(a_in.in(transfer));
		final Expression e0 = (NAME.difference(details_in.join(from))
				.difference(details_in.join(to))).product(AbPurse);
		final Formula f1 = e0.intersection(w.join(abAuthPurse)).eq(
				e0.intersection(wprime.join(abAuthPurse)));
		return f0.and(f1);
	}

	/**
	 * Computes the predicate AbTransferOkayTD.
	 * 
	 * @return
	 * 
	 * <pre>
	 *      pred AbTransferOkayTD (
	 *     	 w, w' : AbWorld, a_in : AIN, a_out : AOUT, details_in : TransferDetails
	 *     	 ) {
	 *     	 AbWorldSecureOp (w, w', a_in, a_out, details_in)
	 *     	 Authentic (w, details_in.from)
	 *     	 Authentic (w, details_in.to)
	 *     	 SufficientFundsProperty (w, details_in)
	 *     	 details_in.to not = details_in.from
	 *     	 some details_in.value
	 *     	 some p_from, p_from' : AbPurse | {
	 *     	 details_in.from.(w.abAuthPurse) = p_from
	 *     		details_in.from.(w'.abAuthPurse) = p_from'
	 *     		p_from'.balance = p_from.balance - details_in.value
	 *     		p_from'.lost = p_from.lost
	 *     	}
	 *     	some p_to, p_to' : AbPurse | {
	 *     		details_in.to.(w.abAuthPurse) = p_to
	 *     		details_in.to.(w'.abAuthPurse) = p_to'
	 *     		p_to'.balance =  p_to.balance + details_in.value
	 *     		p_to'.lost = p_to.lost
	 *     	}
	 *     }
	 * </pre>
	 */
	public Formula abTransferOkayTD(Expression/* AbWorld */w,
			Expression/* AbWorld */wprime, Expression/* AIN */a_in,
			Expression/* AOUT */a_out,
			Expression/* TransferDetails */details_in) {
		final Formula f0 = abWorldSecureOp(w, wprime, a_in, a_out, details_in);
		final Formula f1 = authentic(w, details_in.join(from));
		final Formula f2 = authentic(w, details_in.join(to));
		final Formula f3 = sufficientFundsProperty(w, details_in);
		final Formula f4 = details_in.join(to).eq(details_in.join(from)).not();
		final Formula f5 = details_in.join(value).some();
		final Variable pf0 = Variable.unary("p_from"), pf1 = Variable
				.unary("p_from'");
		final Formula f6 = (details_in.join(from).join(w.join(abAuthPurse))
				.eq(pf0)).and(details_in.join(from).join(
				wprime.join(abAuthPurse)).eq(pf1));
		final Formula f7 = pf1.join(balance).eq(
				pf0.join(balance).difference(details_in.join(value)));
		final Formula f8 = pf1.join(lost).eq(pf0.join(lost));
		final Formula f9 = f6.and(f7).and(f8).forSome(
				pf0.oneOf(AbPurse).and(pf1.oneOf(AbPurse)));
		final Variable pt0 = Variable.unary("p_to"), pt1 = Variable
				.unary("p_to'");
		final Formula f10 = details_in.join(to).join(w.join(abAuthPurse)).eq(
				pt0);
		final Formula f11 = details_in.join(to).join(wprime.join(abAuthPurse))
				.eq(pt1);
		final Formula f12 = pt1.join(balance).eq(
				pt0.join(balance).union(details_in.join(value)));
		final Formula f13 = pt1.join(lost).eq(pt0.join(lost));
		final Formula f14 = f10.and(f11).and(f12).and(f13).forSome(
				pt0.oneOf(AbPurse).and(pt1.oneOf(AbPurse)));
		return f0.and(f1).and(f2).and(f3).and(f4).and(f5).and(f9).and(f14);
	}

	/**
	 * Computes the AbTransferOkay predicate.
	 * 
	 * @return
	 * 
	 * <pre>
	 *      pred AbTransferOkay (
	 *     	 w, w' : AbWorld, a_in : AIN, a_out : AOUT	
	 *     ) {
	 *     	some details_in : TransferDetails | AbTransferOkayTD (w, w', a_in, a_out, details_in)
	 *     }
	 * </pre>
	 */
	public Formula abTransferOkay(Expression/* AbWorld */w,
			Expression/* AbWorld */wprime, Expression/* AIN */a_in,
			Expression/* AOUT */a_out) {
		final Variable d = Variable.unary("details_in");
		return abTransferOkayTD(w, wprime, a_in, a_out, d).forSome(
				d.oneOf(TransferDetails));
	}

	/**
	 * Computes the AbTransferLostTD predicate.
	 * 
	 * @return
	 * 
	 * <pre>
	 *     pred AbTransferLostTD (
	 *    	 w, w' : AbWorld, a_in : AIN, a_out : AOUT, details_in : TransferDetails
	 *    	 ) {
	 *    	 AbWorldSecureOp (w, w', a_in, a_out, details_in)
	 *    	 Authentic (w, details_in.from)
	 *    	 Authentic (w, details_in.to)
	 *    	 SufficientFundsProperty (w, details_in)
	 *    	 details_in.to not = details_in.from
	 *    	 some details_in.value
	 *    	 some p_from, p_from' : AbPurse {
	 *    	 details_in.from.(w.abAuthPurse) = p_from
	 *    	 details_in.from.(w'.abAuthPurse) = p_from'
	 *    	 p_from'.balance = p_from.balance - details_in.value
	 *    	 p_from'.lost = p_from.lost + details_in.value
	 *    	 }
	 *    	 details_in.to.(w.abAuthPurse) = details_in.to.(w'.abAuthPurse)
	 *    	 }
	 * </pre>
	 * 
	 */
	public Formula abTransferLostTD(Expression/* AbWorld */w,
			Expression/* AbWorld */wprime, Expression/* AIN */a_in,
			Expression/* AOUT */a_out,
			Expression/* TransferDetails */details_in) {
		final Formula f0 = abWorldSecureOp(w, wprime, a_in, a_out, details_in);
		final Formula f1 = authentic(w, details_in.join(from));
		final Formula f2 = authentic(w, details_in.join(to));
		final Formula f3 = sufficientFundsProperty(w, details_in);
		final Formula f4 = details_in.join(to).eq(details_in.join(from)).not();
		final Formula f5 = details_in.join(value).some();
		final Variable pf0 = Variable.unary("p_from"), pf1 = Variable
				.unary("p_from'");
		final Formula f6 = details_in.join(from).join(w.join(abAuthPurse)).eq(
				pf0).and(
				details_in.join(from).join(wprime.join(abAuthPurse)).eq(pf1));
		final Formula f7 = pf1.join(balance).eq(
				pf0.join(balance).difference(details_in.join(value)));
		final Formula f8 = pf1.join(lost).eq(
				pf0.join(lost).union(details_in.join(value)));
		final Formula f9 = f6.and(f7).and(f8).forSome(
				pf0.oneOf(AbPurse).and(pf1.oneOf(AbPurse)));
		final Formula f10 = details_in.join(to).join(w.join(abAuthPurse)).eq(
				details_in.join(to).join(wprime.join(abAuthPurse)));
		return f0.and(f1).and(f2).and(f3).and(f4).and(f5).and(f9).and(f10);
	}

	/**
	 * Computes the AbTransferLost predicate.
	 * 
	 * @return
	 * 
	 * <pre>
	 *    pred AbTransferLost (w, w' : AbWorld, a_in : AIN, a_out : AOUT) {
	 *   	 some details_in : TransferDetails | AbTransferLostTD (w, w', a_in, a_out, details_in)
	 *   	 }
	 * </pre>
	 */
	public Formula abTransferLost(Expression/* AbWorld */w,
			Expression/* AbWorld */wprime, Expression/* AIN */a_in,
			Expression/* AOUT */a_out) {
		final Variable d = Variable.unary("details_in");
		return abTransferLostTD(w, wprime, a_in, a_out, d).forSome(
				d.oneOf(TransferDetails));
	}

	/**
	 * Computes the AbTransfer predicate.
	 * 
	 * @return
	 * 
	 * <pre>
	 *   pred AbTransfer (w, w' : AbWorld, a_in : AIN, a_out : AOUT) {
	 *  	  AbTransferOkay (w, w', a_in, a_out) or 
	 *     AbTransferLost (w, w', a_in, a_out) or AbIgnore (w, w', a_in, a_out)
	 *  	 }
	 * </pre>
	 */
	public Formula abTransfer(Expression/* AbWorld */w,
			Expression/* AbWorld */wprime, Expression/* AIN */a_in,
			Expression/* AOUT */a_out) {
		return abTransferOkay(w, wprime, a_in, a_out).or(
				abTransferLost(w, wprime, a_in, a_out)).or(
				abIgnore(w, wprime, a_in, a_out));
	}

	/**
	 * Returns all signature declarations and facts in the model.
	 * 
	 * @return decls0() && decls1() && decls2()
	 */
	public Formula decls() {
		return decls0().and(decls1()).and(decls2());
	}

	/**
	 * Computes the assertion A241.
	 * 
	 * @return
	 * 
	 * <pre>
	 *  assert A241 {
	 * 	 all w, w' : AbWorld, a_in : AIN, a_out : AOUT |
	 * 	 AbTransferOkay (w, w', a_in, a_out) implies {
	 * 	 NoValueCreation (w, w')
	 * 	 AllValueAccounted (w, w')
	 * 	 }
	 * 	 }
	 * </pre>
	 */
	public Formula a241() {
		final Variable w = Variable.unary("w"), wprime = Variable.unary("w'"),
		      a_in = Variable.unary("a_in"), a_out = Variable.unary("a_out");
		final Formula f0 = abTransferOkay(w,wprime,a_in,a_out).
		    implies(noValueCreation(w,wprime).and(allValueAccounted(w,wprime)));
		final Formula f1 = f0.forAll(w.oneOf(AbWorld).and(wprime.oneOf(AbWorld)).and(a_in.oneOf(AIN)).and(a_out.oneOf(AOUT)));
		return decls().and(f1.not());
	}
	
	/**
	 * Returns the bounds for the given scope.
	 * @return bounds for the given scope.
	 */
	public Bounds bounds(int scope) {
		assert scope > 0;
		final List<String> atoms = new ArrayList<String>(6*scope + 1);
		for(int i = 0; i < scope; i++) atoms.add("Coin"+i);
		for(int i = 0; i < scope; i++) atoms.add("AbPurse"+i);
		for(int i = 0; i < scope; i++) atoms.add("NAME"+i);
		for(int i = 0; i < scope; i++) atoms.add("TransferDetails"+i);
		for(int i = 0; i < scope; i++) atoms.add("AbWorld0"+i);
		atoms.add("aNullIn");
		for(int i = 0; i < scope-1; i++) atoms.add("transfer"+i);
		atoms.add("aNullOut");
		
		final Universe u = new Universe(atoms);
		final TupleFactory f = u.factory();
		final Bounds b = new Bounds(u);
		
		final int last = scope-1;
		
		b.bound(Coin, f.range(f.tuple("Coin0"), f.tuple("Coin"+last)));
		b.bound(AbPurse, f.range(f.tuple("AbPurse0"), f.tuple("AbPurse"+last)));
		b.bound(NAME, f.range(f.tuple("NAME0"), f.tuple("NAME"+last)));
		b.bound(TransferDetails, f.range(f.tuple("TransferDetails0"), f.tuple("TransferDetails"+last)));
		b.bound(AbWorld0, f.range(f.tuple("AbWorld00"), f.tuple("AbWorld0"+last)));
		b.bound(AbWorld, b.upperBound(AbWorld0));
		b.boundExactly(aNullIn, f.setOf("aNullIn"));
		b.bound(transfer, f.range(f.tuple("transfer0"), f.tuple("transfer"+(last-1))));
		b.bound(AIN, f.range(f.tuple("aNullIn"), f.tuple("transfer"+(last-1))));
		b.boundExactly(aNullOut, f.setOf("aNullOut"));
		b.bound(AOUT, f.setOf("aNullOut"));
		
		b.bound(balance, b.upperBound(AbPurse).product(b.upperBound(Coin)));
		b.bound(lost, b.upperBound(balance));
		b.bound(from, b.upperBound(TransferDetails).product(b.upperBound(NAME)));
		b.bound(to, b.upperBound(from));
		b.bound(value, b.upperBound(TransferDetails).product(b.upperBound(Coin)));
		b.bound(details, b.upperBound(AIN).product(b.upperBound(TransferDetails)));
		
		b.bound(abAuthPurse, b.upperBound(AbWorld0).product(b.upperBound(NAME)).product(b.upperBound(AbPurse)));
		return b;
	}
	
	public static void main(String[] args) {
		final AbstractWorldDefinitions model = new AbstractWorldDefinitions();
		final Solver solver = new Solver();
		solver.options().setSolver(SATFactory.ZChaffBasic);
//		solver.options().setSkolemize(false);
		
		try {
			final Formula formula = model.a241();
			final int scope = args.length==0 ? 3 : Integer.parseInt(args[0]);
			final Bounds bounds = model.bounds(scope);
//			System.out.println(formula);
//			System.out.println(bounds);
			System.out.println("scope: " + scope);
			final Solution sol = solver.solve(formula, bounds);
			System.out.println(sol);
		} catch (TimeoutException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
	}
	
}
