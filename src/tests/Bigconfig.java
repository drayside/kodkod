package tests;

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
import kodkod.instance.TupleSet;
import kodkod.instance.Universe;

/**
 * A kodkod encoding of bigconfig.als:
 * 
 * <pre>
 *  module internal/bigconfig
 *  
 *  abstract sig Site {}
 *  sig HQ, Sub extends Site {}
 * 
 *  sig Router {
 *  site: Site,
 *  link: set Router
 *  }
 * 
 *  pred invariants() {
 *  -- every site has at least one router  
 *  Site in Router.site
 *  
 *  -- links are symmetric and non-reflexive
 *  link = ~link 
 *  no link &amp; iden
 *  }
 * 
 *  pred subsConnectedToHQ() {
 *  -- every sub location is connected to an HQ location at the given time
 *  site.Sub in (site.HQ).link
 *  }
 * 
 *  pred connectedSites(sites: set Site) {
 *  -- all sites in the given set are connected to each other  
 *  all s: sites | sites - s in ((site.s).&circ;link).site
 *  }
 * 
 *  pred show() {
 *  invariants() &amp;&amp; subsConnectedToHQ() &amp;&amp; connectedSites(Site) 
 *  }
 * </pre>
 * 
 * @author Emina Torlak
 */
public class Bigconfig {
//	 sigs
	private final Relation Router, Site, HQ, Sub;
	// fields
	private final Relation site, link;
	
	/**
	 * Constructs an instance of BigConfig.
	 */
	Bigconfig() {
		Router = Relation.unary("Router");
		Site = Relation.unary("Site");
		HQ = Relation.unary("HQ");
		Sub = Relation.unary("Sub");
		site = Relation.binary("site");
		link = Relation.binary("link");
	}
	
	/**
	 * Returns the constraints implicit in signature and field declarations.
	 * @return 
	 *  abstract sig Site {}
	 *  sig HQ, Sub extends Site {}
	 * 
	 *  sig Router {
	 *  site: Site,
	 *  link: set Router
	 *  }
	 */
	Formula declarations() {
		// HQ + Sub in Site && no HQ & Sub
		final Formula hqSub = HQ.union(Sub).in(Site).and(HQ.intersection(Sub).no());
		// site is a function from Router to Site
		final Formula siteFun = site.function(Router, Site);
		// link in Router->Router
		final Formula links = link.in(Router.product(Router));
		return hqSub.and(siteFun).and(links);
	}
	
	/**
	 * Returns the invariants predicate.
	 * @return  pred invariants() {
	 *  -- every site has at least one router  
	 *  Site in Router.site
	 *  
	 *  -- links are symmetric and non-reflexive
	 *  link = ~link 
	 *  no link & iden
	 *  }
	 */
	Formula invariants() {
		Formula atLeastARouter = Site.in(Router.join(site));
		Formula linksSymmetric = link.eq(link.transpose());
		Formula linksNotReflexive = link.intersection(Expression.IDEN).no();
		return atLeastARouter.and(linksSymmetric).and(linksNotReflexive);
	}

	/**
	 * Returns the subsConnectedToHQ predicate.
	 * @return pred subsConnectedToHQ() {
	 *  -- every sub location is connected to an HQ location at the given time
	 *  site.Sub in (site.HQ).link
	 *  }
	 */
	Formula subsConnectedToHQ() {
		return site.join(Sub).in(site.join(HQ).join(link));
	}
	
	/**
	 * Returns the connectedSites predicate.
	 * @return pred connectedSites(sites: set Site) {
	 *  -- all sites in the given set are connected to each other  
	 *  all s: sites | sites - s in ((site.s).^link).site
	 *  }
	 */
	Formula connectedSites(Expression sites) {
		final Variable s = Variable.unary("s");
		final Expression sreachable = site.join(s).join(link.closure()).join(site);
		final Formula f = sites.difference(s).in(sreachable);
		return f.forAll(s.oneOf(sites));
	}
	
	/**
	 * Returns the show predicate.
	 * @return pred show() {
	 *  invariants() && subsConnectedToHQ() && connectedSites(Site) 
	 *  }
	 */
	Formula show() {
		return declarations().and(invariants()).and(subsConnectedToHQ()).and(connectedSites(Site));
	}
	
	/**
	 * Returns a bounds object that constructs the 'scope' for analyzing
	 * the commands, using the given values for the number of sites
	 * and headquarters.  The number of Routers is taken to be the same
	 * as the number of sites.
	 * @requires all arguments are positive and hqNum <= siteNum
	 * @return a bounds for the model
	 */
	Bounds bounds(int siteNum, int hqNum) {
		assert siteNum > 0 && hqNum > 0 && hqNum <= siteNum;
		final List<String> atoms = new ArrayList<String>(siteNum*2);
		for(int i = 0; i < siteNum; i++) {
			atoms.add("Site"+i);
		}
		for(int i = 0; i < siteNum; i++) {
			atoms.add("Router"+i);
		}
		final Universe u = new Universe(atoms);
		final TupleFactory f = u.factory();
		
		final Bounds b = new Bounds(u);
		
		final String site0 = "Site0";
		final String siteN = "Site" + (siteNum-1);
		final String siteHQ = "Site" + (hqNum-1);
		final String siteSub = "Site" + hqNum;
		final String router0 = "Router0";
		final String routerN = "Router" + (siteNum-1);

		b.boundExactly(Site, f.range(f.tuple(site0), f.tuple(siteN)));
		b.boundExactly(HQ, f.range(f.tuple(site0), f.tuple(siteHQ)));
		if (hqNum < siteNum) {
			b.boundExactly(Sub, f.range(f.tuple(siteSub), f.tuple(siteN)));
		} else {
			b.bound(Sub, f.noneOf(1));
		}
		
		final TupleSet routers = f.range(f.tuple(router0), f.tuple(routerN));
		b.boundExactly(Router, routers);	
		b.bound(link, routers.product(routers));
		
		final TupleSet routerLocations = f.noneOf(2);
		for(int i = 0; i < siteNum; i++) {
			routerLocations.add(f.tuple("Router"+i, "Site"+i));
		}
		b.boundExactly(site, routerLocations);
		
		return b;
	}
	
	public static void main(String[] args) {
		final Bigconfig model = new Bigconfig();
		final Solver solver = new Solver();
		solver.options().setSolver(SATFactory.ZChaff);
		try {
			final Formula show = model.show();
			final Solution sol = solver.solve(show, model.bounds(Integer.parseInt(args[0]),Integer.parseInt(args[1])));
			System.out.println(show);
			System.out.println(sol);
			
		} catch (TimeoutException e) {
			System.out.println("timed out.");
			e.printStackTrace();
		} catch (NumberFormatException nfe) {
			System.out.println("Usage: java tests.Netconfig [# sites] [# hq]");
		}
	}
}
