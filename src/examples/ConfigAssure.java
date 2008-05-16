/**
 * 
 */
package examples;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.IntConstant;
import kodkod.ast.IntExpression;
import kodkod.ast.Relation;
import kodkod.ast.Variable;
import kodkod.engine.Evaluator;
import kodkod.engine.Solution;
import kodkod.engine.Solver;
import kodkod.engine.config.Options;
import kodkod.engine.satlab.SATFactory;
import kodkod.instance.Bounds;
import kodkod.instance.Instance;
import kodkod.instance.Tuple;
import kodkod.instance.TupleFactory;
import kodkod.instance.TupleSet;
import kodkod.instance.Universe;
import kodkod.util.ints.IntSet;
import kodkod.util.ints.IntTreeSet;
import kodkod.util.nodes.Nodes;
import kodkod.util.nodes.PrettyPrinter;

/**
 * Pure Kodkod encoding of the new test case for ConfigAssure.
 * @author Emina Torlak
 */
public final class ConfigAssure {
	/** 
	 * port is a unary relation that contains all the port atoms, represented 
	 * by concatenating the device name and the interface of each port.
	 **/
	private final Relation port;
	
	/** The addr relation maps port atoms to all the power-of-2 atoms (1, 2, 4, ..., 2^31). */
	private final Relation addr;
	
	/**
	 * The subnet relation maps one representative port atom in each subnet to all the other
	 * port atoms in the same subnet.  For example, the Prolog predicate subnet(['IOS_00037'-'Vlan790', 'IOS_00038'-'Vlan790'])
	 * is represented by the tuples <'IOS_00037'-'Vlan790', 'IOS_00037'-'Vlan790'> and <'IOS_00037'-'Vlan790', 'IOS_00038'-'Vlan790'>
	 * in the subnet relation.  We arbitrarily chose the first atom in each Prolog predicate to be representative of
	 * that subnet.  This encoding assumes that each port can participate in at most one subnet.
	 */
	private final Relation subnet;
	
	/**
	 * The group relation maps all atoms in each subnet that have the same interface
	 * to one representative port atom.  For example, the Prolog predicate subnet(['IOS_00091'-'Vlan820', 'IOS_00092'-'Vlan820', 'IOS_00096'-'FastEthernet0/0'])
	 * is represented by the tuples <'IOS_00091'-'Vlan820', 'IOS_00091'-'Vlan820'>, <'IOS_00092'-'Vlan820', 'IOS_00091'-'Vlan820>, 
	 * and <'IOS_00096'-'FastEthernet0/0','IOS_00096'-'FastEthernet0/0'>
	 * in the group relation.  Ports that are not part of any subnet form their own group (of which they are a representative).
	 */
	private final Relation group;
	
	/** 
	 * The groupMask relation maps port atoms that are group representatives to the integer atoms 1, 2, 4, 8 and 16, implicitly
	 * representing each group's mask by the number of zeros it contains [0..31]. 
	 **/
	private final Relation groupMask;
	
	/**
	 * Join of the group relation with the groupMask relation: provides an implicit mask for each port.
	 */
	private final Expression mask;
	
	
	private final IntConstant ones = IntConstant.constant(-1);
	private final static int minAddr = (121<<24) | (96<<16);
	private final static int maxAddr = minAddr | (255<<8) | 255;
	
	/**
	 * Constructs a new instance of ConfigAssure.
	 */
	public ConfigAssure() {
		this.port = Relation.unary("port");
		this.addr = Relation.binary("addr");
		this.groupMask = Relation.binary("groupMask");
		this.subnet = Relation.binary("subnet");
		this.group = Relation.binary("group");
		this.mask = group.join(groupMask);
	}

	/**
	 * Returns the netID of the given port.
	 * @return netID of the given port
	 */
	IntExpression netid(Expression p) { 
		return addr(p).and(explicitMask(p));
	}
	
	/**
	 * Returns the ip address of the given port.
	 * @return ip address of the given port
	 */
	IntExpression addr(Expression p) { return p.join(addr).sum(); }
	
	/**
	 * Returns the number of zeros in the mask of the given port.
	 * @return the number of zeros in the mask of the given port
	 */
	IntExpression implicitMask(Expression p) { return p.join(mask).sum(); }
	
	/**
	 * Returns the explicit mask of the given port.
	 * @return explicit mask of the given port
	 */
	IntExpression explicitMask(Expression p) { return ones.shl(implicitMask(p)); }
	
	/**
	 * Returns the subnet of the given port.
	 * @return subnet of the given port
	 */
	Expression subnet(Expression p) { return p.join(subnet); }
	
	/**
	 * Returns a Formula that evaluates to true if the netid represented of the port p0
	 * contains the netid of the port p1.
	 * @return zeros(p0) >= zeros(p1) and (addr(p0) & mask(p0)) = (addr(p1) & mask(p0))
	 */
	Formula contains(Expression p0, Expression p1) { 
		final Formula f0 = implicitMask(p0).gte(implicitMask(p1));
		final Formula f1 = addr(p0).and(explicitMask(p0)).eq(addr(p1).and(explicitMask(p0)));
		return f0.and(f1);
	}
	
	/**
	 * Returns the requirements.
	 * @return requirements
	 */
	public Formula requirements () { 
		
		final List<Formula> reqs = new ArrayList<Formula>();
		
		final Variable p0 = Variable.unary("p0"), p1 = Variable.unary("p1"), p2 = Variable.unary("p2");
		
		// the domain of the subnet expression contains all the representative atoms for each subnet
		final Expression subreps = subnet.join(port);
		
		// no two ports on the same subnet have the same address:
		// all p0: subreps, disj p1, p2: p0.subnet | addr(p1) != addr(p2)
		final Expression submembers = p0.join(subnet);
		reqs.add( p1.eq(p2).not().implies(addr(p1).eq(addr(p2)).not()).
					forAll(p0.oneOf(subreps).and(p1.oneOf(submembers)).and(p2.oneOf(submembers)))  );
		
		// all ports on the same subnet have the same netid:
		// all p0: subreps, p1: p0.subnet | netid(p0) = netid(p1)
		reqs.add( netid(p0).eq(netid(p1)).
					forAll(p0.oneOf(subreps).and(p1.oneOf(submembers))) );
		
		// netids don't overlap: 
		// all disj p0, p1: subreps | not contains(p0, p1) and not contains(p1, p0)
		reqs.add( p0.eq(p1).not().implies(contains(p0,p1).not().and(contains(p1, p0).not())).
					forAll(p0.oneOf(subreps).and(p1.oneOf(subreps)) ));	
								
		return Nodes.and(reqs);
	}

	/**
	 * Returns a new universe that contains the given ports atoms, followed by Integer objects
	 * containing powers of 2 (1, 2, 4, ..., 2^31).
	 */
	private final Universe universe(Set<String> ports) { 
		final List<Object> atoms = new ArrayList<Object>(ports.size() + 32);
		atoms.addAll(ports);
		for(int i = 0; i < 32; i++) { 
			atoms.add(Integer.valueOf(1<<i));
		}
		return new Universe(atoms);
	}
	
	/**
	 * Returns the bounds corresponding to the given ip address and subnet files.
	 * @return bounds corresponding to the given ip address and subnet files.
	 * @throws IOException if either of the files cannot be found or accessed
	 * @throws IllegalArgumentException if either of the files cannot be parsed
	 */
	public Bounds bounds(String ipAddrsFile, String subnetsFile) throws IOException { 
		final Map<String, IPAddress> addresses = parseAddresses(ipAddrsFile);
		final Collection<Subnet> subnets = parseSubnets(subnetsFile, addresses);
		
		final Universe universe = universe(addresses.keySet());
		final Bounds bounds = new Bounds(universe);
		final TupleFactory factory = universe.factory();

		// bind the integers
		for(int i = 0; i < 32; i++) { bounds.boundExactly(1<<i, factory.setOf(Integer.valueOf(1<<i))); }

		// bind the port relation exactly to the port names
		bounds.boundExactly(port, factory.range(factory.tuple(1, 0), factory.tuple(1, addresses.keySet().size()-1)));
		
		// bind the subnet relation exactly, choosing the first element of each subnet as the representative
		final TupleSet subnetBound = factory.noneOf(2);
		for(Subnet sub : subnets) { 
			final Iterator<IPAddress> itr = sub.members.iterator();
			final String first = itr.next().port;
			subnetBound.add(factory.tuple(first, first));
			while(itr.hasNext()) { 
				subnetBound.add(factory.tuple(first, itr.next().port));
			}
		}
		bounds.boundExactly(subnet, subnetBound);
		
		// bind the addr relation so that each address is guaranteed to be between minAddr (121.96.0.0) and maxAddr (121.96.255.255), inclusive
		final TupleSet lAddr = factory.noneOf(2), uAddr = factory.noneOf(2);
		for(IPAddress ad : addresses.values()) { 
			if (ad.varAddress) { // unknown address
				for(int i = 0 ; i < 32; i++) {
					if ((minAddr & (1<<i))!=0)
						lAddr.add(factory.tuple(ad.port, Integer.valueOf(1<<i)));
					if ((maxAddr & (1<<i))!=0)
						uAddr.add(factory.tuple(ad.port, Integer.valueOf(1<<i)));
				}
			} else { // known address
				for(int i = 0 ; i < 32; i++) {
					if ((ad.address & (1<<i)) != 0) {
						final Tuple tuple = factory.tuple(ad.port, Integer.valueOf(1<<i));
						lAddr.add(tuple);
						uAddr.add(tuple);
					}
				}
			}
		}
		bounds.bound(addr, lAddr, uAddr);
		
		// bind the group and groupMask relations so that all ports with the same interface on the same subnet are guaranteed to have the same mask
		final TupleSet lMask = factory.noneOf(2), uMask = factory.noneOf(2), groupBound = factory.noneOf(2);
		for(Subnet sub : subnets) { 
			
			for(Set<IPAddress> subgroup : sub.groups.values()) {
				
				final Iterator<IPAddress> itr = subgroup.iterator();
				
				final IPAddress first = itr.next();
				addresses.remove(first.port);
				
				int maskVal = first.varMask ? -1 : first.mask;
				groupBound.add(factory.tuple(first.port, first.port));
				
				while(itr.hasNext()) { 
					final IPAddress next = itr.next();
					addresses.remove(next.port);
					
					if (maskVal < 0 && !next.varMask)
						maskVal = next.mask;
					groupBound.add(factory.tuple(next.port, first.port));
				}
				
				if (maskVal < 0) { // unknown (same) mask for this group
					for(int i = 0 ; i < 5; i++) { // need only 5 bits for the mask
						uMask.add(factory.tuple(first.port, Integer.valueOf(1<<i)));
					}
				} else { // known (same) mask for this group
					for(int i = 0 ; i < 5; i++) { // need to look at only low-order 5 bits for the mask
						if ((maskVal & (1<<i)) != 0) {
							final Tuple tuple = factory.tuple(first.port, Integer.valueOf(1<<i));
							lMask.add(tuple);
							uMask.add(tuple);
						}
					}
				}
			}
		}
		
		// bind the group and groupMask relations for ports that are not a part of any subnet
		for(IPAddress ad : addresses.values()) { 
			groupBound.add(factory.tuple(ad.port, ad.port));
			if (ad.varMask) { // unknown (same) mask for this group
				for(int i = 0 ; i < 5; i++) { // need only 5 bits for the mask
					uMask.add(factory.tuple(ad.port, Integer.valueOf(1<<i)));
				}
			} else { // known (same) mask for this group
				for(int i = 0 ; i < 5; i++) { // need to look at only low-order 5 bits for the mask
					if ((ad.mask & (1<<i)) != 0) {
						final Tuple tuple = factory.tuple(ad.port, Integer.valueOf(1<<i));
						lMask.add(tuple);
						uMask.add(tuple);
					}
				}
			}
		}
		
		bounds.bound(groupMask, lMask, uMask);
		bounds.boundExactly(group, groupBound);
		
		System.out.println("groupMask.size: " + uMask.size() + ", group.size: " + groupBound.size());
		return bounds;

	}
	
	/**
	 * Displays an instance obtained with the given options.
	 * @requires inst != null and opt != null
	 */
	private final void display(Instance inst, Options opt) { 
		final Universe univ = inst.universe();
		final Evaluator eval = new Evaluator(inst, opt);
		final TupleFactory factory = univ.factory();
		final List<TupleSet> subnets = new ArrayList<TupleSet>();
		
		System.out.println("--------addresses and masks--------");
		for(int i = 0, ports = univ.size()-32; i < ports; i++) {
			final Object atom = univ.atom(i);
			final Relation p = Relation.unary(atom.toString());
			inst.add(p, factory.setOf(atom));
			
			System.out.print(p);
			System.out.print(": addr=" + eval.evaluate(addr(p)));
			System.out.print(", mask=" + eval.evaluate(implicitMask(p)));
			System.out.println(", netid=" + eval.evaluate(netid(p)) + ".");
			
			final TupleSet members = eval.evaluate(subnet(p));
			if (!members.isEmpty())
				subnets.add(members);
		}
		
		System.out.println("--------subnets--------");
		for(TupleSet sub : subnets) { 
			System.out.println(sub);
		}
		
	}
	
	private static void usage() {
		System.out.println("java examples.ConfigAssure <ipAddresses file> <subnets file>");
		System.exit(1);
	}
	
	/**
	 * Usage: java examples.ConfigAssure <ipAddresses file> <subnets file>
	 */
	public static void main(String[] args) {
		if (args.length < 2) usage();
		try {
			final ConfigAssure ca = new ConfigAssure();
			final Solver solver = new Solver();
			solver.options().setBitwidth(32);
			solver.options().setSolver(SATFactory.MiniSat);
			
			
			final Formula formula = ca.requirements();
			final Bounds bounds = ca.bounds(args[0], args[1]);
			
			System.out.println("requirements: ");
			System.out.println(PrettyPrinter.print(formula, 2));
			
			System.out.println("solving with config files " + args[0] + " and " + args[1]);
			
			final Solution sol = solver.solve(formula, bounds);
			
			System.out.println("---OUTCOME---");
			System.out.println(sol.outcome());
			
			System.out.println("\n---STATS---");
			System.out.println(sol.stats());
			
			if (sol.instance() != null) {
				System.out.println("\n---INSTANCE--");
				ca.display(sol.instance(), solver.options());
			} 
		} catch (IOException ioe) { 
			ioe.printStackTrace();
			usage();
		}
	}
	
	/**
	 * Parses the given ipAddresses file and returns a map that 
	 * binds each port name in the file to its corresponding IPAddress record.
	 * @return a map that binds each port name in the given file to its corresponding IPAddress record.
	 * @throws IOException 
	 */
	private static Map<String, IPAddress> parseAddresses(String ipAddrsFile) throws IOException { 
		final Map<String, IPAddress> addresses = new LinkedHashMap<String, IPAddress>();
		final BufferedReader reader = new BufferedReader(new FileReader(new File(ipAddrsFile)));
		for(String line = reader.readLine(); line != null; line = reader.readLine()) { 
			final IPAddress addr = new IPAddress(line);
			if (addresses.put(addr.port, addr) != null) 
				throw new IllegalArgumentException("Duplicate ip address specification: " + line);
		}
		return addresses;
	}
	
	/**
	 * Returns a grouping of the given IPAddresses into subnets according to the subnet information
	 * in the given file.  
	 * @return a grouping of the given IPAddresses into subnets according to the subnet information
	 * in the given file.
	 * @throws IOException 
	 */
	private static Collection<Subnet> parseSubnets(String subnetsFile, Map<String, IPAddress> addresses) throws IOException { 
		final Collection<Subnet> subnets = new ArrayList<Subnet>();
		final BufferedReader reader = new BufferedReader(new FileReader(new File(subnetsFile)));
		for(String line = reader.readLine(); line != null; line = reader.readLine()) { 
			subnets.add(new Subnet(line, addresses));
		}
		return subnets;
	}
	
	/**
	 * A record containing  the information parsed out of a 
	 * Prolog ipAddress predicate, e.g. ipAddress('IOS_00008', 'Vlan608', int(0), mask(1)).
	 * @specfield device, interfaceName, port: String
	 * @specfield varAddress, varMask: boolean // true if the address (resp. mask) are variables
	 * @specfield address, mask: int // variable or constant identifier of the address or mask
	 * @invariant port = device + "-" + port
	 * @invariant !varAddress => (minAddr <= address <= maxAddr)
	 * @invariant !varMask => (0 <= mask <= 31)
	 * @author Emina Torlak
	 */
	private static class IPAddress {
		final String device, interfaceName, port;
		final boolean varAddress, varMask; 
		final int address, mask;
		
		private static final Pattern pAddress = Pattern.compile("ipAddress\\((.+), (.+), (\\S+), (\\S+)\\)\\.");
		private static final Pattern pAddrVar = Pattern.compile("int\\((\\d+)\\)");
		private static final Pattern pMaskVar = Pattern.compile("mask\\((\\d+)\\)");
		
		private static final Matcher mAddress = pAddress.matcher("");
		private static final Matcher mAddrVar = pAddrVar.matcher(""), mMaskVar = pMaskVar.matcher("");
		
		/**
		 * Constructs an IP address object using the provided ipAddress string.
		 */
		IPAddress(String addrString) { 
			if (mAddress.reset(addrString).matches()) { 
				this.device = mAddress.group(1);
				this.interfaceName = mAddress.group(2);
				this.port = device + "-" + interfaceName;
				if (mAddrVar.reset(mAddress.group(3)).matches()) { 
					this.varAddress = true;
					this.address = Integer.parseInt(mAddrVar.group(1));
				} else {
					this.varAddress = false;
					this.address = parseConstant(mAddress.group(3), minAddr, maxAddr, "Expected the address to be a variable spec, int(<number>), or a number between " + minAddr + " and " + maxAddr + ", inclusive: " + addrString);
				}
				if (mMaskVar.reset(mAddress.group(4)).matches()) { 
					this.varMask = true;
					this.mask = Integer.parseInt(mMaskVar.group(1));
				} else {
					this.varMask = false;
					this.mask = parseConstant(mAddress.group(4), 0, 31, "Expected the mask to be a variable spec, mask(<number>), or a number between 0 and 31, inclusive: " + addrString);
				}
			} else {
				throw new IllegalArgumentException("Unrecognized IP Address format: " + addrString);
			}
		}
	
		/**
		 * Returns the integer value embedded in the given string iff it is between min
		 * and max, inclusive.  Otherwise throws an illegal argument exception with the
		 * given message.
		 */
		private static int parseConstant(String value, int min, int max, String msg) { 
			try {
				final int val = Integer.parseInt(value);
				if (min <= val && val <= max) { 
					return val;
				}
			} catch (NumberFormatException nfe) { }
			throw new IllegalArgumentException(msg);
		}
	}
	
	/**
	 * A record containing  the information parsed out of a 
	 * Prolog subnet predicate, e.g. subnet(['IOS_00091'-'Vlan820', 'IOS_00092'-'Vlan820', 'IOS_00096'-'FastEthernet0/0']).
	 * @specfield member: some IPAddress // members of this subnet
	 * @specfield groups: String -> member
	 * @invariant groups = { s: String, a: IPAddress | a.interfaceName = s }
	 * @invariant all i: member.interfaceName, m1, m2: groups[i] | (!m1.varMask and !m2.varMask) => m1.mask = m2.mask
	 * @author Emina Torlak
	 */
	private static class Subnet {
		final Set<IPAddress> members;
		final Map<String, Set<IPAddress>> groups;
		
		private static final Pattern pSubnet = Pattern.compile("subnet\\(\\[(.+)\\]\\)\\.");
		private static final Pattern pSubMember = Pattern.compile(",*\\s*([^,]+)");
		private static final Matcher mSubnet = pSubnet.matcher(""), mSubMember = pSubMember.matcher("");
		
		/**
		 * Constructs a subnet object out of the given subnet string and addresses.
		 */
		Subnet(String subnetString, Map<String, IPAddress> addresses) { 
			this.members = members(subnetString, addresses);
			this.groups = groups(subnetString, members);
		}
		
		/**
		 * Returns the subnet members specified by the given subnet string.
		 * @return subnet members specified by the given subnet string.
		 */
		private static Set<IPAddress> members(String subnetString, Map<String, IPAddress> addresses) { 
			if (mSubnet.reset(subnetString).matches()) { 
				final Set<IPAddress> members = new LinkedHashSet<IPAddress>();
				mSubMember.reset(mSubnet.group(1));
				while(mSubMember.find()) { 
					final String port = mSubMember.group(1);
					if (addresses.containsKey(port)) { 
						members.add(addresses.get(port));
					} else {
						throw new IllegalArgumentException("Unrecognized port " + port + " in " + subnetString);
					}
				}
				if (members.isEmpty()) { 
					throw new IllegalArgumentException("Subnet spec is empty: " + subnetString);
				}
				return Collections.unmodifiableSet(members);
			} else {
				throw new IllegalArgumentException("Unrecognized subnet format: " + subnetString);
			}
		}
		
		/**
		 * Returns a grouping of the given subnet members according to their interface names.
		 * @return a grouping of the given subnet members according to their interface names.
		 */
		private static Map<String, Set<IPAddress>> groups(String subnetString, Set<IPAddress> members) { 
			final Map<String, Set<IPAddress>> groups = new LinkedHashMap<String, Set<IPAddress>>();
			for(IPAddress addr : members) { 
				Set<IPAddress> group = groups.get(addr.interfaceName);
				if (group == null) {
					group = new LinkedHashSet<IPAddress>();
					groups.put(addr.interfaceName, group);
				}
				group.add(addr);
				
			}
			for(Map.Entry<String, Set<IPAddress>> entry : groups.entrySet()) { 
				final Set<IPAddress> group = entry.getValue();
				final IntSet masks = new IntTreeSet();
				for(IPAddress addr : group) { 
					if (!addr.varMask) { masks.add(addr.mask); }
				}
				if (masks.size()>1) 
					throw new IllegalArgumentException("All members of a subnet with the same interface must have the same mask: " + subnetString);
				
				entry.setValue(Collections.unmodifiableSet(group));
			}
			return Collections.unmodifiableMap(groups);
		}
	}
}
