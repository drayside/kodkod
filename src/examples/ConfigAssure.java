/**
 * 
 */
package examples;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
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
	 * The mask relation maps port atoms to the integer atoms 1, 2, 4, 8 and 16, implicitly
	 * representing each mask by the number of zeros it contains [0..31]. 
	 **/
	private final Relation mask;
	
	/**
	 * The subnet relation maps one representative port atom in each subnet to all the other
	 * port atoms in the same subnet.  For example, the Prolog predicate subnet(['IOS_00037'-'Vlan790', 'IOS_00038'-'Vlan790'])
	 * is represented by the tuples <'IOS_00037'-'Vlan790', 'IOS_00037'-'Vlan790'> and <'IOS_00037'-'Vlan790', 'IOS_00038'-'Vlan790'>
	 * in the subnet relation.  We arbitrarily chose the first atom in each Prolog predicate to be representative of
	 * that subnet.  This encoding assumes that each port can participate in at most one subnet.
	 */
	private final Relation subnet;
	
	private final IntConstant ones = IntConstant.constant(-1);
	private final int min = (121<<24) | (96<<16);
	private final int max = min | (255<<8) | 255;
	
	/**
	 * Constructs a new instance of ConfigAssure.
	 */
	public ConfigAssure() {
		this.port = Relation.unary("port");
		this.addr = Relation.binary("addr");
		this.mask = Relation.binary("mask");
		this.subnet = Relation.binary("subnet");
	}

	/**
	 * Returns the netID of the given port.
	 * @return netID of the given port
	 */
	IntExpression netid(Expression p) { 
		return addr(p).and(mask(p));
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
	IntExpression zeros(Expression p) { return p.join(mask).sum(); }
	
	/**
	 * Returns the explicit mask of the given port.
	 * @return explicit mask of the given port
	 */
	IntExpression mask(Expression p) { return ones.shl(zeros(p)); }
	
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
		final Formula f0 = zeros(p0).gte(zeros(p1));
		final Formula f1 = addr(p0).and(mask(p0)).eq(addr(p1).and(mask(p0)));
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
		reqs.add( netid(p0).eq(netid(p1)).forAll(p0.oneOf(subreps).and(p1.oneOf(submembers))) );
		
		// netids don't overlap: 
		// all disj p0, p1: subreps | not contains(p0, p1) and not contains(p1, p0)
		reqs.add( p0.eq(p1).not().implies(contains(p0,p1).not().and(contains(p1, p0).not())).
					forAll(p0.oneOf(subreps).and(p1.oneOf(subreps)) ));	
		
		// all addresses are in the range 121.96.0.0 to 121.96.255.255:  
		// all p0: Port | 121.96.0.0 <= addr(p0) <= 121.96.255.255
		reqs.add( IntConstant.constant(min).lte(addr(p0)).and(addr(p0).lte(IntConstant.constant(max))).forAll(p0.oneOf(port)) );
						
		return Nodes.and(reqs);
	}
	
	/**
	 * Returns the universe from the given ipAddresses file.
	 * @throws IOException 
	 */
	private Universe universe(String ipAddresses) throws IOException { 
		final BufferedReader reader = new BufferedReader(new FileReader(new File(ipAddresses)));
		final List<Object> atoms = new ArrayList<Object>();
		
		final Pattern pDeviceInterface = Pattern.compile("ipAddress\\((.+), (.+), \\S+, \\S+\\)\\.");
		
		String line = "";
		final Matcher mDeviceInterface = pDeviceInterface.matcher(line);
		
		for(line = reader.readLine(); line != null && mDeviceInterface.reset(line).matches(); line = reader.readLine()) { 
			atoms.add(mDeviceInterface.group(1) + "-" + mDeviceInterface.group(2));
		}
		
		// add the integers
		for(int i = 0; i < 32; i++) { 
			atoms.add(Integer.valueOf(1<<i));
		}
		
		return new Universe(atoms);
	}

	/**
	 * Returns the bounds corresponding to the given ip address and subnet files.
	 * @return bounds corresponding to the given ip address and subnet files.
	 */
	public Bounds bounds(String ipAddresses, String subnets) { 
		try {
			final Universe universe = universe(ipAddresses);
			final Bounds bounds = new Bounds(universe);
			final TupleFactory factory = universe.factory();
			
			for(int i = 0; i < 32; i++) { 
				bounds.boundExactly(1<<i, factory.setOf(Integer.valueOf(1<<i)));
			}
			
			bounds.boundExactly(port, factory.range(factory.tuple(universe.atom(0)), factory.tuple(universe.atom(universe.size()-33))));
			BufferedReader reader = new BufferedReader(new FileReader(new File(ipAddresses)));
			String line = "";
			
			// first parse the ipAddresses file and populate the upper and lower bounds of the addr and mask relations
			final TupleSet lAddr = factory.noneOf(2), uAddr = factory.noneOf(2);
			final TupleSet lMask = factory.noneOf(2), uMask = factory.noneOf(2);
			
			// example:  ipAddress('IOS_00096', 'FastEthernet0/0', 2036387617, 8).
			final Pattern pAddress = Pattern.compile("ipAddress\\((.+), (.+), (\\S+), (\\S+)\\)\\.");
			final Pattern pDigits = Pattern.compile("\\d+");
			
			final Matcher mFullAddr = pAddress.matcher(line), mDigits = pDigits.matcher(line);
			
			for(line = reader.readLine(); line != null && mFullAddr.reset(line).matches(); line = reader.readLine()) { 
				final String portName = mFullAddr.group(1) + "-" + mFullAddr.group(2);
				
				if (mDigits.reset(mFullAddr.group(3)).matches()) { // address is constant
					final int addrVal = Integer.parseInt(mFullAddr.group(3));
					for(int i = 0 ; i < 32; i++) {
						if ((addrVal & (1<<i)) != 0) {
							final Tuple tuple = factory.tuple(portName, Integer.valueOf(1<<i));
							lAddr.add(tuple);
							uAddr.add(tuple);
						}
					}
				} else {
					for(int i = 0 ; i < 32; i++) {
						// specify a lower / upper bound based on the knowledge that
						// all addresses are between 21.96.0.0 to 121.96.255.255
						// (i.e. share the first 16 bits).
						if ((min & (1<<i))!=0)
							lAddr.add(factory.tuple(portName, Integer.valueOf(1<<i)));
						if ((max & (1<<i))!=0)
							uAddr.add(factory.tuple(portName, Integer.valueOf(1<<i)));
					}
				}
				
				if (mDigits.reset(mFullAddr.group(4)).matches()) { // mask is constant
					final int maskVal = Integer.parseInt(mFullAddr.group(4));
					// number of zeros must be between 0 and 31, inclusive
					if (maskVal < 0 || maskVal > 31)
						throw new RuntimeException("Illegal implicit mask definition (must be between 0 and 31 inclusive): " + line);
					for(int i = 0 ; i < 5; i++) { // need to look at only low-order 5 bits for the mask
						if ((maskVal & (1<<i)) != 0) {
							final Tuple tuple = factory.tuple(portName, Integer.valueOf(1<<i));
							lMask.add(tuple);
							uMask.add(tuple);
						}
					}
				} else {
					for(int i = 0 ; i < 5; i++) { // need only 5 bits for the mask
						uMask.add(factory.tuple(portName, Integer.valueOf(1<<i)));
					}
				}
			}
						
			bounds.bound(addr, lAddr, uAddr);
			bounds.bound(mask, lMask, uMask);
			
			// then parse the subnets file and populate the exact bound for the subnet relation
			reader = new BufferedReader(new FileReader(new File(subnets)));
			line = "";
			
			final TupleSet bsubnet = factory.noneOf(2);
			
			// example: subnet(['IOS_00022'-'Vlan172', 'IOS_00023'-'Vlan172']).
			final Pattern pSubnet = Pattern.compile("subnet\\(\\[(.+)\\]\\)\\.");
			final Pattern pSubMember = Pattern.compile(",*\\s*([^,]+)");
			final Matcher mSubnet = pSubnet.matcher(line), mSubMember = pSubMember.matcher(line);
			
			int n = 1;
			for(line = reader.readLine(); line != null && mSubnet.reset(line).matches(); line = reader.readLine()) { 

				mSubMember.reset(mSubnet.group(1));
				if (mSubMember.find()) { 
					final String repPort = mSubMember.group(1);
					bsubnet.add(factory.tuple(repPort, repPort));
					
					while(mSubMember.find()) { 
						bsubnet.add(factory.tuple(repPort, mSubMember.group(1)));	
					} 
				}
				n++;
			}
			
			bounds.boundExactly(subnet, bsubnet);
			
			return bounds;
			
		} catch (IOException e) {
			e.printStackTrace();
			usage();
		}
		
		return null;
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
			System.out.print(", mask=" + eval.evaluate(zeros(p)));
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
	}
}
