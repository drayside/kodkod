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

/**
 * Pure Kodkod encoding of the new test case for ConfigAssure.
 * @author Emina Torlak
 */
public final class ConfigAssure {
	private final Relation port, addr, mask, subnet;
	
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
	Expression netid(Expression p) { 
		return addr(p).intersection(mask(p));
	}
	
	/**
	 * Returns the ip address of the given port.
	 * @return ip address of the given port
	 */
	Expression addr(Expression p) { return p.join(addr); }
	
	/**
	 * Returns the mask of the given port.
	 * @return mask of the given port
	 */
	Expression mask(Expression p) { return p.join(mask); }
	
	/**
	 * Returns the subnet of the given port.
	 * @return subnet of the given port
	 */
	Expression subnet(Expression p) { return p.join(subnet); }
	
	/**
	 * Returns the requirements.
	 * @return requirements
	 */
	public Formula requirements () { 
		
		final Variable p1 = Variable.unary("p1"), p2 = Variable.unary("p2");
		
		// no two ports have the same address: all disj p1, p2: Port | p1.addr != p2.addr
		final Formula f0 = p1.eq(p2).not().implies(addr(p1).eq(addr(p2)).not()).forAll(p1.oneOf(port).and(p2.oneOf(port)));
		
		// two ports on the same subnet have the same netid:  all p1, p2: Port | p1.subnet = p2.subnet => p1.netid = p2.netid
		final Formula f1 = subnet(p1).eq(subnet(p2)).implies(netid(p1).eq(netid(p2))).forAll(p1.oneOf(port).and(p2.oneOf(port)));
		
		// net ids don't overlap:  all disj p1, p2: Port | bitwise-and (p1.netid, p2.netid) = 0
		final Formula f2 = p1.eq(p2).not().implies(netid(p1).intersection(netid(p2)).no()).forAll(p1.oneOf(port).and(p2.oneOf(port)));
		
		// all addresses are in the range 121.96.0.0 to 121.96.255.255:  all p: Port | 121.96.0.0 <= p.addr <= 121.96.255.255
		final IntExpression p1bits = addr(p1).sum();
		final Formula f3 = IntConstant.constant(2036334592).lte(p1bits).and(p1bits.lte(IntConstant.constant(2036400127))).forAll(p1.oneOf(port));
		
		return Nodes.and(f0, f1, f2, f3);
	}
	
	/**
	 * Returns the universe from the given ipAddresses file.
	 * @throws IOException 
	 */
	private Universe universe(String ipAddresses) throws IOException { 
		final BufferedReader reader = new BufferedReader(new FileReader(new File(ipAddresses)));
		final List<Object> atoms = new ArrayList<Object>();
		
		final Pattern p = Pattern.compile("ipAddress\\((.+), (.+), \\S+, \\S+\\)\\.");
		
		String line = "";
		final Matcher m = p.matcher(line);
		
		for(line = reader.readLine(); line != null && m.reset(line).matches(); line = reader.readLine()) { 
			atoms.add(m.group(1) + "-" + m.group(2));
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
			
			bounds.boundExactly(port, factory.range(factory.tuple(universe.atom(0)), factory.tuple(universe.atom(universe.size()-32))));
			
			BufferedReader reader = new BufferedReader(new FileReader(new File(ipAddresses)));
			String line = "";
			
			// first parse the ipAddresses file and populate the upper and lower bounds of the addr and mask relations
			final TupleSet lAddr = factory.noneOf(2), uAddr = factory.noneOf(2);
			final TupleSet lMask = factory.noneOf(2), uMask = factory.noneOf(2);
			
			// example:  ipAddress('IOS_00096', 'FastEthernet0/0', 2036387617, 8).
			final Pattern p0 = Pattern.compile("ipAddress\\((.+), (.+), (\\S+), (\\S+)\\)\\.");
			final Pattern p1 = Pattern.compile("\\d+");
			
			final Matcher m0 = p0.matcher(line), m1 = p1.matcher(line);
			
			for(line = reader.readLine(); line != null && m0.reset(line).matches(); line = reader.readLine()) { 
				final String portName = m0.group(1) + "-" + m0.group(2);
				
				if (m1.reset(m0.group(3)).matches()) { // address is constant
					final int pAddr = Integer.parseInt(m0.group(3));
					for(int i = 0 ; i < 32; i++) {
						if ((pAddr & (1<<i)) != 0) {
							final Tuple tuple = factory.tuple(portName, Integer.valueOf(1<<i));
							lAddr.add(tuple);
							uAddr.add(tuple);
						}
					}
				} else {
					for(int i = 0 ; i < 32; i++) {
						uAddr.add(factory.tuple(portName, Integer.valueOf(1<<i)));
					}
				}
				
				if (m1.reset(m0.group(4)).matches()) { // mask is constant
					final int pMask = Integer.parseInt(m0.group(4));
					for(int i = 0 ; i < 32; i++) {
						if ((pMask & (1<<i)) != 0) {
							final Tuple tuple = factory.tuple(portName, Integer.valueOf(1<<i));
							lMask.add(tuple);
							uMask.add(tuple);
						}
					}
				} else {
					for(int i = 0 ; i < 32; i++) {
						uMask.add(factory.tuple(portName, Integer.valueOf(1<<i)));
					}
				}
			}
			
			
			bounds.bound(addr, lAddr, uAddr);
			bounds.bound(mask, lMask, uMask);
			
			// then parse the subnets file and populate the exact bound for the subnet relation
			
			final TupleSet bsubnet = factory.noneOf(2);
			reader = new BufferedReader(new FileReader(new File(subnets)));

			// example: subnet(['IOS_00022'-'Vlan172', 'IOS_00023'-'Vlan172']).
			final Pattern p2 = Pattern.compile("subnet\\(\\[(.+)(?:, (.+))*\\]\\)\\.");
			final Matcher m2 = p2.matcher(line);
			
			int n = 1;
			for(line = reader.readLine(); line != null && m2.reset(line).matches(); line = reader.readLine()) { 
				for(int i = 1, groups = m2.groupCount(); i < groups; i++) { 
					final String portName = m2.group(i);
					for(int j = 0 ; j < 32; i++) {
						if ((n & (1<<j)) != 0) {
							bsubnet.add(factory.tuple(portName, Integer.valueOf(1<<j)));
						}
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
		
		final Relation[] ports = new Relation[univ.size()-31];
		for(int i = 0; i < ports.length; i++) { 
			ports[i] = Relation.unary(univ.atom(i).toString());
			inst.add(ports[i], factory.setOf(ports[i].name()));
		}
		
		for(Relation p : ports) { 
			System.out.print(p);
			System.out.print(": addr=" + eval.evaluate(addr(p).sum()));
			System.out.print(", mask=" + eval.evaluate(mask(p).sum()));
			System.out.print(", netid=" + eval.evaluate(netid(p).sum()));
			System.out.println(", subnet=" + eval.evaluate(subnet(p).sum()) + ".");
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
		
		final Solution sol = solver.solve(ca.requirements(), ca.bounds(args[0], args[1]));
		
		if (sol.instance() != null) {
			System.out.println(sol.stats());
			ca.display(sol.instance(), solver.options());
		} else {
			System.out.println(sol);
		}
	}
}
