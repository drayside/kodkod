package examples.sudoku_dumbed_down;

import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import kodkod.instance.Tuple;
import kodkod.instance.TupleFactory;
import kodkod.instance.TupleSet;
import kodkod.instance.Universe;

public final class SudokuParser {
	private SudokuParser() {}
	
	private static String[] split(String puzzle) { 
		final String[] parsed = puzzle.split("\\s+");
		return (parsed.length>1) ? parsed : puzzle.replaceAll("(\\d)", "$1 ").split(" ");
	}
		
	public static final TupleSet parse(String[] puzzle, Universe univ) { 
		final int n = (int)StrictMath.sqrt(puzzle.length);
		final int r = (int)StrictMath.sqrt(n);
		if (puzzle.length!=r*r*r*r)  throw new IllegalArgumentException("Not a valid puzzle spec: expected " + (r*r*r*r) + " numbers but found " + puzzle.length);
				
		final TupleFactory f = univ.factory();
		
		final TupleSet givens = f.noneOf(3);
		
		for(int i = 0; i < n; i++) { 
			for(int j = 0; j < n; j++) { 
				try {
					final int digit = Integer.parseInt(puzzle[i*n+j]);
					if (digit>0 && digit<=n) {
						final Tuple t = f.tuple(i+1, j+1, digit);
						givens.add(t);
					} else if (digit>n) { 
						throw new IllegalArgumentException("Not a valid puzzle spec: expected numbers from [0, " + n+"] but found "+digit);
					} 
				} catch (NumberFormatException nfe) { 
					throw new IllegalArgumentException("Not a valid puzzle spec: expected numbers from [0, " + n+"] but found "+puzzle[i*n+j]);
				}
			}
		}
		
		return givens;
	}
	
    public static final TupleSet parse(String[] puzzle) {
        final List<Object> atoms = new LinkedList<Object>();
        int n = (int) StrictMath.sqrt(puzzle.length);
        for (int i = 0; i < n; i++)
            atoms.add(i + 1);
        for (int i = 0; i < 1000; i++)
            atoms.add("__" + Integer.toString(i));
        return parse(puzzle, new Universe(atoms));
    }
	
	public static final TupleSet parse(String puzzle) { 
		return parse(split(puzzle));
	}
	
	public static final TupleSet parse(String puzzle, Universe univ) { 
		return parse(split(puzzle), univ);
	}
	
	public static final String toString(TupleSet puzzle) { 
		final StringBuilder str = new StringBuilder();
		final int n = puzzle.universe().size();
		final String sep = (n>9) ? " " : "";
		Iterator<Tuple> itr = puzzle.iterator();
		if (!itr.hasNext()) { 
			str.append(0);
			for(int i = 1, max = n*n; i < max; i++) { 
				str.append(sep+0);
			}
			return str.toString();
		}
		
		int last = 0;
		Tuple tuple = itr.next();
		if ((Integer)tuple.atom(0)==1 && (Integer)tuple.atom(1)==1) { 
			str.append(tuple.atom(2));
		} else {
			str.append(0);
			itr = puzzle.iterator();
		}
		
		while(itr.hasNext()) { 
			tuple = itr.next();
			final int current = n*((Integer)tuple.atom(0)-1) + ((Integer)tuple.atom(1)-1);
			for(int i = last+1; i < current; i++) { 
				str.append(sep+0);
			}
			str.append(sep+tuple.atom(2));
			last = current;
		}

		for(int i = last+1, max = n*n; i < max; i++) { 
			str.append(sep+0);
		}
		return str.toString();
	}
	
	private static void appendDivider(StringBuilder str, int r) { 
		final String len = (r<=3) ? "--" : "---";
		for(int i = 0; i < r; i++) { 
			str.append("+-");
			for(int j = 0; j < r; j++) { 
				str.append(len);
			}
		}
		str.append("+\n");
	}
	
	public static final String prettyPrint(TupleSet solution) { 
		final StringBuilder str = new StringBuilder();
		final int n = 9;
		final int r = (int)Math.sqrt(n);
		appendDivider(str, r);
		final Iterator<Tuple> psol = solution.iterator();
		for(int i = 1; i <= n; i++) {
			str.append("| ");
			for(int j = 0; j < r; j++) {
				for(int k = 0; k < r; k++) {
					final int atom = (Integer)psol.next().atom(2);
					if (atom<10&&r>3) str.append(" ");
					str.append(atom);
					str.append(" ");
				}
				str.append("| ");
			}
			str.append("\n");
			if (i%r==0)	appendDivider(str, r);		
		}
		return str.toString();
	}
	
	static Map<String, String> options(String[] args) { 
		final Map<String,String> opts = new LinkedHashMap<String,String>();
		for(String arg: args) { 
			if (arg.startsWith("-")) { 
				String[] opt = arg.split("=");
				switch(opt.length) { 
				case 1 : opts.put(opt[0], null); break;
				case 2 : opts.put(opt[0], opt[1]); break;
				default : throw new IllegalArgumentException("Unrecognized option format: " + arg);
				}
			}
		}
		return opts;
	}
	
}
