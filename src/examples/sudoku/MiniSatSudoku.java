package examples.sudoku;

import java.io.IOException;
import java.io.PrintStream;

import kodkod.instance.Tuple;
import kodkod.instance.TupleSet;

/**
 * Implemented based on the paper by Ines Lynce et al, "Sudoku as SAT Problem". 
 * A sudoku solver that works
 * by calling MinSat with a file containing
 * a sudoku encoding.
 * @author Emina Torlak
 */
final class MiniSatSudoku extends ExternalSolver {
	/**
	 * Constructs a new MiniSat solver, assuming minisat solver is in the specified directory.
	 */
	MiniSatSudoku(String path)  throws IOException { 
		super(path+"minisat", ".cnf"); 
	}
	/**
	 * {@inheritDoc}
	 * @see examples.sudoku.ExternalSolver#printSudoku(kodkod.instance.TupleSet, java.io.PrintStream)
	 */
	@Override
	void printSudoku(TupleSet puzzle, PrintStream stream) {
		final int n = puzzle.universe().size();
		final int r = (int)Math.sqrt(n);
		final int nsq = n * n;
		final int atLeastOne = 4 * nsq;
		final int atMostOne = atLeastOne * ((n*(n-1))/2);
		stream.println("p cnf " + puzzle.capacity() + " " + (atLeastOne + atMostOne +puzzle.size()));
		
		// there is at least one number in each entry:
		// /\_{x=[1..n]} /\_{y=[1..n]} \/_{z=[1..n]} s_{x,y,z}
		
		for(int x = 0; x < n; x++) { 
			for(int y = 0; y < n; y++) { 
				for(int z = 0; z < n; z++) { 
					stream.print((x*nsq+y*n+z+1) + " ");
				}
				stream.println("0");
			}
		}
		
		// each number appears at least once in each column
		// /\_{y=[1..n]} /\_{z=[1..n]} \/_{x=[1..n]} s_{x,y,z}
		
		for(int y = 0; y < n; y++) { 
			for(int z = 0; z < n; z++) { 
				for(int x = 0; x < n; x++) { 
					stream.print((x*nsq+y*n+z+1) + " ");
				}
				stream.println("0");
			}
		}
		
		// each number appears at least once in each row
		// /\_{x=[1..n]} /\_{z=[1..n]} \/_{y=[1..n]} s_{x,y,z}
		
		for(int x = 0; x < n; x++) { 
			for(int z = 0; z < n; z++) { 
				for(int y = 0; y < n; y++) { 
					stream.print((x*nsq+y*n+z+1) + " ");
				}
				stream.println("0");
			}
		}
		
		// each number appears at least once in each rxr region
		// /\_{z=[1..n]} /\_{i=[0..r)} /\_{j=[0..r)} \/_{x=[1..r]} \/_{y=[1..r]}  s_{r*i+x, r*j+y, z}
		for(int z = 0; z < n; z++) { 
			for(int i = 0; i < r; i++) { 
				for(int j = 0; j < r; j++) { 
					for(int x = 0; x < r; x++) { 
						for(int y = 0; y < r; y++) { 
							stream.print((r*nsq*i+x*nsq+r*n*j+y*n+z+1) + " ");
						}
					}
					stream.println("0");
				}
			}
		}
		
		// each number appears at most once in each entry
		// /\_{x=[1..n]} /\_{y=[1..n]} /\_{z=[1..n)} /\_{i=(z..n]} !s_{x,y,z} \/ !s_{x,y,i}
		
		for(int x = 0; x < n; x++) { 
			for(int y = 0; y < n; y++) { 
				for(int z = 0; z < n-1; z++) {
					for(int i = z+1; i < n; i++) { 
						stream.println(-(x*nsq+y*n+z+1) + " "+ (-(x*nsq+y*n+i+1)) + " 0");
					}
				}
			}
		}
		
		// each number appears at most once in each row
		// /\_{x=[1..n]} /\_{z=[1..n]} /\_{y=[1..n)} /\_{i=(y..n]} !s_{x,y,z} \/ !s_{x,i,z}
		
		for(int x = 0; x < n; x++) { 
			for(int z = 0; z < n; z++) { 
				for(int y = 0; y < n-1; y++) {
					for(int i = y+1; i < n; i++) { 
						stream.println(-(x*nsq+y*n+z+1) + " "+ (-(x*nsq+i*n+z+1)) + " 0");
					}
				}
			}
		}
		
		// each number appears at most once in each column
		// /\_{y=[1..n]} /\_{z=[1..n]} /\_{x=[1..n)} /\_{i=(x..n]} !s_{x,y,z} \/ !s_{i,y,z}
		
		for(int y = 0; y < n; y++) { 
			for(int z = 0; z < n; z++) { 
				for(int x = 0; x < n-1; x++) {
					for(int i = x+1; i < n; i++) { 
						stream.println(-(x*nsq+y*n+z+1) + " "+ (-(i*nsq+y*n+z+1)) + " 0");
					}
				}
			}
		}
		
		// each number appears at most once in each 3x3 region
		// /\_{z=[1..n]} /\_{i=[0..r)} /\_{j=[0..r)} /\_{x=[1..r]} /\_{y=[1..r]} /\_{k=(y..r]} !s_{r*i+x, r*j+y, z} \/ !s_{r*i+x, r*j+k, z}  
		// /\_{z=[1..n]} /\_{i=[0..r)} /\_{j=[0..r)} /\_{x=[1..r]} /\_{y=[1..r]} /\_{k=(x..r]} /\_{l=[1..r]} !s_{r*i+x, r*j+y, z} \/ !s_{r*i+k, r*j+l, z}  
		
		final int[] tmp = new int[n];
		for(int z = 0; z < n; z++) { 
			for(int i = 0; i < r; i++) { 
				for(int j = 0; j < r; j++) { 
					for(int x = 0; x < r; x++) { 
						for(int y = 0; y < r; y++) { 
							tmp[x*r+y] = -(r*nsq*i+x*nsq+r*n*j+y*n+z+1);
						}
					}
					for(int k = 0; k < n-1; k++) { 
						for(int l = k+1; l < n; l++) { 
							stream.println(tmp[k] + " " + tmp[l] + " 0");
						}
					}
				}
			}
		}
		
		// unit clauses
		for(Tuple t : puzzle) { 
			stream.println((t.index()+1) + " 0");
		}
	}
	
}