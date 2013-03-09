package examples;

import java.util.LinkedList;
import java.util.List;

import kodkod.ast.Formula;
import kodkod.ast.IntConstant;
import kodkod.ast.Relation;
import kodkod.ast.Variable;
import kodkod.engine.Evaluator;
import kodkod.engine.Solution;
import kodkod.engine.Solver;
import kodkod.instance.Bounds;
import kodkod.instance.Tuple;
import kodkod.instance.TupleFactory;
import kodkod.instance.TupleSet;
import kodkod.instance.Universe;

public class KodkodEx {


    private Relation Node;
    private Relation Null;
    private Relation next;
    private Relation size;

    private void go() {
        createRelations();
        Bounds b = getBounds();
        Formula f = getFormula();
        System.out.println(f);
        Solver s = new Solver();
        Solution sol = s.solve(f, b);
        System.out.println(sol);
    }
    
    private void createRelations() {
        Node = Relation.unary("Node");
        Null = Relation.unary("Null");
        
        next = Relation.binary("next");
        size = Relation.binary("size");
    }
    
    private Universe getUniverse() {
        List<Object> atoms = new LinkedList<Object>();
        atoms.add("Node1");
        atoms.add("Node2");
        atoms.add("Node3");
        atoms.add("Null");
        atoms.add(0);
        atoms.add(1);
        atoms.add(2);
        atoms.add(3);
        return new Universe(atoms);
    }
    
    private Bounds getBounds() {
        Universe u = getUniverse();
        TupleFactory f = u.factory();
        Bounds b = new Bounds(u); 
        b.boundExactly(Node, f.setOf("Node1", "Node2", "Node3"));
        b.boundExactly(Null, f.setOf("Null"));
        
        List<Tuple> tuples = new LinkedList<Tuple>();
        tuples.add(f.tuple("Node1", "Node2"));
        tuples.add(f.tuple("Node2", "Node3"));
        tuples.add(f.tuple("Node3", "Null"));
        b.boundExactly(next, f.setOf(tuples));
        
        List<Tuple> nodes = new LinkedList<Tuple>();
        nodes.add(f.tuple("Node1"));
        nodes.add(f.tuple("Node2"));
        nodes.add(f.tuple("Node3"));
        TupleSet setOfNodes = f.setOf(nodes);
        TupleSet ints = f.range(f.tuple(0), f.tuple(3));
        TupleSet sizeUpper = setOfNodes.product(ints);
        b.bound(size, f.noneOf(2), sizeUpper);
        
        b.boundExactly(0, f.setOf(0));
        b.boundExactly(1, f.setOf(1));
        b.boundExactly(2, f.setOf(2));
        b.boundExactly(3, f.setOf(3));
        
        return b;
    }

    private Formula getFormula() {
        Variable n = Variable.nary("n", 1);
        Formula cond = n.join(next).eq(Null);
        return cond.implies(n.join(size).sum().eq(IntConstant.constant(1)))
            .and(cond.not().implies(n.join(size).sum().eq(IntConstant.constant(1).plus(n.join(next).join(size).sum()))))
            .forAll(n.oneOf(Node));
    }

    public static void main(String[] args) {
        new KodkodEx().go();
    }
}
