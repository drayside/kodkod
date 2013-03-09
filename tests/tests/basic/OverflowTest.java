package tests.basic;

import java.util.ArrayList;
import java.util.List;

import junit.framework.Assert;
import junit.framework.TestCase;
import kodkod.ast.Expression;
import kodkod.ast.Formula;
import kodkod.ast.IntConstant;
import kodkod.ast.IntExpression;
import kodkod.ast.Relation;
import kodkod.ast.Variable;
import kodkod.engine.Evaluator;
import kodkod.engine.Solution;
import kodkod.engine.Solver;
import kodkod.engine.satlab.SATFactory;
import kodkod.instance.Bounds;
import kodkod.instance.Instance;
import kodkod.instance.TupleFactory;
import kodkod.instance.Universe;
import kodkod.util.nodes.PrettyPrinter;

import org.junit.Test;

public class OverflowTest extends TestCase {

    private static final int bw = 5;
    private final TupleFactory factory;
    private final Solver solver;
    private final Relation ret;
    private Bounds bounds;
    
    private static final int DEF_VAL = -1;
    
    @SuppressWarnings("serial")
    public class NoSol extends RuntimeException {}
    
    public abstract class Tester {
        public abstract void doTest(int i, int j);
    }
    
    public abstract class GenericTester extends Tester {
        @Override
        public void doTest(int i, int j) {
            doTestNoDef(i, j);
            doTestDef(i, j);
        }
        
        protected void doTestDef(int i, int j) {
            int min = min();
            int max = max();
            String op = getOp();
            int res; 
            boolean of = false;
            try {
                res = exeJava(i, j);
                if (res > max || res < min) {
                    of = true;
                    res = DEF_VAL;
                }
            } catch (Exception e) {
                of = true;
                res = DEF_VAL;
            }
            try {
                int x = exeKodkodDef(i, j);
                String msg = of
                        ? String.format("Expected the default value (%d) due to overflow ((%d) %s (%d)) but got %d", 
                                DEF_VAL, i, op, j, x)
                        : String.format("Wrong result: %s %s %s != %s", i, op, j, x);                    
                Assert.assertEquals(msg, res, x);
            } catch (Exception e) {
                String msg = String.format("Expected the default value (%d) due to overflow (%d %s %d) but got exception: %s",
                        DEF_VAL, i, op, j, e.getClass().getSimpleName() + ": " + e.getMessage());
                Assert.fail(msg);
            }
        }

        protected void doTestNoDef(int i, int j) {
            if (skip(i, j))
                return;
            int min = min();
            int max = max();
            String op = getOp();
            int res = exeJava(i, j); 
            if (res > max || res < min) {
                try {
                    int x = exeKodkod(i, j);
                    Assert.fail(String.format("Overflow not detected: (%s) %s (%s) != %s", i, op, j, x));
                } catch (Exception e) {}
            } else {
                try {
                    int x = exeKodkod(i, j);
                    Assert.assertEquals(String.format("Wrong result: (%s) %s (%s) != %s", i, op, j, x), res, x);
                } catch (NoSol e) {
                    Assert.fail(String.format("No solution for (%s) %s (%s); expected: %s", i, op, j, res));
                }
            }
        }
        
        protected int eval(Instance instance) {
            if (instance == null)
                throw new NoSol();
            Evaluator ev = new Evaluator(instance);
            return Integer.parseInt(ev.evaluate(ret).iterator().next().atom(0).toString());
        }
        
        protected int exeKodkod(int i, int j) {
            Formula f = ret.eq(kodkodOpExpr(IntConstant.constant(i), IntConstant.constant(j)).toExpression());
            Solution sol = solve(f);
            return eval(sol.instance());
        }

        protected int exeKodkodDef(int i, int j) {
            IntExpression kkIntExpr = kodkodOpExpr(IntConstant.constant(i), IntConstant.constant(j));
            Expression kkExpr = kkIntExpr.toExpression();
            Formula f = ret.eq(kkExpr.some().thenElse(kkIntExpr, IntConstant.constant(DEF_VAL)).toExpression());
            Solution sol = solve(f);
            return eval(sol.instance());
        }
        
        protected abstract int exeJava(int i, int j);
        protected abstract IntExpression kodkodOpExpr(IntConstant i, IntConstant j);        
        protected abstract String getOp();
        protected boolean skip(int i, int j) { return false; }
    }
        
    public OverflowTest(String arg0) {
        super(arg0);
        this.solver = new Solver();
        this.ret = Relation.unary("ret");
        int min = min();
        int max = max();
        List<String> atoms = new ArrayList<String>(max - min + 1);
        for (int i = min; i <= max; i++) {
            atoms.add(String.valueOf(i));
        }
        final Universe universe = new Universe(atoms);
        this.factory = universe.factory();
        this.bounds = new Bounds(factory.universe());
        for (int i = min; i <= max; i++) {
            bounds.boundExactly(i, factory.setOf(String.valueOf(i)));
        }
        bounds.bound(ret, factory.noneOf(1), factory.allOf(1));
    }

    protected void setUp() throws Exception {
        super.setUp();
        solver.options().setNoOverflow(true);
        solver.options().setBitwidth(bw);
        solver.options().setSolver(SATFactory.MiniSat);
    }

    protected void tearDown() throws Exception {
        super.tearDown();
    }
    
    protected Solution solve(Formula formula) {
        return solver.solve(formula, bounds);        
    }
        
    @Test
    public void testPlus() {
        runTestForAll(new GenericTester() {
            @Override protected int exeJava(int i, int j) { return i + j; }
            @Override protected IntExpression kodkodOpExpr(IntConstant i, IntConstant j) { return i.plus(j); }
            @Override protected String getOp() { return "+"; }            
        });    
    }

    @Test
    public void testMinus() {
        runTestForAll(new GenericTester() {
            @Override protected int exeJava(int i, int j) { return i - j; }
            @Override protected IntExpression kodkodOpExpr(IntConstant i, IntConstant j) { return i.minus(j); }
            @Override protected String getOp() { return "-"; }            
        });    
    }
    
    @Test
    public void testTimes() {
        runTestForAll(new GenericTester() {
            @Override protected int exeJava(int i, int j) { return i * j; }
            @Override protected IntExpression kodkodOpExpr(IntConstant i, IntConstant j) { return i.multiply(j); }
            @Override protected String getOp() { return "*"; }            
        });    
    }
    
    @Test
    public void testDiv() {
        runTestForAll(new GenericTester() {
            @Override protected int exeJava(int i, int j) { return i / j; }
            @Override protected IntExpression kodkodOpExpr(IntConstant i, IntConstant j) { return i.divide(j); }
            @Override protected String getOp() { return "/"; }   
            @Override protected boolean skip(int i, int j) { return j == 0; }
        });    
    }
    
    @Test
    public void testMod() {
        runTestForAll(new GenericTester() {
            @Override protected int exeJava(int i, int j) { return i % j; }
            @Override protected IntExpression kodkodOpExpr(IntConstant i, IntConstant j) { return i.modulo(j); }
            @Override protected String getOp() { return "%"; }   
            @Override protected boolean skip(int i, int j) { return j == 0; }
        });    
    }
        
    /**
     * <code>check { all s : set univ | (some s) iff #s > 0 }</code>
     */
    @Test
    public void testCardinality() {
        Variable s = Variable.unary("s");        
        Formula f = s.some().iff(s.count().gt(IntConstant.constant(0))).forAll(s.setOf(Expression.UNIV));
        System.out.println(PrettyPrinter.print(f, 0));
        checkTrue(f);
    }

    protected void runTestForAll(Tester t) {
        int min = min();
        int max = max();
        for (int i = min; i <= max; i++) {
            for (int j = min; j <= max; j++) {
                t.doTest(i, j);
            }
        }
    }
    
    protected int min() { return -(1 << (bw - 1)); }
    protected int max() { return (1 << (bw - 1)) - 1; }
    
    protected void checkTrue(Formula f)  { assertNoInstance(solve(f.not()));}
    protected void checkFalse(Formula f) { assertInstance(solve(f.not())); }
    
    protected void assertInstance(Solution sol) {
        assertNotNull("expected sat, actual " + sol.outcome(), sol.instance());
    }
    
    protected void assertNoInstance(Solution sol) {
        assertNull("expected unsat, actual " + sol.outcome(), sol.instance());
    }
}
