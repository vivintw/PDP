

import java.util.ArrayList;
import java.util.List;

//This class defines a main method for testing the Competitor1 class,
//which implements the Competitor interface.

public class TestClass {
    public static void main(String[] args) {

        List<Outcome> oclst;
        List<String> output;

        Competitor1 cA = new Competitor1("A");
        Competitor1 cB = new Competitor1("B");
        Competitor1 cC = new Competitor1("C");
        Competitor1 cD = new Competitor1("D");
        Competitor1 cE = new Competitor1("E");
        Competitor1 cF = new Competitor1("F");
        Competitor1 cH = new Competitor1("H");
        Competitor1 cI = new Competitor1("I");
        Competitor1 cG = new Competitor1("G");
        Competitor1 cK = new Competitor1("K");
        Competitor1 cL = new Competitor1("L");
        Competitor1 cM = new Competitor1("M");
        Competitor1 cJ = new Competitor1("J");
        Competitor1 cN = new Competitor1("N");
        Competitor1 cP = new Competitor1("P");
        Competitor1 cQ = new Competitor1("Q");
        Competitor1 cR = new Competitor1("R");
        Competitor1 cS = new Competitor1("S");
        Competitor1 cT = new Competitor1("T");
        Competitor1 cU = new Competitor1("U");
        Competitor1 cV = new Competitor1("V");
        Competitor1 cW = new Competitor1("W");
        Competitor1 cX = new Competitor1("X");
        Competitor1 cY = new Competitor1("Y");
        Competitor1 cZ = new Competitor1("Z");
        Competitor1 cO = new Competitor1("O");

        //////////////////////// test for Tie///////////////////////////////
        checkTrue((new Tie1(cA, cB)) instanceof Tie1, "must be of type Tie");
        //////////////////////// test for Defeat///////////////////////////////
        checkTrue((new Defeat1(cA, cB)) instanceof Defeat1,
                "must be of type Defeat");

        //////////////////////// test for hasDefeated()////////////////////////
        oclst = new ArrayList<Outcome>();
        oclst.add(new Tie1(cA, cB));
        oclst.add(new Defeat1(cB, cA));
        oclst.add(new Tie1(cB, cC));
        checkTrue(cA.hasDefeated(cB, oclst),
                "Should return True since there is tie between A and B");

        oclst = new ArrayList<Outcome>();
        oclst.add(new Defeat1(cA, cB));
        oclst.add(new Tie1(cB, cC));

        checkTrue(cA.hasDefeated(cB, oclst),
                "must return true as A defeated B");
        checkFalse(cA.hasDefeated(cC, oclst), "must return false");
        checkFalse(cB.hasDefeated(cA, oclst), "must return false");
        checkTrue(cB.hasDefeated(cC, oclst), "must return true");
        checkTrue(cC.hasDefeated(cB, oclst), "must return true");
        checkFalse(cA.hasDefeated(cB, new ArrayList<Outcome>()),
                "must return false");

        /////////////////////// test for outranks()////////////////////////////
        output = new ArrayList<String>();
        output.add("B");
        output.add("C");

        checkTrue(cA.outranks(oclst).equals(output), "should return true");
        checkTrue(cC.outranks(oclst).equals(output), "should return true");

        oclst = new ArrayList<Outcome>();
        oclst.add(new Tie1(cB, cC));
        oclst.add(new Defeat1(cA, cB));

        checkTrue(cA.outranks(oclst).equals(output), "should return true");

        oclst = new ArrayList<Outcome>();
        oclst.add(new Defeat1(cA, cB));
        oclst.add(new Tie1(cB, cC));
        oclst.add(new Defeat1(cC, cA));

        output = new ArrayList<String>();
        output.add("A");
        output.add("B");
        output.add("C");

        checkTrue(cA.outranks(oclst).equals(output), "should return true");

        oclst = new ArrayList<Outcome>();
        oclst.add(new Defeat1(cA, cB));
        oclst.add(new Defeat1(cB, cA));

        output = new ArrayList<String>();
        output.add("A");
        output.add("B");

        checkTrue(cB.outranks(oclst).equals(output), "should return true");

        oclst = new ArrayList<Outcome>();
        oclst.add(new Defeat1(cC, cD));
        oclst.add(new Tie1(cE, cF));

        checkTrue(cA.outranks(oclst).equals(new ArrayList<String>()),
                "should return empty");

        /////////////////// test for outrankedBy() //////////////////
        oclst = new ArrayList<Outcome>();
        oclst.add(new Defeat1(cA, cB));
        oclst.add(new Tie1(cB, cC));

        checkTrue(cA.outrankedBy(oclst).equals(new ArrayList<String>()),
                "should return empty");

        output = new ArrayList<String>();
        output.add("A");
        output.add("B");
        output.add("C");

        checkTrue(cC.outrankedBy(oclst).equals(output), "should return true");

        oclst = new ArrayList<Outcome>();
        oclst.add(new Defeat1(cA, cB));
        oclst.add(new Defeat1(cB, cA));

        output = new ArrayList<String>();
        output.add("A");
        output.add("B");

        checkTrue(cB.outrankedBy(oclst).equals(output), "should return true");

        /////////////////// test for powerRanking() //////////////////
        oclst = new ArrayList<Outcome>();
        oclst.add(new Defeat1(cA, cD));
        oclst.add(new Defeat1(cA, cE));
        oclst.add(new Defeat1(cC, cB));
        oclst.add(new Defeat1(cC, cF));
        oclst.add(new Tie1(cD, cB));
        oclst.add(new Defeat1(cF, cE));

        output = new ArrayList<String>();
        output.add("C");
        output.add("A");
        output.add("F");
        output.add("E");
        output.add("B");
        output.add("D");

        checkTrue(cA.powerRanking(oclst).equals(output), "C outranks 4");

        oclst = new ArrayList<Outcome>();
        oclst.add(new Defeat1(cA, cB));
        oclst.add(new Defeat1(cB, cC));
        oclst.add(new Defeat1(cC, cD));
        oclst.add(new Tie1(cD, cE));
        oclst.add(new Defeat1(cE, cH));
        oclst.add(new Tie1(cF, cI));
        oclst.add(new Tie1(cG, cK));
        oclst.add(new Defeat1(cH, cL));
        oclst.add(new Defeat1(cI, cM));
        oclst.add(new Tie1(cJ, cN));
        oclst.add(new Tie1(cP, cB));
        oclst.add(new Tie1(cC, cE));
        oclst.add(new Defeat1(cJ, cP));
        oclst.add(new Tie1(cQ, cP));
        oclst.add(new Defeat1(cR, cK));
        oclst.add(new Tie1(cS, cL));
        oclst.add(new Defeat1(cT, cA));
        oclst.add(new Defeat1(cU, cB));
        oclst.add(new Defeat1(cV, cE));
        oclst.add(new Defeat1(cW, cP));
        oclst.add(new Tie1(cX, cB));
        oclst.add(new Defeat1(cY, cE));
        oclst.add(new Defeat1(cZ, cP));

        output = new ArrayList<String>();
        output.add("T");
        output.add("U");
        output.add("W");
        output.add("Z");
        output.add("V");
        output.add("Y");
        output.add("R");
        output.add("A");
        output.add("J");
        output.add("N");
        output.add("F");
        output.add("I");
        output.add("M");
        output.add("G");
        output.add("K");
        output.add("Q");
        output.add("X");
        output.add("B");
        output.add("P");
        output.add("C");
        output.add("E");
        output.add("D");
        output.add("H");
        output.add("S");
        output.add("L");

        checkTrue(cA.powerRanking(oclst).equals(output),
                "There exists multiple cyclic paths between the competitors");

        oclst = new ArrayList<Outcome>();
        oclst.add(new Defeat1(cA, cB));
        oclst.add(new Defeat1(cB, cC));
        oclst.add(new Defeat1(cC, cD));
        oclst.add(new Defeat1(cD, cE));
        oclst.add(new Tie1(cC, cE));

        output = new ArrayList<String>();
        output.add("A");
        output.add("B");
        output.add("C");
        output.add("D");
        output.add("E");

        checkTrue(cA.powerRanking(oclst).equals(output),
                "Alphabetical order gets the precedence");

        oclst = new ArrayList<Outcome>();
        oclst.add(new Tie1(cA, cB));
        oclst.add(new Tie1(cB, cC));
        oclst.add(new Tie1(cC, cD));
        oclst.add(new Tie1(cD, cE));
        oclst.add(new Tie1(cE, cH));
        oclst.add(new Tie1(cF, cI));
        oclst.add(new Tie1(cG, cK));
        oclst.add(new Tie1(cH, cL));
        oclst.add(new Tie1(cI, cM));
        oclst.add(new Tie1(cJ, cN));
        oclst.add(new Tie1(cK, cO));
        oclst.add(new Tie1(cL, cP));
        oclst.add(new Tie1(cM, cK));
        oclst.add(new Tie1(cN, cL));
        oclst.add(new Defeat1(cO, cA));
        oclst.add(new Tie1(cP, cB));
        oclst.add(new Tie1(cC, cE));
        oclst.add(new Tie1(cJ, cP));

        output = new ArrayList<String>();
        output.add("F");
        output.add("G");
        output.add("I");
        output.add("K");
        output.add("M");
        output.add("O");
        output.add("B");
        output.add("C");
        output.add("D");
        output.add("E");
        output.add("H");
        output.add("J");
        output.add("L");
        output.add("N");
        output.add("P");
        output.add("A");

        checkTrue(cA.powerRanking(oclst).equals(output),
                "There exists multiple cyclic paths between the competitors");

        summarize();

    }
    ////////////////////////////////////////////////////////////////

    private static int testsPassed = 0;
    private static int testsFailed = 0;

    private static final String FAILED = "    TEST FAILED: ";

    static void checkTrue(boolean result) {
        checkTrue(result, "anonymous");
    }

    // GIVEN : a Boolean result and a String name
    // EFFECT : prints failed message if result is false
    static void checkTrue(boolean result, String name) {
        if (result)
            testsPassed = testsPassed + 1;
        else {
            testsFailed = testsFailed + 1;
            System.err.println(FAILED + name);
        }
    }

    // GIVEN : a Boolean result
    // EFFECT : prints failed message if result is false
    static void checkFalse(boolean result) {
        checkFalse(result, "anonymous");
    }

    // GIVEN : a Boolean result
    // EFFECT : prints a failed message if the result is true.
    static void checkFalse(boolean result, String name) {
        checkTrue(!result, name);
    }

    // EFFECT : prints a summary with the number of failures and passed
    // tests.
    static void summarize() {
        System.err.println("Passed " + testsPassed + " tests");
        if (testsFailed > 0) {
            System.err.println("Failed " + testsFailed + " tests");
        }
    }
}
