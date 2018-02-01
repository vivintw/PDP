
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;

public class TestClass {
    public static void main(String args[]) {
        Player A = Players.make("A");
        Player A1 = Players.make("A");
        Player B = Players.make("B");
        Player C = Players.make("C");
        Player D = Players.make("D");

        // Tests for Player
        checkTrue(A.name().equals("A"),
                "the name of the player should be \"A\"");
        checkTrue(A.available(), "new player created should be available");
        checkFalse(A.isInjured(), "a new player is not injured");
        checkFalse(A.isSuspended(), "a new player is not suspended");
        checkTrue(A.underContract(), "a new player is always under contract.");

        A.changeInjuryStatus(true);
        checkFalse(A.available(), "injured player should not be available");
        checkTrue(A.isInjured(), "injured player should be injured");
        A.changeInjuryStatus(false);

        A.changeContractStatus(false);
        checkFalse(A.available(), "player not under contract is not available");
        checkFalse(A.underContract(),
                "player not under contract must return false");
        A.changeContractStatus(true);

        A.changeSuspendedStatus(true);
        checkFalse(A.available(), "suspended player is not available");
        checkTrue(A.isSuspended(), "true as the player is suspended");
        A.changeSuspendedStatus(false);

        checkFalse(A.equals(A1), "two diffrent players can have the same name");
        checkTrue(A.equals(A), "checking for the same player");

        checkTrue(A.hashCode() == A.hashCode(),
                "the hashcode of the same player must be same");
        checkFalse(A.hashCode() == A1.hashCode(),
                "the hashcode different players must be unique");

        checkTrue(A.toString() == "A",
                "the name of the player is returned by the toString() method");
        checkFalse(A.equals(null),
                "making sure that a non player object returns false");

        // Tests for Roster

        Roster r = Rosters.empty();
        Roster r1 = r.with(A);

        checkTrue(r.equals(r),
                "the roster must be the same as the initial roster");
        checkFalse(r.equals(r1), "r1 must be a new roster, and not equal to r");

        checkTrue(r.with(A).with(A).equals(r.with(A)),
                "duplicate players do not change the roster");
        checkTrue(r.with(A).with(A).hashCode() == r.with(A).hashCode(),
                "equavalent rosters will have the same hashcode");

        checkTrue(r.with(A).has(A), "must return true");
        checkFalse(r.has(A), "must return true");
        checkFalse(r.with(A1).has(A), "must return true");

        checkTrue(r.with(A).with(A).size() == 1, "should have a size of 1");
        checkTrue(r.with(A).with(A1).size() == 2, "should have a size of 2");

        checkTrue(r.readyCount() == 0, "no players in the roster");
        checkTrue(r.with(A).with(B).readyCount() == 2,
                "2 available player in the roster");
        checkTrue(r.with(A).with(B).readyRoster().equals(r.with(A).with(B)),
                "2 available player in the roster");
        A.changeInjuryStatus(true);
        checkTrue(r.with(A).with(B).readyCount() == 1,
                "1 available player in the roster");
        checkTrue(r.with(A).with(B).readyRoster().equals(r.with(B)),
                "1 available player in the roster");
        checkFalse(r.with(A).with(B).readyRoster().equals(r.with(A).with(B)),
                "1 available player in the roster");
        A.changeInjuryStatus(false);

        checkTrue(r1.toString().equals("[A]"), "make sure that toString works");
        Roster r2 = Rosters.empty().with(A).with(B);
        Roster r3 = Rosters.empty().with(A).with(C);
        checkFalse(r2.equals(r3),
                "similar length rosters with different players.");

        checkTrue(r2.without(B).equals(r1),
                "checking functionality of without");

        Roster r4 = Rosters.empty().with(C).with(A).with(B).with(D);
        ArrayList<String> names = new ArrayList<String>(
                Arrays.asList(new String[] { "A", "B", "C", "D" }));
        Iterator<String> n = names.iterator();
        for (Player p : r4) {
            checkTrue(p.toString().equals(n.next()), "the order must match");
        }
        

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
