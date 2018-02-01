
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collector;
import java.util.stream.Collectors;

public class TestRosterWithStream {
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

        checkFalse(A.equals(null), "null is not a player object");
        checkFalse(A.equals(new Object()), "object is not a player");

        checkTrue(A.hashCode() == A.hashCode(),
                "the hashcode of the same player must be same");
        checkFalse(A.hashCode() == A1.hashCode(),
                "the hashcode different players must be unique");

        checkTrue(A.toString() == "A",
                "the name of the player is returned by the toString() method");
        checkFalse(A.equals(null),
                "making sure that a non player object returns false");

        // Tests for Roster

        RosterWithStream r = RosterWithStreams.empty();
        RosterWithStream r1 = r.with(A);

        checkFalse(r.equals(null), "null is not a RosterWithStream object");
        checkFalse(r.equals(new Object()), "object is not a RosterWithStream");

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
        RosterWithStream r2 = RosterWithStreams.empty().with(A).with(B);
        RosterWithStream r3 = RosterWithStreams.empty().with(A).with(C);
        checkFalse(r2.equals(r3),
                "similar length rosters with different players.");

        checkTrue(r2.without(B).equals(r1),
                "checking functionality of without");

        RosterWithStream r4 = RosterWithStreams.empty().with(C).with(A).with(B)
                .with(D);
        ArrayList<String> names = new ArrayList<String>(
                Arrays.asList(new String[] { "A", "B", "C", "D" }));
        Iterator<String> n = names.iterator();
        for (Player p : r4) {
            checkTrue(p.toString().equals(n.next()), "the order must match");
        }
        checkTrue(r4.stream().allMatch(p -> p.available()),
                "all players must be available initially");

        A.changeInjuryStatus(true);

        checkFalse(r4.stream().allMatch(p -> p.available()),
                "player A is not available");

        checkTrue(r4.stream().anyMatch(p -> p.available()),
                "all players other than A are available");

        List<Player> expectedList = new ArrayList<Player>(
                Arrays.asList(new Player[] { B, C, D }));
        List<Player> output = r4.stream().filter(p -> p.available())
                .collect(Collectors.toList());

        checkTrue(output.equals(expectedList), "Player A must be filtered out");

        checkTrue(
                RosterWithStreams.empty().stream().findAny()
                        .equals(Optional.empty()),
                "no players to find in am empty roster");

        checkTrue(RosterWithStreams.empty().with(A).stream().findAny()
                .equals(Optional.of(A)), "only one player A to find.");

        checkTrue(r4.stream().findFirst().equals(Optional.of(A)),
                "first player must be A in roster r4");

        List<Player> allPlayersR4 = new ArrayList<Player>(
                Arrays.asList(new Player[] { A, B, C, D }));
        r4.stream().forEach(p -> checkTrue(allPlayersR4.contains(p),
                "p must be one of allPlayersR4"));

        checkFalse(r4.stream().map(p -> p.available()).reduce(true,
                (p1, p2) -> p1 && p2), "player A is not available");

        checkTrue(
                r4.stream().map(p -> p.available()).reduce(false,
                        (p1, p2) -> p1 || p2),
                "only player A is not available");

        Player first = r4.stream().findFirst().get();
        RosterWithStream rest = RosterWithStreams.empty();
        List<Player> lst = r4.stream().skip(1).collect(Collectors.toList());
        for (Player p : lst) {
            rest = rest.with(p);
        }

        checkTrue(
                rest.stream()
                        .reduce(first,
                                (p1, p2) -> Players.make(p1.name() + p2.name()))
                        .name().equals("ABCD"),
                "reduce all players of the roster into a single player");

        checkTrue(r4.stream().skip(3).collect(Collectors.toList()).get(0)
                .equals(D), "last player in r4 = D");
        checkTrue(r4.stream().skip(4).collect(Collectors.toList()).isEmpty(),
                "all players of r4 skipped.");
        checkTrue(r4.stream().skip(5).collect(Collectors.toList()).isEmpty(),
                "skipping more than the available players");

        Object[] players = r4.stream().toArray();
        for (Object p : players) {
            checkTrue(allPlayersR4.contains(p),
                    "p must be one of allPlayersR4");
        }

        B.changeInjuryStatus(true);
        C.changeInjuryStatus(true);
        D.changeInjuryStatus(true);

        checkFalse(r4.stream().anyMatch(p -> p.available()),
                "none of the players are available");
        checkTrue(r4.stream().count() == 4,
                "the number of players in this roster are 4");
        RosterWithStream r5 = RosterWithStreams.empty().with(A).with(A1).with(B)
                .with(C).with(A);
        checkTrue(
                r5.stream().distinct().collect(Collectors.toList()).toString()
                        .equals(r5.toString()),
                "should contain the same players in order");

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
