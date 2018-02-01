
//If r1 and r2 are rosters of different sizes, then
//r1.toString() is not the same string as r2.toString().
//
//Rosters.empty() is a static factory method that returns an
//empty roster.

import java.util.ArrayList;
import java.util.List;

public class Rosters {

    // RETURNS : an empty Roster
    // EXAMPLES :
    // empty().has(p) => false
    // DESIGN STRATEGY : call the default constructor of Roster1
    public static Roster empty() {
        return new Roster1();
    }

    // GIVEN : a Roster r
    // RETURNS : a string that represents the Roster.
    // EXAMPLE :
    // toString(Roster1([Player("A"),Player("C"),Player("B")])) => [A, B, C]
    // DESIGN STRATEGY : return a string containing all the names of the
    // players in the Roster.
    public static String toString(Roster r) {
        List<String> str = new ArrayList<String>();
        StringBuilder sb = new StringBuilder();

        for (Player i : r) {
            str.add(i.toString());
        }

        sb.append("[");
        sb.append(String.join(", ", str));
        sb.append("]");

        return sb.toString();
    }
}
