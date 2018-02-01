
//If p1 and p2 are players with distinct names, then
//p1.toString() is not the same string as p2.toString().
//
//Players.make(String name) is a static factory method that returns
//a player with the given name who is (initially) available.

public class Players {

    // GIVEN : a String
    // RETURNS : a Player with the name of the player being the given String
    // EXAMPLE :
    // Players.make("A") returns a player with name "A"
    // DESIGN STRATEGY : return an object of class Player1
    public static Player make(String name) {
        return (new Player1(name));
    }

    // GIVEN : a Player p
    // RETURNS : a string that represents p
    // EXAMPLE :
    // Player A = Players.make("A")
    // A.toString() => "A"
    // DESIGN STRATEGY : return p.name()
    public static String toString(Player p) {
        return p.name();
    }
}
