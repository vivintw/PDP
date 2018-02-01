
//CONSTRUCTOR TEMPLATE : 
//new Roster1() or 
//ArrayList<Player> lst = new ArrayList<Player>()
//lst.add(Players.make("A"))
//lst.add(Players.make("B"))
//new Roster1(lst)
//INTERP :  A Roster object represents a set of players.

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

public class Roster1 implements Roster {
    // stores the names of all the players belonging to this roster.
    private Set<Player> players = null;

    // EFFECT : initializes this roster to an empty roster
    // EXAMPLE :
    // (Roster()).size() => 0
    // (Roster()).has(Player("A")) => false
    // DESIGN STRATEGY : assign an empty ArrayList of Players to this.players
    public Roster1() {
        players = new HashSet<Player>();
    }

    // GIVEN : a List of Players
    // EFFECT : initializes the given roster with the given list of Players.
    // EXAMPLE :
    // (Roster([Player("A"), Player("B"), PLayer("C")]).size() => 3
    // (Roster(([Player("A"), Player("B"), PLayer("C")]).has(Player("A")) =>
    // true
    // (Roster([Player("A"), Player("B"), PLayer("C")]).has(Player("B")) =>
    // true
    // (Roster([Player("A"), Player("B"), PLayer("C")]).has(Player("C")) =>
    // true
    // (Roster([Player("A"), Player("B"), PLayer("C")]).has(Player("F")) =>
    // false
    // DESIGN STRATEGY : add all Players to this.players
    public Roster1(List<Player> lst) {
        this();
        players.addAll(lst);
    }

    // GIVEN : a Player p
    // RETURNS : a roster consisting of the given player together
    // with all players on this roster.
    // EXAMPLE:
    // r.with(p).with(p) => r.with(p)
    // DESIGN STRATEGY : create a new roster with all the players of this and
    // the given Player
    @Override
    public Roster with(Player p) {
        List<Player> temp = new ArrayList<Player>();
        temp.addAll(players);
        temp.add(p);
        return new Roster1(temp);
    }

    // GIVEN : a player p
    // RETURNS: a roster consisting of all players on this roster
    // except for the given player.
    // EXAMPLES:
    // Rosters.empty().without(p) => Rosters.empty()
    // r.without(p).without(p) => r.without(p)
    // DESIGN STRATEGY : create a new roster with all but the given player
    @Override
    public Roster without(Player p) {
        ArrayList<Player> temp = new ArrayList<Player>();
        for (Player i : players) {
            if (i != p) {
                temp.add(i);
            }
        }
        return new Roster1(temp);
    }

    // GIVEN : a Player p
    // RETURNS : true iff the given player is on this roster.
    // EXAMPLE:
    // Rosters.empty().has(p) => false
    //
    // If r is any roster, then
    //
    // r.with(p).has(p) => true
    // r.without(p).has(p) => false
    // DESIGN STRATEGY : check if this.players contains the given player
    @Override
    public boolean has(Player p) {
        return players.contains(p);
    }

    // RETURNS : the number of players on this roster.
    // EXAMPLE:
    //
    // Rosters.empty().size() => 0
    //
    // If r is a roster with r.size() == n, and r.has(p) is false, then
    //
    // r.without(p).size() => n
    // r.with(p).size => n+1
    // r.with(p).with(p).size => n+1
    // r.with(p).without(p).size() => n
    // DESIGN STRATEGY : return the size of this.players
    @Override
    public int size() {
        return players.size();
    }

    // RETURNS : the number of players on this roster whose current
    // status indicates they are available.
    // EXAMPLE :
    // Player A = Players.make("A")
    // A.changeInjuryStatus(true)
    // Player B = Players.make("B")
    // Player C = Players.make("C")
    // Roster([A, B, C]).readyCount() => 2
    // DESIGN STRATEGY : increment ctr if the player is available, return ctr
    @Override
    public int readyCount() {
        int ctr = 0;
        for (Player p : players) {
            if (p.available()) {
                ctr++;
            }
        }
        return ctr;
    }

    // RETURNS : a roster consisting of all players on this roster
    // whose current status indicates they are available.
    // EXAMPLE :
    // Player A = Players.make("A")
    // A.changeInjuryStatus(true)
    // Player B = Players.make("B")
    // Player C = Players.make("C")
    // Roster([A, B, C]).readyCount() => 2
    // DESIGN STRATEGY : create a new roster with all the player that are
    // available in this
    @Override
    public Roster readyRoster() {
        List<Player> temp = new ArrayList<Player>();
        for (Player p : players) {
            if (p.available()) {
                temp.add(p);
            }
        }
        return new Roster1(temp);
    }

    // RETURNS : an iterator that generates each player on this
    // roster exactly once, in alphabetical order by name.
    // EXAMPLE :
    // Player A = Players.make("A")
    // Player B = Players.make("B")
    // Player C = Players.make("C")
    // Roster r = Roster([A, B, C])
    // r.iterator().next() => A
    // r.iterator().next() => B
    // r.iterator().next() => C
    // DESIGN STRATEGY : remove redundant players and sort the list of players
    // by name, return iterator of the list
    @Override
    public Iterator<Player> iterator() {
        List<Player> p = new ArrayList<Player>(new HashSet<Player>(players));
        Collections.sort(p, new PlayerSortName());
        return p.iterator();
    }

    // RETURNS : a string representing this roster
    // EXAMPLE :
    // Player A = Players.make("A")
    // Player B = Players.make("B")
    // Player C = Players.make("C")
    // Roster r = Roster([A, B, C])
    // r.toString() => [A, B, C]
    // DESIGN STRATEGY : call Rosters.toString()
    @Override
    public String toString() {
        return Rosters.toString(this);
    }

    // RETURNS : a unique Integer for this object
    // EXAMPLE :
    // Player A = Players.make("A")
    // Player B = Players.make("B")
    // Player C = Players.make("C")
    // Roster r = Roster([A, B, C])
    // Roster r1 = Roster([A, B, C])
    // r.hashCode() != r1.hashCode()
    // r.hashCode() == r.hashCode()
    // DESIGN STRATEGY : sum up the hash of all players
    public int hashCode() {
        return players.hashCode();
    }

    // RETURNS : true if this and the given object ob, are the same
    // EXAMPLE :
    // Player A = Players.make("A")
    // Player B = Players.make("B")
    // Player C = Players.make("C")
    // Roster r = Roster([A, B, C])
    // Roster r1 = Roster([A, B, C])
    // r.equals(r) => true
    // r.equals(r1) => false
    // DESIGN STRATEGY : check whether the length of this and ob are the same,
    // check if all Players in this are in ob
    public boolean equals(Object ob) {
        Roster r = (Roster) ob;

        if (this.size() != r.size()) {
            return false;
        }

        for (Player i : r) {
            if (!this.has(i)) {
                return false;
            }
        }

        return true;
    }

}
