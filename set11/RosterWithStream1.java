
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

// CONSTRUCTOR TEMPLATE : 
// RosterWithStream r = new RosterWithStream1()
// List<Players> lst = new ArrayList();
// lst.add(A);
// lst.add(B);
// RosterWithStream r = new RosterWithStream1(lst)
// INTERP : 
// A RosterWithStream object represents a set of players.

public class RosterWithStream1 implements RosterWithStream {
    // stores the names of all the players belonging to this RosterWithStream1.
    private Set<Player> players = null;

    // EFFECT : initializes this RosterWithStream to an empty RosterWithStream1
    // EXAMPLE :
    // (RosterWithStream1()).size() => 0
    // (RosterWithStream1()).has(Player("A")) => false
    // DESIGN STRATEGY : assign an empty HashSet of Players to this.players
    public RosterWithStream1() {
        players = new HashSet<Player>();
    }

    // GIVEN : a List of Players
    // EFFECT : initializes the given RosterWithStream with the given list of
    // Players.
    // EXAMPLE :
    // (RosterWithStream([Player("A"), Player("B"), PLayer("C")]).size() => 3
    // (RosterWithStream(([Player("A"), Player("B"),
    // PLayer("C")]).has(Player("A")) =>
    // true
    // (RosterWithStream([Player("A"), Player("B"),
    // PLayer("C")]).has(Player("B")) =>
    // true
    // (RosterWithStream([Player("A"), Player("B"),
    // PLayer("C")]).has(Player("C")) =>
    // true
    // (RosterWithStream([Player("A"), Player("B"),
    // PLayer("C")]).has(Player("F")) =>
    // false
    // DESIGN STRATEGY : add all Players to this.players
    public RosterWithStream1(List<Player> lst) {
        this();
        players.addAll(lst);
    }

    // GIVEN : a player p
    // RETURNS : a roster consisting of the given player together
    // with all players on this roster.
    // EXAMPLE:
    // r.with(p).with(p) => r.with(p)
    // DESIGN STRATEGY : create a new roster with all the players of this and
    // the given Player
    @Override
    public RosterWithStream with(Player p) {
        List<Player> temp = new ArrayList<Player>();
        temp.addAll(players);
        temp.add(p);
        return new RosterWithStream1(temp);
    }

    // GIVEN : a player p
    // RETURNS : a roster consisting of all players on this roster
    // except for the given player.
    // EXAMPLE:
    // RosterWithStreams.empty().without(p) => RosterWithStreams.empty()
    // r.without(p).without(p) => r.without(p)
    // DESIGN STRATEGY : use HOF filter
    @Override
    public RosterWithStream without(Player p) {
        // GIVEN : a player pl
        // RETURNS : player if player not equal to p
        return new RosterWithStream1(players.stream().filter(pl -> pl != p)
                .collect(Collectors.toList()));
    }

    // GIVEN : a player p
    // RETURNS true iff the given player is on this roster.
    // EXAMPLE:
    //
    // RosterWithStreams.empty().has(p) => false
    //
    // If r is any roster, then
    //
    // r.with(p).has(p) => true
    // r.without(p).has(p) => false
    // DESIGN STRATEGY : use HOF anyMatch
    @Override
    public boolean has(Player p) {
        // GIVEN : a player pl
        // RETURNS : true if pl equals p else false
        return players.stream().anyMatch(pl -> pl.equals(p));
    }

    // RETURNS : the number of players on this roster.
    // EXAMPLE:
    //
    // RosterWithStreams.empty().size() => 0
    //
    // If r is a roster with r.size() == n, and r.has(p) is false, then
    //
    // r.without(p).size() => n
    // r.with(p).size() => n+1
    // r.with(p).with(p).size() => n+1
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
    // RosterWithStream([A, B, C]).readyCount() => 2
    // DESIGN STRATEGY : call a simpler function
    @Override
    public int readyCount() {
        return (int) players.stream().filter(p -> p.available()).count();
    }

    // RETURNS : a roster consisting of all players on this roster
    // whose current status indicates they are available.
    // EXAMPLE :
    // Player A = Players.make("A")
    // A.changeInjuryStatus(true)
    // Player B = Players.make("B")
    // Player C = Players.make("C")
    // RosterWithStream([A, B, C]).readyCount() => 2
    // DESIGN STRATEGY : use HOF filter and collect
    @Override
    public RosterWithStream readyRoster() {
        // GIVEN : a player p
        // RETURNS : p if p.available()
        return new RosterWithStream1(players.stream().filter(p -> p.available())
                .collect(Collectors.toList()));
    }

    // RETURNS : an iterator that generates each player on this
    // roster exactly once, in alphabetical order by name.
    // EXAMPLE :
    // Player A = Players.make("A")
    // Player B = Players.make("B")
    // Player C = Players.make("C")
    // RosterWithStream r = RosterWithStream ([A, B, C])
    // r.iterator().next() => A
    // r.iterator().next() => B
    // r.iterator().next() => C
    // DESIGN STRATEGY : sort the list of players
    // by name, return iterator of the list

    @Override
    public Iterator<Player> iterator() {
        List<Player> temp = new ArrayList<Player>(players);
        Collections.sort(temp, new PlayerSortName());
        return Collections.unmodifiableList(temp).iterator();
    }

    // RETURNS : a sequential Stream with this RosterWithStream
    // as its source.
    // The result of this method generates each player on this
    // roster exactly once, in alphabetical order by name.
    // Examples:
    //
    // RosterWithStreams.empty().stream().count() => 0
    //
    // RosterWithStreams.empty().stream().findFirst().isPresent()
    // => false
    //
    // RosterWithStreams.empty().with(p).stream().findFirst().get()
    // => p
    //
    // this.stream().distinct() => this.stream()
    //
    // this.stream().filter((Player p) -> p.available()).count()
    // => this.readyCount()
    // DESIGN STRATEGY : generate a stream using the splititerator
    @Override
    public Stream<Player> stream() {
        return StreamSupport.stream(this.spliterator(), false);
    }

    // RETURNS : a unique Integer for this object
    // EXAMPLE :
    // Player A = Players.make("A")
    // Player B = Players.make("B")
    // Player C = Players.make("C")
    // RosterWithStream r = RosterWithStream([A, B, C])
    // RosterWithStream r1 = RosterWithStream([A, B, C])
    // r.hashCode() != r1.hashCode()
    // r.hashCode() == r.hashCode()
    // DESIGN STRATEGY : return this.players.hashcode()
    @Override
    public int hashCode() {
        return players.hashCode();
    }

    // RETURNS : true if this and the given object ob, are the same
    // EXAMPLE :
    // Player A = Players.make("A")
    // Player B = Players.make("B")
    // Player C = Players.make("C")
    // RosterWithStream r = RosterWithStream([A, B, C])
    // RosterWithStream r1 = RosterWithStream([A, B, C])
    // r.equals(r) => true
    // r.equals(r1) => false
    // DESIGN STRATEGY : check whether the length of this and ob are the same,
    // check if all Players in this are in ob
    @Override
    public boolean equals(Object ob) {

        if (ob instanceof RosterWithStream) {
            RosterWithStream r = (RosterWithStream) ob;

            if (r.size() != size()) {
                return false;
            }

            for (Player p : r) {
                if (!has(p)) {
                    return false;
                }
            }
            return true;
        }
        return false;
    }

    // RETURNS : a string representing this roster
    // EXAMPLE :
    // Player A = Players.make("A")
    // Player B = Players.make("B")
    // Player C = Players.make("C")
    // RosterWithStream r = RosterWithStream([A, B, C])
    // r.toString() => [A, B, C]
    // DESIGN STRATEGY : call RosterWithStreams.toString()
    @Override
    public String toString() {
        return RosterWithStreams.toString(this);
    }

}
