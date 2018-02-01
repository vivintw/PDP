
//CONSTRUCTOR TEMPLATE : 
// new Player1("Gordon Wayhard")
//INTERP :  A Player object represents a member of a team.

public class Player1 implements Player {
    // represents the name of the this
    private String name;
    // represents the status of this players contract
    private boolean underContract;
    // represents if this player is injured.
    private boolean injured;
    // represents if this player is suspended.
    private boolean suspended;

    Player1(String name) {
        this.name = name;
        this.injured = false;
        this.suspended = false;
        this.underContract = true;
    }

    // RETURNS : the name of this player.
    // EXAMPLE:
    // Players.make("Gordon Wayhard").name() => "Gordon Wayhard"
    // DESIGN STRATEGY : return this.name
    @Override
    public String name() {
        return this.name;
    }

    // RETURNS : true iff this player is
    // under contract, and not injured, and not suspended
    // EXAMPLE:
    // Player gw = Players.make ("Gordon Wayhard");
    // System.out.println (gw.available()); // prints true
    // gw.changeInjuryStatus (true);
    // System.out.println (gw.available()); // prints false
    // DESIGN STRATEGY : return underContract && !injured && !suspended
    @Override
    public boolean available() {
        return (underContract && !injured && !suspended);
    }

    // RETURNS : true iff this player is under contract (employed).
    // EXAMPLE:
    // Player ih = Players.make ("Isaac Homas");
    // System.out.println (ih.underContract()); // prints true
    // ih.changeContractStatus (false);
    // System.out.println (ih.underContract()); // prints false
    // ih.changeContractStatus (true);
    // System.out.println (ih.underContract()); // prints true
    // DESIGN STRATEGY : return this.underContract
    @Override
    public boolean underContract() {
        return underContract;
    }

    // RETURNS : true iff this player is injured.
    // EXAMPLE :
    // Player ih = Players.make ("Isaac Homas");
    // ih.isInjured() => false
    // ih.changeInjuryStatus(true)
    // ih.isInjured() => true
    // DESIGN STRATEGY : return this.injured
    @Override
    public boolean isInjured() {
        return injured;
    }

    // RETURNS : true iff this player is suspended.
    // EXAMPLE :
    // Player ih = Players.make ("Isaac Homas");
    // ih.isSuspended() => false
    // ih.changeSuspendedStatus(true)
    // ih.isSuspended() => true
    // DESIGN STRATEGY : return this.suspended
    @Override
    public boolean isSuspended() {
        return suspended;
    }

    // GIVEN : a boolean
    // EFFECT : Changes the underContract() status of this player to the
    // specified boolean.
    // EXAMPLE :
    // Player ih = Players.make ("Isaac Homas");
    // ih.underContract() => true
    // ih.changeContractStatus(false)
    // ih.underContract() => false
    // DESIGN STRATEGY : assign given boolean to this.underContract
    @Override
    public void changeContractStatus(boolean newStatus) {
        this.underContract = newStatus;

    }

    // GIVEN : a boolean
    // EFFECT : Changes the isInjured() status of this player
    // to the specified boolean.
    // EXAMPLE :
    // Player ih = Players.make ("Isaac Homas");
    // ih.isInjured() => false
    // ih.changeInjuryStatus(true)
    // ih.isInjured() => true
    // DESIGN STRATEGY : assign given boolean to this.injured
    @Override
    public void changeInjuryStatus(boolean newStatus) {
        this.injured = newStatus;
    }

    // GIVEN : a boolean
    // EFFECT : Changes the isSuspended() status of this player
    // to the specified boolean.
    // EXAMPLE :
    // Player ih = Players.make ("Isaac Homas");
    // ih.isSuspended() => false
    // ih.changeSuspendedStatus(true)
    // ih.isSuspended() => true
    // DESIGN STRATEGY : assign given boolean to this.suspended
    @Override
    public void changeSuspendedStatus(boolean newStatus) {
        this.suspended = newStatus;

    }

    // RETURNS : a unique Integer for this object
    // EXAMPLE :
    // Player A1 = Players.make("A")
    // Player A2 = Players.make("A")
    // A1.hashCode() != A2.hashCode()
    // A1.hashCode() == A1.hashCode()
    // DESIGN STRATEGY : call super.hashCode()
    public int hashCode() {
        return super.hashCode();
    }

    // GIVEN : an object p2
    // RETURNS : true if this is the same object as p2
    // EXAMPLE :
    // Player A1 = Players.make("A")
    // Player A2 = Players.make("A")
    // A1.equals(A2) => false
    // A1.equals(A1) => true
    // DESIGN STRATEGY : use object comparison between this and p2
    public boolean equals(Object p2) {
        if (p2 instanceof Player) {
            Player p = (Player) p2;
            return this == p;
        }
        return false;
    }

    // RETURNS : a string that represents this
    // EXAMPLE :
    // Player A = Players.make("A")
    // A.toString() => "A"
    // DESIGN STRATEGY : call Players.toString()
    public String toString() {
        return Players.toString(this);
    }

}
