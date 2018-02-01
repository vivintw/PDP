
//Constructor template for Tie1:
//new Defeat1 (Competitor c1, Competitor c2)
//Interpretation:
//c1 and c2 have engaged in a contest that ended with
//  c1 defeating c2

public class Defeat1 implements Outcome {

    Competitor c1;
    Competitor c2;

    // GIVEN : two competitor c1 and c2
    // WHERE : c1 represents the winner and c2 represents the loser of this
    //         Outcome
    // EFFECT : assigns c1 and c2 as the competitors in this Outcome.
    // DESIGN STRATEGY : assign c1 to this.c1 and c2 to this.c2
    
    Defeat1(Competitor c1, Competitor c2) {
        this.c1 = c1;
        this.c2 = c2;
    }

    // RETURNS: true iff this outcome represents a tie
    // EXAMPLES : 
    // Defeat1(Competitor("A"), Competitor("B")).isTie() => false
    // DESIGN STRATEGY : return false as Defeat1 represents a Defeat
    
    public boolean isTie() {
        return false;
    }

    // RETURNS: one of the competitors
    // EXAMPLES : 
    // Defeat1(Competitor("A"), Competitor("B")).first() => Competitor("A")
    // DESIGN STRATEGY : return Competitor this.c1
    
    public Competitor first() {
        return this.c1;
    }

    // RETURNS: the other competitor
    // EXAMPLES : 
    // Defeat1(Competitor("A"), Competitor("B")).second() => Competitor("B")
    // DESIGN STARTEGY : return Competitor this.c2
    
    public Competitor second() {
        return this.c2;
    }

    // GIVEN: no arguments
    // WHERE: this.isTie() is false
    // RETURNS: the winner of this outcome
    // EXAMPLES:
    // Defeat1(Competitor("A"), Competitor("B")).winner() => Competitor("A")
    // DESIGN STRATEGY : return Competitor this.c1
    
    public Competitor winner() {
        return this.c1;
    }

    // GIVEN: no arguments
    // WHERE: this.isTie() is false
    // RETURNS: the loser of this outcome
    // EXAMPLES : 
    // Defeat1(Competitor("A"), Competitor("B")).loser() => Competitor("B")
    // DESIGN STRATEGY : return Competitor this.c2

    public Competitor loser() {
        return this.c2;
    }
}
