//Constructor template for Tie1:
//new Tie1 (Competitor c1, Competitor c2)
//Interpretation:
//c1 and c2 have engaged in a contest that ended in a tie

public class Tie1 implements Outcome {

    Competitor c1;
    Competitor c2;
    
    // GIVEN : two competitors c1 and c2
    // EFFECT : c1 and c2 as the Competitors in this outcome.
    // DESIGN STRATEGY : assign c1 to this.c1 and c2 to this.c2
    
    Tie1(Competitor c1, Competitor c2) {
        this.c1 = c1;
        this.c2 = c2;
    }

    // RETURNS: true iff this outcome represents a tie
    // EXAMPLES:
    // Tie1(Competitor("A"), Competitor("B")).isTie() => true
    // DESIGN STRATEGY : return true, as Tie1 represents a Tie outcome.
    
    public boolean isTie() {
        return true;
    }

    // RETURNS: one of the competitors
    // EXAMPLES : 
    // Tie1(Competitor("A"), Competitor("B")).first() => Competitor("A")
    // DESIGN STRATEGY : return Competitor this.c1
    
    public Competitor first() {
        return this.c1;
    }

    // RETURNS: the other competitor
    // EXAMPLES:
    // Tie1(Competitor("A"), Competitor("B")).second() => Competitor("B")
    // DESIGN STRATEGY : return Competitor this.c2

    public Competitor second() {
        return this.c2;
    }

    // GIVEN: no arguments
    // WHERE: this.isTie() is false
    // RETURNS: the winner of this outcome
    // DESIGN STRATEGY : return null as the outcome is a tie.
    
    public Competitor winner() {
       return null;
    }

    // GIVEN: no arguments
    // WHERE: this.isTie() is false
    // RETURNS: the loser of this outcome
    // DESIGN STRATEGY : return null as the outcome is a tie.
    
    public Competitor loser() {
        return null;
    }
}
