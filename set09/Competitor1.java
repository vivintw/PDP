
//Constructor template for Competitor1:
//new Competitor1 (String s)
//
//Interpretation: the competitor represents an individual or team

//Note:  In Java, you cannot assume a List is mutable, because all
//of the List operations that change the state of a List are optional.
//Mutation of a Java list is allowed only if a precondition or other
//invariant says the list is mutable and you are allowed to change it.

import java.util.List;
import java.util.Set;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;

class Competitor1 implements Competitor {

    String name;

    // GIVEN : a string s 
    // WHERE : s represents the name of this competitor.
    // EFFECT : assigns this competitor the name s.
    // DESIGN STRATEGY : assign s to this.name 
    
    Competitor1(String s) {
        this.name = s;
    }

    // RETURNS: the name of this competitor
    // EXAMPLES :
    // (Competitor "A").name() => "A"
    // DESIGN STRATEGY : return this.name
    
    public String name() {
        return this.name;
    }

    // GIVEN: another competitor and a list of outcomes
    // RETURNS: true iff one or more of the outcomes indicates this
    // competitor has defeated or tied the given competitor
    // EXAMPLES : 
    // Competitor("A").hasDefeated(Competitor("B")),
    //      [Defeat (Competitor("A"), Competitor("B")), 
    //        Tie (Competitor("B"), Competitor("C"))]) => true
    // Competitor("B").hasDefeated(Competitor("A")),
    //      [Defeat (Competitor("A"), Competitor("B")),
    //       Tie (Competitor("B"), Competitor("C"))]) => false
    // DESIGN STRATEGY : iterate on Outcomes checking whether this competitor
    //                   defeats or ties competitor c2
    
    public boolean hasDefeated(Competitor c2, List<Outcome> outcomes) {

        for (Outcome oc : outcomes) {
            if (oc.isTie()) {
                if (tieContainsInorder(this, c2, oc)
                        || tieContainsInorder(c2, this, oc)) {
                    return true;
                }
            } else if (oc.winner().name().equals(this.name)
                    && oc.loser().name().equals(c2.name())) {
                return true;
            }

        }
        return false;

    }

    // GIVEN: a list of outcomes
    // RETURNS: a list of the names of all competitors mentioned by
    // the outcomes that are outranked by this competitor,
    // without duplicates, in alphabetical order
    // EXAMPLES:
    // (Competitor "A").outranks([Defeat(Competitor("A"),Competitor("B")) 
    // Tie (Competitor("B"), Competitor("C"))]) 
    // => ["B", "C"]
    // (Competitor "A").outranks ([Defeat(Competitor("A"), Competitor("B"))
    //                           Tie(Competitor("B") Competitor("C"))
    //                           Defeat(Competitor("C") Competitor("A"))])
    // => [ "A",  "B",  "C"]
    // DESIGN STRATEGY : call a simpler function.
    // HALTING MEASURE : length of the list of all competitors in outcome list
    // - length of output list

    public List<String> outranks(List<Outcome> outcomes) {
        Set<Competitor> output = new HashSet<Competitor>();

        List<Competitor> input = new ArrayList<Competitor>();
        input.add(this);

        List<String> opt = new ArrayList<String>();

        int oldLength;

        while (true) {
            oldLength = output.size();
            input = outrankInner(input, outcomes);
            output.addAll(input);

            if (oldLength == output.size()) {
                for (Competitor c : output) {
                    opt.add(c.name());
                }
                Collections.sort(opt);
                return opt;
            }

        }
    }

    // GIVEN: a list of outcomes
    // RETURNS: a list of the names of all competitors mentioned by
    // the outcomes that outrank this competitor,
    // without duplicates, in alphabetical order
    // EXAMPLES :
    // Competitor("A").outrankedBy([Defeat(Competitor("A"), Competitor("B")),
    //                              Tie (Competitor("B"), Competitor("C"))])
    // => []
    // Competitor("B").outrankedBy([Defeat(Competitor("A"), Competitor("B")),
    //                               Defeat(Competitor("B"), Competitor("A"))])
    // => ["A", "B"]
    // Competitor("C").outrankedBy([ Defeat(Competitor("A"),Competitor("B"))
    //                                Tie(Competitor("B"),Competitor("C"))])
    // => [ "A", "B", "C"]
    // DESIGN STRATEGY : call a simpler function.

    public List<String> outrankedBy(List<Outcome> outcomes) {
        List<Competitor> clst = allCompetitors(outcomes);
        List<String> output = new ArrayList<String>();
        for (Competitor c : clst) {
            if (c.outranks(outcomes).contains(this.name)) {
                output.add(c.name());
            }
        }
        Collections.sort(output);
        return output;
    }

    // GIVEN: a list of outcomes
    // RETURNS: a list of the names of all competitors mentioned by
    // one or more of the outcomes, without repetitions, with
    // the name of competitor A coming before the name of
    // competitor B in the list if and only if the power-ranking
    // of A is higher than the power ranking of B.
    // EXAMPLE : 
    // (Competitor "A").powerRanking([ Defeat(Competitor("A"), Competitor("D"))
    //                                 Defeat(Competitor("A"), Competitor("E"))
    //                                 Defeat(Competitor("C"), Competitor("B"))
    //                                 Defeat(Competitor("C"), Competitor("F"))
    //                                 Tie(Competitor("D"), Competitor("B"))
    //                                 Defeat(Competitor("F"), Competitor("E"))]
    // => ["C", "A", "F", "E", "B", "D"]
    // DESIGN STRATEGY : call a simpler function.
    // WHERE : List clst is mutable.
    public List<String> powerRanking(List<Outcome> outcomes) {

        List<Competitor> clst = allCompetitors(outcomes);
        List<String> outlst = new ArrayList<String>();

        mergeSort(clst, outcomes, 0, clst.size() - 1);

        for (Competitor c : clst) {
            outlst.add(c.name());
        }

        return outlst;
    }

    // GIVEN : a competitor c1, a competitor c2 and an Tie oc
    // RETURNS : true iff the first competitor of the Tie is c1 and the second
    // competitor is c2 in Tie oc.
    // EXAMPLES : 
    // tieContainsInorder(Competitor("A"), Competitor("B"),
    //                    Tie(Competitor("A"),Competitor("B"))) => true
    // tieContainsInorder(Competitor("B"), Competitor("A"),
    //                    Tie(Competitor("A"),Competitor("B"))) => false
    // DESIGN STRATEGY : check if the order of c1 and c2 in oc
    
    private boolean tieContainsInorder(Competitor c1, Competitor c2,
            Outcome oc) {
        if (oc.first().name().equals(c1.name())
                && oc.second().name().equals(c2.name())) {
            return true;
        }
        return false;
    }

    // GIVEN : list of all outcomes
    // RETURNS : list of all competitors that are mentioned in the outcomes
    // EXAMPLES : 
    // allCompetitors([Defeat(Competitor("A"), Competitor("B"))
    //                 Tie(Competitor("B"), Competitor("C"))])
    // => [Competitor("A"), Competitor("B"), Competitor("C")]
    //  allCompetitors([Defeat(Competitor("A"), Competitor("B"))
    //                 Tie(Competitor("C"), Competitor("D"))])
    // => [Competitor("A"), Competitor("B"), Competitor("C"), Competitor("D")]
    // DESIGN STRATEGY : iterate through outcomes and store the competitors into 
    //                   a HashSet
    // WHERE : HashSet clst is mutable.
    
    private List<Competitor> allCompetitors(List<Outcome> outcomes) {
        Set<Competitor> clst = new HashSet<Competitor>();
        for (Outcome oc : outcomes) {
            clst.add(oc.first());
            clst.add(oc.second());
        }
        return new ArrayList<Competitor>(clst);
    }

    // GIVEN : a list of competitors inplst and a list of all outcomes
    // RETURNS : the list of all competitors defeated by competitors in inplst
    // without duplicates.
    // EXAMPLES : 
    // outranksInner([Competitor("A")],
    //               [Defeat(Competitor("A"), Competitor("B")),
    //                Tie(Competitor("B"), Competitor("C"))])
    // => [Competitor("B"),Competitor("C")]
    // outranksInner([Competitor("A")],
    //               [Defeat(Competitor("A"),Competitor("B"))
    //                Tie(Competitor("B"),Competitor("C"))
    //                Defeat(Competitor("C"),Competitor("A"))])
    // => [Competitor("A"), Competitor("B"), Competitor("C")]
    // DESIGN STRATEGY : call a simpler function.
    // WHERE : HashSet output is mutable.
    
    private List<Competitor> outrankInner(List<Competitor> inplst,
            List<Outcome> outcomes) {
        List<Competitor> clst = allCompetitors(outcomes);
        Set<Competitor> output = new HashSet<Competitor>();

        for (Competitor c1 : inplst) {
            for (Competitor c : clst) {
                if (c1.hasDefeated(c, outcomes)) {
                    output.add(c);
                }
            }
        }
        return new ArrayList<Competitor>(output);

    }

    // GIVEN : a competitor c and a list of outcomes outcomes
    // RETURNS : the number of outcomes that state that c defeats or ties
    // another competitor
    // EXAMPLES : 
    // nonLosingNumerator(Competitor("A"), 
    //                    [Defeat(Competitor("A"), Competitor("B")),
    //                     Defeat(Competitor("B"), Competitor("A")),
    //                     Tie(Competitor("B"), Competitor("C"))]) => 1
    // nonLosingNumerator(Competitor("B"),
    //                    [Defeat(Competitor("A"), Competitor("B")),
    //                     Defeat(Competitor("B"), Competitor("A")),
    //                     Tie(Competitor("B"), Competitor("C"))]) => 2
    // DESIGN STRATEGY : iterate through the outcome list incrementing a 
    //                   counter when the outcome states that c defeats or ties
    //                   another competitor.
    
    private int nonLosingNumerator(Competitor c, List<Outcome> outcomes) {
        int ctr = 0;
        for (Outcome oc : outcomes) {
            if (oc.isTie()) {
                if (oc.first().name().equals(c.name())
                        || oc.second().name().equals(c.name()))
                    ctr++;
            } else {
                if (oc.winner().name().equals(c.name()))
                    ctr++;
            }
        }
        return ctr;
    }

    // GIVEN : a competitor c, and a list of outcomes outcomes
    // RETURNS : the number of outcomes that mention c
    // EXAMPLES : 
    // nonLosingDenominator(Competitor("A"),
    //                      [Defeat(Competitor("A"), Competitor("B")),
    //                       Tie(Competitor("B"), Competitor("C"))]) => 1
    // nonLosingDenominator(Competitor("A"),
    //                      [Defeat(Competitor("A"), Competitor("B")),
    //                       Tie(Competitor("B"), Competitor("C")),
    //                       Defeat(Competitor("C"),Competitor("A"))] => 2
    // DESIGN STRATEGY : iterate through the outcome list incrementing a
    //                   counter if the outcome has competitor c in it.
    
    private int nonLosingDenominator(Competitor c, List<Outcome> outcomes) {
        int ctr = 0;
        for (Outcome oc : outcomes) {
            if (oc.first().name().equals(c.name())
                    || oc.second().name().equals(c.name()))
                ctr++;
        }
        return ctr;
    }

    // GIVEN ; a competitor c and a list of outcomes, outcomes
    // RETURNS : the number of outcomes in which c defeats or ties another
    // competitor divided by the number of outcomes that mention c.
    // EXAMPLES : 
    // nonLosingPercent(Competitor("A"),
    //                  [Defeat(Competitor("A"), Competitor("B")),
    //                   Tie(Competitor("B"), Competitor("A"),)]) => 1
    // nonLosingPercent(Competitor("A"),
    //                  [Defeat(Competitor("A"), Competitor("B")),
    //                   Tie(Competitor("B"), Competitor("C")),
    //                   Defeat(Competitor("C"), Competitor("A")),
    //                   Tie(Competitor("A"), Competitor("B"))]) => 2/3
    // DESIGN STRATEGY : call simpler functions.
    
    private float nonLosingPercent(Competitor c, List<Outcome> outcomes) {
        return (float) nonLosingNumerator(c, outcomes)
                / nonLosingDenominator(c, outcomes);
    }

    // GIVEN : a Competitor c1, a Competitor c2, and an OutcomeList outcomes
    // WHERE : c1 != C2
    // AND : outcomes must at least contain one Outcome.
    // RETURNS : true, if the power-rank of c1 is greater than c2
    // EXAMPLES : 
    // powerRank(Competitor("A"), Competitor("B"),
    //           [Defeat(Competitor("A"),Competitor("B"))]) => true
    // powerRank(Competitor("A"),Competitor("B"),
    //          [Defeat(Competitor("B"),Competitor("X")),
    //           Defeat(Competitor("C"),Competitor("B")),
    //           Defeat(Competitor("A"),Competitor("C")),
    //           Defeat(Competitor("B"),Competitor("E"))
    //           Tie(Competitor("A"),Competitor("X"))
    //           Defeat(Competitor("E"),Competitor("F"))] => true
    // powerRank(Competitor("B"),Competitor("A"),
    //          [Defeat(Competitor("B"),Competitor("X")),
    //           Defeat(Competitor("C"),Competitor("B")),
    //           Defeat(Competitor("A"),Competitor("C")),
    //           Defeat(Competitor("B"),Competitor("E"))
    //           Tie(Competitor("A"),Competitor("X"))
    //           Defeat(Competitor("E"),Competitor("F"))] => false
    // DESIGN STRATEGY : call simpler functions.
    
    private boolean powerRank(Competitor c1, Competitor c2,
            List<Outcome> outcomes) {
        int outrankedByC1 = c1.outrankedBy(outcomes).size();
        int outrankedByC2 = c2.outrankedBy(outcomes).size();
        int outranksC1 = c1.outranks(outcomes).size();
        int outranksC2 = c2.outranks(outcomes).size();
        float nonLosingPercentC1 = nonLosingPercent(c1, outcomes);
        float nonLosingPercentC2 = nonLosingPercent(c2, outcomes);

        if (outrankedByC1 < outrankedByC2)
            return true;
        else if (outrankedByC1 > outrankedByC2)
            return false;
        else if (outranksC1 > outranksC2)
            return true;
        else if (outranksC1 < outranksC2)
            return false;
        else if (nonLosingPercentC1 > nonLosingPercentC2)
            return true;
        else if (nonLosingPercentC1 < nonLosingPercentC2)
            return false;
        else
            return c1.name().compareTo(c2.name()) < 0;
    }

    // GIVEN : a CompetitorList clst, an OutcomeList outcomes, and Integers
    // l,h,m
    // WHERE : clst logically divided into 2 parts [l,m] and (m,h], with
    // competitor A coming before competitor B in the list if and only if
    // the power-ranking of A is higher than the power ranking of B.
    // RETURNS : the sorted merge of its two arguments
    // DESIGN STRATEGY : call a simpler function.
    // WHERE : List output and clst are mutable.
    
    private void merge(List<Competitor> clst, List<Outcome> outcomes, int l,
            int h, int m) {

        int i = l, j = m + 1;
        int ctr = 0;
        ArrayList<Competitor> outlist = new ArrayList<Competitor>();

        while (i <= m && j <= h) {
            if (powerRank(clst.get(i), clst.get(j), outcomes))
                outlist.add(clst.get(i++));
            else
                outlist.add(clst.get(j++));
        }

        while (i <= m) {
            outlist.add(clst.get(i++));
        }

        while (j <= h) {
            outlist.add(clst.get(j++));
        }

        while (ctr <= (h - l)) {
            clst.set(ctr + l, outlist.get(ctr));
            ctr++;
        }
    }

    // GIVEN : a CompetitorList clst and an OutcomeList oclst and Integers l,h
    // WHERE : clst and OutcomeList must contain atleast one element each.
    // AND : h and l are non-negative Integers.
    // RETURNS : a CompetitorList like clst but sorted, with competitor A
    // coming before competitor B in the list if and only if
    // the power-ranking of A is higher than the power ranking
    // of B.
    // DESIGN STRATEGY : split clst and recur on the parts, call a simpler 
    //                   function
    // HALTING MEASURE : h - l
    
    private void mergeSort(List<Competitor> clst, List<Outcome> outcomes, int l,
            int h) {
        int m;
        if (l < h) {
            m = (l + h) / 2;

            mergeSort(clst, outcomes, l, m);
            mergeSort(clst, outcomes, m + 1, h);
            merge(clst, outcomes, l, h, m);
        }
    }

}
