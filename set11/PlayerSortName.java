

import java.util.Comparator;

//CONSTRUCTOR TEMPLATE : 
//new PlayerSortName()

public class PlayerSortName implements Comparator<Player> {

  // GIVEN : two players p1 and p2
  // RETURNS : a value less than zero if p1.name() lexically precedes
  // p2.name(), 0 if both pi.names() and p2.name() are the same,
  // a value greater than zero if p2.name() lexically precedes
  // p1.name()
  // EXAMPLE :
  // compare("A","B") < 0 => true
  // compare("B","A") > 0 => true
  // compare("A","A") == 0 => true
  // DESIGN STRATEGY : call a library function compareTo().
  @Override
  public int compare(Player p1, Player p2) {
      return p1.name().compareTo(p2.name());
  }

}
