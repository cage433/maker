package starling.eai

import starling.auth.User
import java.io.Serializable
import starling.utils.Log
import starling.gui.api.Desk

/**
 * The Traders type provides an implementation that retrieves traders using hard-coded names from a users look-up
 * function then maps these traders to their desks.
 * 
 * @documented
 */
// TODO [DM][2011-09-23] review: make lookup private, to avoid accidental exposure of a potentially private def?
class Traders(lookup: String => Option[User]) {

  /**
   * @param name The trader's user name.
   * @return The trader having the given (case insensitive) user name.
   */
  def trader(name: String): Option[User] = traders.find(_.name.equalsIgnoreCase(name))

  /** This instance's mapping from each trader to its desks.  */
  // TODO [DM][2011-09-23] review: are dynamic trader names required (in Squirrel)?
  lazy val deskMap:Map[User, List[Desk]] = {

    val JaakkoAhmala  = doLookup("jaakko.ahmala")
    val SeetalPatel = doLookup("seetal.patel")

    // london derivatives option
    val JonFox = doLookup("jon.fox")
    val AndreasMattsson = doLookup("andreas.mattsson")

    // north sea crude spec trades are all done under the same user
    val CrudeSpecNorthSeaTraders = Some(User("crudenorthseaspec", "crudenorthseaspec"))

    // houston derivatives
    val MatthewTunney = doLookup("matthew.tunney")
    val JoshHolmes = doLookup("josh.holmes")

    Map(JonFox -> List(Desk.LondonDerivativesOptions),
        AndreasMattsson ->  List(Desk.LondonDerivativesOptions),

        SeetalPatel ->  List(Desk.GasolineSpec),
        JaakkoAhmala ->  List(Desk.LondonDerivatives),

        CrudeSpecNorthSeaTraders ->  List(Desk.CrudeSpecNorthSea),

        MatthewTunney ->  List(Desk.HoustonDerivatives),
        JoshHolmes ->  List(Desk.HoustonDerivatives)
    ).flatMap {
      case (Some(t), b) => {
        Some(t -> b)
      }
      case _ => {
        None
      }
    }
  }

  /** The list of traders from this instance's desk map.  */
  lazy val traders: List[User] = deskMap.keys.toList

  /**
   * @return The given live trader's user name.
   * @throws Exception if the given user is not a mapped trader.
   */
  def getLiveUserName(user : User) = {
    if (!traders.contains(user)) throw new Exception("Not a valid live trader: " + user)

    user.username
  }

  private def doLookup(username: String): Option[User] = {
    lookup(username) match {
      case s@Some(_) => s
      case None => {
        Log.warn("User " + username + " no longer in LDAP?")
        None
      }
    }
  }
}
