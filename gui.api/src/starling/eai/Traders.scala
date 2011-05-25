package starling.eai

import starling.auth.User
import java.io.Serializable
import starling.utils.Log
import starling.gui.api.Desk

class Traders(lookup: String => Option[User]) {

  lazy val bookMap:Map[User,(Book,Desk)] = {
    // london derivatives option
    val JonFox = doLookup("jon.fox")
    val AndreasMattsson = doLookup("andreas.mattsson")

    // london derivatives
    val JaakkoAhmala  = doLookup("jaakko.ahmala")
    val SeetalPatel = doLookup("seetal.patel")
    val MarkHeath = doLookup("mark.heath")

    // north sea crude spec trades are all done under the same user
    val CrudeSpecNorthSeaTraders = Some(User("crudenorthseaspec", "crudenorthseaspec"))

    // houston derivatives
    val MatthewTunney = doLookup("matthew.tunney")
    val JoshHolmes = doLookup("josh.holmes")

    Map(JonFox -> (Book.LondonDerivativesOptions, Desk.LondonDerivativesOptions),
        AndreasMattsson -> (Book.LondonDerivativesOptions, Desk.LondonDerivativesOptions),

        SeetalPatel -> (Book.GasolineSpec, Desk.GasolineSpec),
        MarkHeath -> (Book.LondonDerivatives, Desk.LondonDerivatives),
        JaakkoAhmala -> (Book.LondonDerivatives, Desk.LondonDerivatives),

        CrudeSpecNorthSeaTraders -> (Book.CrudeSpecNorthSea, Desk.CrudeSpecNorthSea),

        MatthewTunney -> (Book.HoustonDerivatives, Desk.HoustonDerivatives),
        JoshHolmes -> (Book.HoustonDerivatives, Desk.HoustonDerivatives)
    ).flatMap {
      case (Some(t), b) => {
        Some(t -> b)
      }
      case _ => {
        None
      }
    }
  }

  lazy val traders = bookMap.keys.toList

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
