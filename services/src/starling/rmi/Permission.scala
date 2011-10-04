package starling.rmi

import starling.gui.api.Desk
import starling.auth.User
import starling.utils.CaseInsensitive._
import starling.utils.CaseInsensitive

object Permission {

  import Groups._

  def desks(user: User): Set[Desk] = {
    val desks = user.groups.flatMap {
      group => {
        groupToDesksMap.getOrElse(group, Set())
      }
    }.toSet
    desks
  }

  val groupToDesksMap: Map[CaseInsensitive, Set[Desk]] =
    Map(
      StarlingLondonDerivatives -> Set(Desk.LondonDerivativesOptions, Desk.LondonDerivatives),
      StarlingGasolineSpec -> Set(Desk.GasolineSpec),
      StarlingHoustonDerivatives -> Set(Desk.HoustonDerivatives),
      StarlingDevelopers -> Desk.values.toSet,
      StarlingTesters -> Desk.values.toSet,
      StarlingAdmin -> Desk.values.toSet
    )
}

object Groups {
  val StarlingDevelopers = "Starling Developers".i
  val StarlingAdmin = "Starling Admin".i
  val StarlingShanghai = "Starling Shanghai".i
  val StarlingLucerne = "Starling Lucerne".i
  val StarlingLucerneAdmin = "Starling Lucerne Admin".i
  val StarlingTesters = "Starling Testers".i
  val StarlingLondonDerivatives = "Starling London derivatives".i
  val StarlingGasolineSpec = "Starling Gasoline Spec Global".i
  val StarlingHoustonDerivatives = "Starling Houston Derivatives".i
}
