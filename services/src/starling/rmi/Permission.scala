package starling.rmi

import starling.gui.api.Desk
import starling.auth.User
import starling.utils.CaseInsensitive._
import starling.utils.CaseInsensitive

object Permission {

  import Groups._

  def desks(user: User, production:Boolean): Set[Desk] = {
    val desks = user.groups.flatMap {
      group => {
        groupToDesksMap(production).getOrElse(group, Set())
      }
    }.toSet
    desks
  }

  def groupToDesksMap(production:Boolean) = if (production) productionGroupToDesksMap0 else devGroupToDesksMap0

  private val productionGroupToDesksMap0: Map[CaseInsensitive, Set[Desk]] =
    Map(
      StarlingLondonDerivatives -> Set(Desk.LondonDerivativesOptions, Desk.LondonDerivatives),
      StarlingGasolineSpec -> Set(Desk.GasolineSpec),
      StarlingHoustonDerivatives -> Set(Desk.HoustonDerivatives),
      StarlingDevelopers -> Desk.values.toSet,
      StarlingAdmin -> Desk.values.toSet
    )

  private val devGroupToDesksMap0: Map[CaseInsensitive, Set[Desk]] =
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
