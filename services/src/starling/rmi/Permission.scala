package starling.rmi

import starling.gui.api.Desk
import starling.auth.User
import starling.utils.CaseInsensitive._
import starling.utils.CaseInsensitive

object Permission {

  import starling.auth.Groups._

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
      StarlingGasoilSpec -> Set(Desk.GasoilSpec),
      StarlingGasolineSpec -> Set(Desk.GasolineSpec, Desk.GasoilSpec, Desk.GasolinePhysicalBargesAndARABlending),
      StarlingNaphthaSpec -> Set(Desk.NaphthaSpec),
      StarlingHoustonDerivatives -> Set(Desk.HoustonDerivatives),
      StarlingProductionAdmin -> Desk.values.toSet
    )

  private val devGroupToDesksMap0: Map[CaseInsensitive, Set[Desk]] = productionGroupToDesksMap0 ++
    Map(
      StarlingDevelopers -> Desk.values.toSet,
      StarlingTesters -> Desk.values.toSet
    )
}


