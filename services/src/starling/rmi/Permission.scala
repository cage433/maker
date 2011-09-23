package starling.rmi

import starling.gui.api.Desk
import starling.auth.User
import starling.utils.CaseInsensitive._
import starling.utils.CaseInsensitive

/**
 * The Permission singleton implementation defines the mapping from the production and development environments to
 * the set of desks for each by Group value.
 *
 * @see Groups
 * @documented
 */
object Permission {

  import Groups._

  /**
   * @param user The user.
   * @param production A statement as to whether the production group is required, true if it is, false if the
   * development one is.
   * @return The set of desks for the given user's groups in the production or development environments. 
   */
  def desks(user: User, production:Boolean): Set[Desk] = {
    val desks = user.groups.flatMap {
      group => {
        groupToDesksMap(production).getOrElse(group, Set())
      }
    }.toSet
    desks
  }

  /**
   * Returns the (group, desks) maps for the production or development environments.
   * @param production true if production desks are required, false if development ones are.
   */
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

/**
 * The Groups singleton defines a set of case insensitive groups against which Desks may be persmissioned.
 *
 * @documented
 */
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
