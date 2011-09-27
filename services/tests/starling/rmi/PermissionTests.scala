package starling.rmi

import org.testng.annotations.Test
import org.testng.Assert._
import starling.auth.User
import starling.utils.CaseInsensitive._
import starling.gui.api.Desk
import starling.utils.{StarlingTest, CaseInsensitive}

class PermissionTests extends StarlingTest {
  @Test
  def testGroups = {
    val jon = User("jon.fox", "Jon Fox", None, None, List("StaRling london DerivAtives"))
    assertEquals(Permission.desks(jon, true), Set(Desk.LondonDerivativesOptions, Desk.LondonDerivatives))

    val josh = User("josh.holmes", "Josh Holmes", None, None, List("StaRling houSton DerivAtives"))
    assertEquals(Permission.desks(josh, true), Set(Desk.HoustonDerivatives))

    val seetal = User("seetal.patel", "Seetal Patel", None, None, List("StaRling Gasoline Spec GloBal"))
    assertEquals(Permission.desks(seetal, true), Set(Desk.GasolineSpec, Desk.GasoilSpec, Desk.GasolinePhysicalBargesAndARABlending))
  }
}