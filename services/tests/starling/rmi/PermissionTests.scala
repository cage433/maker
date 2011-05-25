package starling.rmi

import org.testng.annotations.Test
import org.testng.Assert._
import starling.auth.User
import starling.utils.CaseInsensitive._
import starling.utils.CaseInsensitive
import starling.gui.api.Desk

class PermissionTests {
  @Test
  def testGroups = {
    val jon = User("jon.fox", "Jon Fox", None, List("StaRling london DerivAtives"))
    assertEquals(Permission.desks(jon), Set(Desk.LondonDerivativesOptions, Desk.LondonDerivatives))

    val josh = User("josh.holmes", "Josh Holmes", None, List("StaRling houSton DerivAtives"))
    assertEquals(Permission.desks(josh), Set(Desk.HoustonDerivatives))

    val seetal = User("seetal.patel", "Seetal Patel", None, List("StaRling Gasoline Spec GloBal"))
    assertEquals(Permission.desks(seetal), Set(Desk.GasolineSpec))
  }
}