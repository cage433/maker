package starling.instrument

/**
 * Created by IntelliJ IDEA.
 * User: thomas
 * Date: 12-Aug-2010
 * Time: 15:37:30
 * To change this template use File | Settings | File Templates.
 */

import starling.utils.StarlingTest
import cern.colt.matrix.impl.{DenseDoubleMatrix1D => DVector, DenseDoubleMatrix2D => DMatrix}
import starling.maths.{AcklamInverseNormal, MatrixUtils}
import org.testng.annotations._
import org.testng.Assert._
import org.mockito.Mockito._
import org.mockito.Matchers._
import org.mockito.Matchers.eq
import starling.quantity.UOM._
import starling.utils.ScalaTestUtils._
import starling.varcalculator._
import starling.quantity.{UOM, Quantity}
import org.mockito.{ArgumentMatcher, ArgumentCaptor}
import reflect.Manifest
import starling.curves._
import starling.daterange.Day
import starling.market.RIC

import starling.quantity.utils.QuantityTestUtils._

class NetEquityPositionTests extends StarlingTest {

	@Test
	/** Value a typical FX Forward and check its mtm is as expected
  */
	def testMTM {
	  val marketDay = Day(2009, 10, 1).endOfDay
	  val envBuilder = TestEnvironmentBuilder(marketDay)
    val ric = RIC("AEM")
    val price = Quantity(70.1, USD / SHARE)

	  envBuilder.setEquityPrice(ric, price)
	  val env = envBuilder.build

	  val volume = Quantity(1000.0, SHARE)

    val netEquityPosition = NetEquityPosition(ric, volume)

	  assertQtyEquals(
     volume * price,
     netEquityPosition.mtm(env),
     1e-6)
	}

  @Test
  def testMTMForCADListedEquity {
    val marketDay = Day(2009, 10, 1).endOfDay
    val envBuilder = TestEnvironmentBuilder(marketDay)
    val ric = RIC("SU.TO") //The .TO implies CAD
    val price = Quantity(70.1, CAD / SHARE)
    val spotFX = Quantity(1.1, USD/CAD)

    envBuilder.setEquityPrice(ric, price)
    envBuilder.setUSDPerCCYSpotRate(CAD, spotFX)
    val env = envBuilder.build

    val volume = Quantity(1000.0, SHARE)

    val netEquityPosition = NetEquityPosition(ric, volume)

    assertQtyEquals(
     volume * price * spotFX,
     netEquityPosition.mtm(env, UOM.USD),
     1e-6)
  }

}
