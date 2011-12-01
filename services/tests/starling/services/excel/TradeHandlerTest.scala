package starling.services.excel

import starling.tradestore.intraday.IntradayTradeStore
import starling.auth.{User, LdapUserLookup}
import starling.services.trade.ExcelTradeReader
import org.mockito.Mockito._
import org.mockito.Matchers._
import collection.immutable.Nil
import starling.instrument.Trade
import starling.gui.api.BlotterTradeUpdate
import starling.utils.{StarlingTest}
import starling.utils.StarlingMatchers._
import org.testng.annotations.{AfterClass, BeforeClass, BeforeTest, Test}
import starling.tradestore.TradeStore.StoreResults
import starling.manager.Broadcaster

class TradeHandlerTest extends StarlingTest {
  lazy val tradeHandler = new TradeHandler(broadcaster, tradeReader, intradayTradeStore)

  // Collaborators
  var broadcaster: Broadcaster = _
  var tradeReader: ExcelTradeReader = _
  var intradayTradeStore: IntradayTradeStore = _

  // Data
  val allTrades: List[Trade] = Nil
  val header: Array[Array[Object]] = Array(Array("<header>"))
  val optionalAdditionalTrades: Array[Array[Object]] = Array(Array("<optionalAdditionalTrade>"))
  val tradesHash: Long = 123
  val user = User("<userName>", "<name>", None, None, Nil, "<phoneNumber>", "<email>", "<departments>")

  @BeforeClass
  def initialise {
    User.setLoggedOn(Some(user))

    broadcaster = mock(classOf[Broadcaster])
    tradeReader = mock(classOf[ExcelTradeReader]);
    intradayTradeStore = mock(classOf[IntradayTradeStore])
//    traders = mock(classOf[Traders])
  }

  @AfterClass
  def tearDown {
    User.setLoggedOn(None)
  }

  // I don't think anyone ever listened to this.
//  @Test
//  def shouldSendTradesFromTradeReaderToIntradayTradeStoreAndBroadCast {
//    when(tradeReader.allTrades(anyHeader, anyTrades, anySubgroupName)) thenReturn allTrades
//    when(intradayTradeStore.storeTrades(anyUser, anySubgroupName, same(allTrades))) thenReturn (StoreResults(1, 2, 3, tradesHash))
//    //when(ldapUserLookup.user(anyString)) thenReturn Some(anyUser)
//
//    assert(tradeHandler.blotterTrades("<subgroupName>", header, optionalAdditionalTrades) == "OK:" + tradesHash)
//
//    verify(intradayTradeStore).storeTrades(anyUser, anySubgroupName, same(allTrades))
//
//    verify(broadcaster).broadcast(
//      BlotterTradeUpdate(user, "Oil Derivatives/Scratch/Houston Derivatives/<name>/<subgroupName>", List( List("<header>"), List("<optionalAdditionalTrade>"))))
//  }

}