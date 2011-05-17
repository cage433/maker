package starling.services.excel

import starling.tradestore.intraday.IntradayTradeStore
import starling.auth.{User, LdapUserLookup}
import starling.services.trade.ExcelTradeReader
import org.mockito.Mockito._
import org.mockito.Matchers._
import collection.immutable.Nil
import starling.trade.Trade
import starling.gui.api.BlotterTradeUpdate
import starling.utils.{Broadcaster, StarlingTest}
import starling.utils.StarlingMatchers._
import starling.eai.Traders
import org.testng.annotations.{AfterClass, BeforeClass, BeforeTest, Test}

class TradeHandlerTest extends StarlingTest {
  lazy val tradeHandler = new TradeHandler(broadcaster, tradeReader, intradayTradeStore, traders)

  // Collaborators
  var broadcaster: Broadcaster = _
  var tradeReader: ExcelTradeReader = _
  var intradayTradeStore: IntradayTradeStore = _

  // Data
  val allTrades: List[Trade] = Nil
  val header: Array[Array[Object]] = Array(Array("<header>"))
  val optionalAdditionalTrades: Array[Array[Object]] = Array(Array("<optionalAdditionalTrade>"))
  val tradesHash: Long = 123
  val user = User("<userName>", "<name>", None, Nil, "<phoneNumber>", "<email>", "<departments>")
  var traders: Traders = new Traders((s) => Some(user))

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

  @Test
  def shouldSendTradesFromTradeReaderToIntradayTradeStoreAndBroadCast {
    when(tradeReader.allTrades(anyHeader, anyTrades, anySubgroupName)) thenReturn allTrades
    when(intradayTradeStore.storeTrades(anyUser, anySubgroupName, same(allTrades))) thenReturn ((tradesHash, true))
    //when(ldapUserLookup.user(anyString)) thenReturn Some(anyUser)

    assert(tradeHandler.blotterTrades("<subgroupName>", header, optionalAdditionalTrades) == "OK:" + tradesHash)

    verify(intradayTradeStore).storeTrades(anyUser, anySubgroupName, same(allTrades))

    verify(broadcaster).broadcast(
      BlotterTradeUpdate(user, "Oil Derivatives/Scratch/Houston Derivatives/<name>/<subgroupName>", List( List("<header>"), List("<optionalAdditionalTrade>"))))
  }

}