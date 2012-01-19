// Imports a number of packages to make command line interaction with Starling a little easier
import starling.quantity._
import starling.quantity.UOM._
import starling.services.StarlingInit
import starling.market._
import starling.marketdata._
import starling.daterange._
import starling.daterange.Day._
import starling.curves.readers._
import starling.maths._
import starling.instrument._
import starling.db._
import starling.models._
import starling.tradeimport.ClosedDesks
import starling.utils._
import starling.gui.api._
import starling.curves._
import starling.richdb._
import starling.instrument.physical._
import starling.services.rpc.refdata._
import starling.services.rpc.logistics._
import starling.services.rpc.valuation._
import starling.services.rpc.marketdata._
import starling.titan._
import com.trafigura.services.valuation.TitanMarketDataIdentifier
import starling.manager._

def setNullHolidays{
  import starling.calendar._
  import starling.market._
  HolidayTablesFactory.registerHolidayTablesImpl(NullHolidays)
  FuturesExpiryRuleFactory.registerRulesImpl(new FuturesExpiryRules(new BusinessCalendars(NullHolidays)){
    def ruleOption(eaiQuoteID: Int) = Some(new FuturesExpiryRule{
      def lastTradingDay(d: DateRange) = d.firstDay - 1
      val name = "Null holiday expiry rule"
    })

  })

}

def init() = starling.services.StarlingInit.devInstance

lazy val devInstance = init()
lazy val rmiInstance = starling.services.StarlingInit.rmiInstance

lazy val neptuneRefData = devInstance.referenceDataLookup
def makeEnv(pricingGroup : PricingGroup, marketDay : DayAndTime) : Environment = {
  val marketDataStore = devInstance.marketDataStore

  val marketDataSelection = MarketDataSelection(Some(pricingGroup), None)
  val marketDataID = marketDataStore.latestMarketDataIdentifier(marketDataSelection)
  val reader = new NormalMarketDataReader(marketDataStore, marketDataID)

  val rule = new MostRecentClosesEnvironmentRule(neptuneRefData)
  rule.createEnv(marketDay, reader).environment
//  val marketDataSlice = new MarketDataReaderMarketDataSlice(reader, ObservationPoint(marketDay, ObservationTimeOfDay.LMEClose), Map(), new MarketDataTypes(neptuneRefData))
//  Environment(
//    MarketDataCurveObjectEnvironment(
//      marketDay.endOfDay,
//      marketDataSlice,
//      shiftsCanBeIgnored=false,
//      ReferenceDataLookup.Null
//    ))
}
lazy val titanTradeStore = {
  val broadcaster = Broadcaster.Null
  val dbConnection = devInstance.props.StarlingDatabase()
  val richDB = new RichDB(dbConnection, new RichResultSetRowFactory)

  val closedDesks = new ClosedDesks(broadcaster, richDB.db)
  val ts = new starling.titan.TitanTradeStore(richDB, broadcaster, TitanTradeSystem, neptuneRefData, closedDesks)
  //ts.init
  ts
}

def titanTrades = titanTradeStore.readLatestVersionOfAllTrades().values.map(_.trade)
def titanTradeables = titanTrades.map(_.tradeable).flatMap{
  case p : PhysicalMetalAssignmentOrUnassignedSalesQuota => Some(p)
  case _ : ErrorInstrument => None
}

lazy val titanServices = new DefaultTitanServices(devInstance.props)
lazy val logisticsServices = new DefaultTitanLogisticsServices(devInstance.props)

lazy val edmTrades = titanServices.getAllCompletedPhysicalTrades

lazy val titanTradeStoreManager = TitanTradeStoreManager(
  titanServices,
  titanTradeStore,
  titanServices,
  logisticsServices,
  () =>  {}
)

lazy val environmentProvider = new DefaultEnvironmentProvider(devInstance.marketDataStore, neptuneRefData)
lazy val valuationService = new ValuationService(environmentProvider, titanTradeStore)
lazy val marketDataService = new MarketDataService(devInstance.marketDataStore)
