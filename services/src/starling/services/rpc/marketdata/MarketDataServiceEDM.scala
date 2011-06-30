package starling.services.rpc.marketdata

import com.trafigura.edm.marketdata.{Maturity => MaturityE, ReferenceRateSource => ReferenceRateSourceE}
import com.trafigura.edm.shared.types.{Quantity => QuantityE, Currency => CurrencyE, Date => DateE}
import com.trafigura.services.{TitanSerializableCurrency, TitanSerializableDate}
import com.trafigura.services.marketdata.{Maturity, ReferenceRateSource}
import com.trafigura.edm.marketdata.MarketDataServiceResourceStub
import starling.services.Server
import com.trafigura.tradinghub.support.ServiceFilter


class MarketDataServiceEDM(service: MarketDataService) extends com.trafigura.edm.marketdata.MarketDataService {
  def getSpotFXRates(observationDate: DateE): List[QuantityE] =
    service.getSpotFXRates(TitanSerializableDate.fromTitan(observationDate)).map(_.toTitan)

  def getSpotFXRate(observationDate: DateE, from: CurrencyE, to: CurrencyE) =
    service.getSpotFXRate(TitanSerializableDate.fromTitan(observationDate), TitanSerializableCurrency.fromTitan(from),
      TitanSerializableCurrency.fromTitan(to)).toTitan

  def getReferenceInterestRates(observationDate: DateE) =
    service.getReferenceInterestRates(TitanSerializableDate.fromTitan(observationDate)).map(_.toTtian)

  def getReferenceInterestRate(observationDate: DateE, source: ReferenceRateSourceE, maturity: MaturityE, currency: CurrencyE) =
    service.getReferenceInterestRate(TitanSerializableDate.fromTitan(observationDate), ReferenceRateSource.fromTitan(source),
      Maturity.fromTitan(maturity), TitanSerializableCurrency.fromTitan(currency)).toTtian
}


class MarketDataServiceResourceStubEx extends MarketDataServiceResourceStub(
  new MarketDataServiceEDM(Server.server.marketDataService), new java.util.ArrayList[ServiceFilter]()) {

  override def requireFilters(filterClasses:String*) {}
}