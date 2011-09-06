package starling.services

import org.boris.xlloop.util.ExcelDate
import starling.richdb.RichInstrumentResultSetRow
import java.lang.String
import starling.instrument._
import org.boris.xlloop.reflect.{XLFunction, ReflectFunctionHandler}
import org.boris.xlloop.handler.{FunctionInformationHandler, CompositeFunctionHandler}
import collection.mutable.{Subscriber, Publisher}
import starling.models.{Put, Call, CallOrPut}
import starling.daterange._
import javax.xml.transform.stream.{StreamResult, StreamSource}
import java.io.ByteArrayInputStream
import javax.xml.transform.TransformerFactory
import trade.{ExcelTradeReader, ExcelTradesRange}
import xml.{Utility, Node}
import starling.props.Props
import starling.dbx.ConnectionParams
import starling.db._
import collection.immutable.{TreeSet, SortedMap, TreeMap}
import starling.marketdata._
import starling.quantity.{Conversions, UOM, Quantity, Percentage}
import starling.tradestore.eai.{EAITradeAttributes}
import starling.tradestore.intraday.{IntradayTradeAttributes, IntradayTradeStore}
import starling.market._
import org.boris.xlloop.{IFunctionContext, IFunctionHandler, FunctionServer}
import starling.gui.api._
import org.springframework.core.io.ByteArrayResource
import starling.tradestore.TradePredicate
import starling.pivot.{Totals, TableCell, PivotQuantity, Field, PivotFieldParams}
import starling.utils.{Broadcaster, Log, StringIO}
import starling.curves.{SpreadStdDevSurfaceData, SpreadStdDevSurfaceDataKey}
import starling.pivot.model.{TotalAxisValueType, AxisValue, MeasureAxisValueType}
import starling.auth.{LdapUserLookup, User}
import starling.eai._
import starling.trade.{TradeID, TradeAttributes}
import org.boris.xlloop.xloper.{XLError, XLString, XLoper}
import starling.rmi.{StarlingServer, StarlingServerImpl, UserSettingsDatabase}
import org.springframework.mail.javamail.{JavaMailSender, JavaMailSenderImpl, MimeMessageHelper}

/**
 * Locally store curves uploaded from xlloop and hand them out as price data.
 */
object PricingGroupIDUtil {
  def pricingGroupIDFor(marketDataSelection: MarketDataSelection): Int = {
    pricingGroupIDFor(marketDataSelection.pricingGroup)
  }

  def pricingGroupIDFor(pricingGroup: Option[PricingGroup]): Int = {
    pricingGroup match {
      case None => 4 //in case Jon uses excel only data
      case Some(PricingGroup.LondonDerivativesOptions) => 4
      case _ => throw new Exception("Only LondonDerivativesOptions supports pricing groups at the moment")
    }
  }
}


