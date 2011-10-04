package starling.index

import starling.daterange.{Day, DateRange}
import starling.pivot.{PivotQuantity => PQ}
import starling.utils.MethodRecorder
import starling.curves._
import starling.quantity.{Quantity, UOM}
import starling.instrument.{Tradeable, Instrument}
import starling.market._
import java.lang.reflect.Method
import net.sf.cglib.proxy.{MethodProxy, MethodInterceptor, Enhancer}
import rules.SwapPricingRule

/**
 * Values an averaging trade and returns an explanation of how it was valued.
 */
object IndexRuleEvaluation {

  case class Row(day: Day, index : SingleIndex, value: PQ, valueInIndexUOM: PQ, observed: DateRange) {
    def market = index.market
  }

  /**
   * Returns the average price and rows that describe the components of that price.
   *
   * e.g. for Apr WTI front month it would return all the observed days in april, what the observe (May/Jun) and the price
   * for that observed period. Also returns the average price as calculated by the index
   */
  def averagePrice(index: Index, period: DateRange, pricingRule: SwapPricingRule, rounding: Option[Int], environment: Environment): (Iterable[Row], PQ) = {
    case class Entry(method: String, params: List[Object], result: Object, keysAndValues: Map[AtomicDatumKey, Any])

    val atomicRecorder = KeyAndValueRecordingCurveObjectEnvironment(environment.atomicEnv)
    val recordingEnvironment = new EnvironmentMethodRecorder(Environment(atomicRecorder), classOf[NoConstructorArgsEnvironment])

    var entries: List[Entry] = Nil
    val watching = List("indexFixing", "indexForwardPrice")
    val (indexFixing :: indexForwardPrice :: Nil) = watching

    val avg = PQ.calcOrCatch{
      atomicRecorder.clear
      val env = recordingEnvironment.proxyWithIntercept {
        case (m, p, r) if watching.contains(m) => {
          entries ::= Entry(m, p, r, atomicRecorder.keysAndValues)
          atomicRecorder.clear
        }
        case a => atomicRecorder.clear
      }
      env.averagePrice(index, period, pricingRule, index.priceUOM, rounding)
    }

    val rows = entries.flatMap {
      case Entry(`indexFixing`, (singleIndex : SingleIndex) :: (day: Day) :: Nil, result: Quantity, keysAndValues) => {
        val resultInUOM = PQ.calcOrCatch(singleIndex.convert(result, index.priceUOM) get)
        Some(Row(day, singleIndex, PQ(result), resultInUOM, singleIndex.observedPeriod(day)))
      }
      case Entry(`indexForwardPrice`, (singleIndex: SingleIndex) :: (day: Day) :: p, result: Quantity, keysAndValues) => {
        val resultInUOM = PQ.calcOrCatch(singleIndex.convert(result, index.priceUOM) get)
        keysAndValues.headOption match {
          case Some((ForwardPriceKey(market, observed, _), value)) => Some(Row(day, singleIndex, PQ(result), resultInUOM, observed))
          case _ => throw new Exception("Unexpected keysAndValues for forward price: " + keysAndValues)
        }
      }
      case a => {
        None
      }
    }
    (rows, avg)
  }
}

