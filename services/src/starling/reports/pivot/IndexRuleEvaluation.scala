package starling.reports.pivot

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

  case class Row(day: Day, market: CommodityMarket, observed: FixingPeriod, value: PQ, valueInIndexUOM: PQ)

  def rows(index: Index, period: DateRange, pricingRule: SwapPricingRule, rounding: Option[Int], environment: Environment): (Iterable[Row], PQ) = {
    case class Entry(method: String, params: List[Object], result: Object, keysAndValues: Map[AtomicDatumKey, Any])

    val atomicRecorder = KeyAndValueRecordingCurveObjectEnvironment(environment.atomicEnv)
    val recordingEnvironment = new EnvironmentMethodRecorder(Environment(atomicRecorder), classOf[NoConstructorArgsEnvironment])

    var entries: List[Entry] = Nil
    val watching = List("averagePrice", "fixing", "indexForwardPrice")
    val (averagePrice :: fixing :: indexForwardPrice :: Nil) = watching

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
      case Entry(`fixing`, FixingsHistoryKey(market, observing, _, _) :: (day: Day) :: Nil, result: Quantity, keysAndValues) => {
        val resultInUOM = PQ.calcOrCatch(market.convert(result, index.priceUOM) get)
        Some(Row(day, market, observing, PQ(result), resultInUOM))
      }
      case Entry(`indexForwardPrice`, (singleIndex: SingleIndex) :: (day: Day) :: p, result: Quantity, keysAndValues) => {
        val observing: DateRange = keysAndValues.keys.flatMap {
          case ForwardPriceKey(_, o, _) => Some(o)
          case _ => None
        } head
        val resultInUOM = PQ.calcOrCatch(singleIndex.convert(result, index.priceUOM) get)
        Some(Row(day, singleIndex.market, DateRangeFixingPeriod(observing), PQ(result), resultInUOM))
      }
      case a => {
        None
      }
    }
    (rows, avg)
  }
}

class EnvironmentMethodRecorder[T](wrapped: T, klass: Class[_]) {
  val record = new scala.collection.mutable.HashSet[(String, List[Object], Object)]()

  def proxyWithIntercept(m: (String, List[Object], Object) => Unit): T = {
    val e = new Enhancer()
    e.setSuperclass(klass)
    e.setCallback(new MethodInterceptor() {
      def intercept(enhancedObject: Object, originalMethod: Method,
                    args: Array[Object], enhancedMethod: MethodProxy): Object = {
        def resultOnSelf = enhancedMethod.invokeSuper(enhancedObject, args)

        val argsList = if (args == null) List() else args.toList

        originalMethod.getName match {
          case "instrumentLevelEnv" | "environmentParameters" => {
            originalMethod.invoke(wrapped, args: _*)
          }
          case name => {
            val result = resultOnSelf
            record += ((originalMethod.getName, argsList, result))
            m(originalMethod.getName, argsList, result)
            result
          }
        }
      }
    })
    e.create().asInstanceOf[T]
  }
}