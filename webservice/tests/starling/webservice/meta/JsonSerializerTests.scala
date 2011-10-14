package starling.webservice.meta

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.WordSpec
import com.trafigura.services.marketdata.SpotFXRate
import com.trafigura.services.valuation.TitanMarketDataIdentifier
import com.trafigura.services.TitanSerializableQuantity
import scalaz.Scalaz._
import starling.webservice.{EDMFormats, JsonSerializer}
import starling.daterange.Day._


class JsonSerializerTests extends WordSpec with ShouldMatchers {
  implicit val formats = EDMFormats
  val serializer = JsonSerializer(null)
  val identifier = TitanMarketDataIdentifier.valueOf("4717-2011-10-12")
  val rate = TitanSerializableQuantity(0.12, Map("USD" → -1, "GBP" → 1))

  def logException[T](f: => T) = try {
    f
  } catch {
    case e => e.printStackTrace()
  }

  "should be able to serialize a Day" in logException {
    serializer.serialize(12 Oct 2011) should be === "\"" + ((12 Oct 2011).toString) + "\""
  }

  "should be able to serialize a SpotFXRate" in logException {
    serializer.serialize(SpotFXRate(identifier, rate))
  }

  "should be able to serialize lots of SpotFXRates" in {
    serializer.serialize(SpotFXRate(identifier, rate).replicate[List](100))
  }
}