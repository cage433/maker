package starling.db

import com.thoughtworks.xstream.io.{HierarchicalStreamReader, HierarchicalStreamWriter}
import com.thoughtworks.xstream.converters.{Converter, UnmarshallingContext, MarshallingContext}
import starling.market.{FuturesMarket, Index, Market, CommodityMarket}
import starling.marketdata.{OilVolSurfaceDataKey, BradyMetalVolsDataKey, PriceDataKey}
import starling.curves.{SpreadStdDevSurfaceDataKey, SpreadSkewStdDevCurveKey}

object MarketDataXStreamConverters {

  val priceDataKeyConverter = new Converter {
    def canConvert(theType: Class[_]) = {
      theType == classOf[PriceDataKey]
    }

    def marshal(obj: Object, writer: HierarchicalStreamWriter, context: MarshallingContext) {
      writer.startNode("market")
      writer.addAttribute("class", classOf[CommodityMarket].getCanonicalName)
      writer.setValue(obj.asInstanceOf[PriceDataKey].market.name)
      writer.endNode
    }

    def unmarshal(reader: HierarchicalStreamReader, context: UnmarshallingContext) = {
      reader.moveDown
      val text = reader.getValue()
      reader.moveUp
      PriceDataKey(Market.fromName(text))
    }
  }

  val bradyMetalVolsDataKeyConverter = new Converter {
    def canConvert(theType: Class[_]) = {
      theType == classOf[BradyMetalVolsDataKey]
    }

    def marshal(obj: Object, writer: HierarchicalStreamWriter, context: MarshallingContext) {
      writer.startNode("market")
      writer.addAttribute("class", classOf[CommodityMarket].getCanonicalName)
      writer.setValue(obj.asInstanceOf[BradyMetalVolsDataKey].market.name)
      writer.endNode
    }

    def unmarshal(reader: HierarchicalStreamReader, context: UnmarshallingContext) = {
      reader.moveDown
      val text = reader.getValue()
      reader.moveUp
      BradyMetalVolsDataKey(Market.fromName(text))
    }
  }


  val oilVolsDataKeyConverter = new Converter {
    def canConvert(theType: Class[_]) = {
      theType == classOf[OilVolSurfaceDataKey]
    }

    def marshal(obj: Object, writer: HierarchicalStreamWriter, context: MarshallingContext) {
      writer.startNode("market")
      writer.addAttribute("class", classOf[CommodityMarket].getCanonicalName)
      writer.setValue(obj.asInstanceOf[OilVolSurfaceDataKey].market.name)
      writer.endNode
    }

    def unmarshal(reader: HierarchicalStreamReader, context: UnmarshallingContext) = {
      reader.moveDown
      val text = reader.getValue()
      reader.moveUp
      OilVolSurfaceDataKey(Market.fromName(text))
    }
  }

  val spreadStdDevDataKeyConverter = new Converter {
    def canConvert(theType: Class[_]) = {
      theType == classOf[SpreadStdDevSurfaceDataKey]
    }

    def marshal(obj: Object, writer: HierarchicalStreamWriter, context: MarshallingContext) {
      writer.startNode("market")
      writer.addAttribute("class", classOf[FuturesMarket].getCanonicalName)
      writer.setValue(obj.asInstanceOf[SpreadStdDevSurfaceDataKey].market.name)
      writer.endNode
    }

    def unmarshal(reader: HierarchicalStreamReader, context: UnmarshallingContext) = {
      reader.moveDown
      val text = reader.getValue()
      reader.moveUp
      SpreadStdDevSurfaceDataKey(FuturesMarket.fromName(text))
    }
  }

  val converters = List(priceDataKeyConverter, bradyMetalVolsDataKeyConverter, oilVolsDataKeyConverter, spreadStdDevDataKeyConverter)
}