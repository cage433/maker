package starling.gui.api

import starling.quantity.Quantity

case class PivotReportTypeLabel(name:String, slidable:Boolean, fieldNames:List[String]) {
  override def toString = name
}

case class ReportOptionsAvailable(options:List[PivotReportTypeLabel], slideParameters:List[SlideParametersAvailable])

case class SlideParametersAvailable(slideType:String, markets:Option[List[SlideAttributes]], upSteps:String,
                                    downSteps:String, defaultStepSize:Option[String], defaultUOM:Option[String],
                                    commodities:Option[List[SlideAttributes]], slideTypeType:String)

case class SlideAttributes(marketOrCommodity:String, uom: Option[String], stepSize:String)

case class SlideParametersLabel(slideType:String, market:Option[String], stepSize:Double, upSteps:Int, downSteps:Int,
                                uom:String, commodity:Option[String], slideTypeType:String)

case class ReportOptions(options:List[PivotReportTypeLabel], slide1:Option[SlideParametersLabel], slide2:Option[SlideParametersLabel]) {
  def isEmpty = options.isEmpty

  def containsReport(name : String) = options.exists(_.name == name)

  def slides = List(slide1, slide2).flatten
}

case class MonitorInfo(id:String, width:Int, height:Int, availableAcceleratedMemory:Int)

case class OSInfo(name:String, arch:String, version:String, vmName:String, vmVendor:String, vmVersion:String,
                  availableProcessors:Int, totalSystemMemory:Int, freeSystemMemory:Int, numMonitors:Int, monitors:List[MonitorInfo])

case class CostsInfo(name:String, value:Quantity, info:List[(String,String)])
case class CostsLabel(costInfo:List[CostsInfo])

case class BookmarkLabel(name:String, bookmark:String)