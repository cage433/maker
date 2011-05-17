package starling.gui.api

class ReportSpecificOptions(val options : List[(String, List[Any])]) {
  def this(seqOptions : (String, List[Any])*) = this(seqOptions.toList)

  def :+(option : (String, List[Any])) = new ReportSpecificOptions(options :+ option)

  def ++(right : ReportSpecificOptions) : ReportSpecificOptions =
    new ReportSpecificOptions(options ++ right.options)

  def distinct : ReportSpecificOptions = new ReportSpecificOptions(options.distinct)

  def default = options.map{ case (label, choices) => label -> choices.head}.toMap

  def stringValues = options.map {
    case (name,values) => {
      name -> values.map { v =>
        v match {
          case true|false => v
          case _ => v.toString
        }
      }
    }
  }

  def labels = options.map(_._1)

  def valuesFor(label : String): Option[scala.List[Any]] = options.toMap.get(label)
}
