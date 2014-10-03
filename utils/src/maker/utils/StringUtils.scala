package maker.utils


object StringUtils{
  private val camel = "[A-Z][^A-Z]*".r

  private def segments(s : String) = {
    val segments = camel.findAllIn(s).toList
    if (segments.map(_.size).sum == s.size)
      segments
    else 
      Nil
  }

  def intellijDistance(matcher : String, name : String) : Option[Int] = {
    val segments1 = segments(matcher)
    val segments2 = segments(name)
    if (segments1.nonEmpty && segments1.size <= segments2.size && segments1.zip(segments2).forall{
      case (s1, s2) => 
        s2.startsWith(s1)
    }){
      Some(segments2.size - segments1.size)
    } else {
      None
    }
  }

  def bestIntellijMatches(matcher : String, names : Iterable[String]) : List[String] = {
    def shortClassName(longName : String) = longName.split('.').last
    val grouped = names.groupBy{name => intellijDistance(matcher, shortClassName(name))}
    val distances = grouped.keySet.flatten.toList.sortWith(_<_)
    distances.headOption match {
      case s : Some[Int] => grouped(s).toList.sortWith(_.length < _.length)
      case _ => Nil
    } 
  }
}
