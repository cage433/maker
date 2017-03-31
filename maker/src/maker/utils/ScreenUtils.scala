package maker.utils

object ScreenUtils{
  val EscapeChars = sys.env.get("MAKER_ESCAPECHARS").isDefined

  def clear(): Unit = if (EscapeChars)
    sys.env.get("TERM") match {
      case None =>
        // no op
      case Some("rxvt-unicode") =>
        sys.env.get("MAKER_TERM_LINES") match {
          case Some(Int(lines)) =>
            println("\n" * lines)
          case _ =>
        }
        magic()
      case term =>
        magic()
    }

  private def magic(): Unit = {
    println("\u001b[2J\u001b[1000A")
  }
}

object Int {
  def unapply(s: String): Option[Int] = try {
    Some(s.toInt)
  } catch {
    case _: java.lang.NumberFormatException => None
  }
}
