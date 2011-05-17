package starling.daterange


case class Tenor(tenorName: String, value: Int) extends Ordered[Tenor] {
  def compare(that: Tenor) = indexOf(tenorName).compare(indexOf(that.tenorName)) match {
    case 0 => value.compare(that.value)
    case other => other
  }

  override def toString = value + tenorName

  private def indexOf(tenor: String) = TenorType.ALL_IN_ORDER.indexOf(tenor)
}

object Tenor {
  def apply(tenorType: TenorType, value: Int): Tenor = Tenor(tenorType.shortName, value)

  val ON = Tenor(Day, 0)
  val SN = Tenor(Day, 0)
}

case class StoredFixingPeriod(period: Either[DateRange, Tenor]) extends Ordered[StoredFixingPeriod] {
  def compare(that: StoredFixingPeriod) = (period, that.period) match {
    case (Left(leftDR), Left(rightDR)) => leftDR.compare(rightDR)
    case (Right(leftOffset), Right(rightOffset)) => leftOffset.compare(rightOffset)
    case (Left(_), Right(_)) => 1
    case (Right(_), Left(_)) => -1
  }

  override def toString = period.fold(_.toString, tenor => if (tenor.value == 0) "CASH" else tenor.toString)
}

object StoredFixingPeriod {
  object Tenor {
    def unapply(storedFixingPeriod: StoredFixingPeriod): Option[Tenor] = {
      storedFixingPeriod.period.right.toOption
    }
  }

  def dateRange(dateRange: DateRange) = StoredFixingPeriod(Left(dateRange))
  def tenor(offset: Tenor) = StoredFixingPeriod(Right(offset))
}
