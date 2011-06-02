package starling.utils.conversions


trait RichOption {
  implicit def enrichOption[A](option: Option[A]) = new {
    def optPair[B](b: B): Option[(A, B)] = option.map(_ → b)
    def flatMapL[B](f: A => List[B]): List[B] = option.map(f).getOrElse(Nil)
  }
  implicit def enrichTraversableOption[A](option : Option[Traversable[A]]) =
    new RichTraversableOption(option)

  class RichTraversableOption[+A](option : Option[Traversable[A]]) {
    def contains[B >: A](value : B) = option.exists(_.exists(_ == value))
  }

  implicit def enrichPartialFunctionOption[From, To](option : Option[PartialFunction[From, To]]): PartialFunction[From, To] =
    option.getOrElse(Map[From, To]())

  implicit def enrichNumericOption[A : Numeric](option: Option[A]) = new RichNumericOption(option)

  implicit def enrichOptionalOption[A](option: Option[Option[A]]) = new RichOptionalOption(option)

  class RichOptionalOption[A](option: Option[Option[A]]) {
    def flatOpt(): Option[A] = option match {
      case Some(x) => x
      case None => None
    }
  }

  class RichNumericOption[A : Numeric](option: Option[A]) {
    private val num = new OptionNumeric[A]

    def +(rhs: Option[A]): Option[A] = num.plus(option, rhs)
    def *(rhs: Option[A]): Option[A] = num.times(option, rhs)
    def -(rhs: Option[A]): Option[A] = num.minus(option, rhs)
    val negate = num.negate(option)
  }

  class OptionNumeric[A : Numeric] extends Numeric[Option[A]] {
    private val num = implicitly[Numeric[A]]

    override def zero = None

    def toDouble(x: Option[A]) = throw new IllegalStateException("Can't convert Option[A] to double.")
    def toFloat(x: Option[A]) = throw new IllegalStateException("Can't convert Option[A] to float.")
    def toLong(x: Option[A]) = throw new IllegalStateException("Can't convert Option[A] to long.")
    def toInt(x: Option[A]) = throw new IllegalStateException("Can't convert Option[A] to int.")
    def fromInt(x: Int) = Some(num.fromInt(x))
    def negate(x: Option[A]): Option[A] = x.map(num.negate(_))
    def times(x: Option[A], y: Option[A]) = liftOp(x, y, num.times _)
    def minus(x: Option[A], y: Option[A]) = liftOp(x, y, num.minus _)
    def plus(x: Option[A], y: Option[A]) = liftOp(x, y, num.plus _)
    def compare(x: Option[A], y: Option[A]) = (x, y) match {
      case (_, None) => -1
      case (None, _) => 1
      case (Some(l), Some(r)) => num.compare(l, r)
    }

    private def liftOp(x: Option[A], y: Option[A], f: (A, A) => A) = (x, y) match {
      case (_, None) => x
      case (None, _) => y
      case (Some(l), Some(r)) => Some(f(l, r))
    }
  }

  implicit object NumericOptionDouble extends OptionNumeric[Double]
  implicit object NumericOptionInt extends OptionNumeric[Int]
  implicit object NumericOptionFloat extends OptionNumeric[Float]
}
