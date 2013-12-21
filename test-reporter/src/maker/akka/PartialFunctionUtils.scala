package maker.akka

object PartialFunctionUtils{
  def withExceptionsToStdOut[A, B](pf : PartialFunction[A, B]) : PartialFunction[A, B] = {
    case a : A  if (pf.isDefinedAt(a)) => {
      try {
        pf(a)
      } catch {
        case t : Throwable =>
          t.printStackTrace
          throw t
      }
    }
  }
}
  
