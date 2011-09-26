package starling.utils

import java.lang.reflect.{Method, InvocationHandler, Proxy}
import starling.utils.ImplicitConversions._
import starling.utils.ClosureUtil._
import scalaz.Scalaz._

trait DynamicProxyFactory {
  def proxy[T](handler: InvocationHandler)(implicit m: Manifest[T]) =
    Proxy.newProxyInstance(m.erasure.getClassLoader,Array(m.erasure), handler).asInstanceOf[T]
}

object VerifyingDynamicProxy extends DynamicProxyFactory with Log {
  def create[T: Manifest](canonical: T, candidate: T, throwFailures: Boolean = false): T =
    proxy[T](VerifyingInvocationHandler(canonical, candidate, throwFailures))
}

case class VerifyingInvocationHandler[T](canonical: T, candidate: T, throwFailures: Boolean) extends InvocationHandler with Log {
  def invoke(proxy: AnyRef, method: Method, args: Array[AnyRef]) = log.infoWithTime("Verifying: %s(%s)" % (method, formatArgs(args))) {
    val (canonicalResult, canonicalTime) = time { safely { method.invoke(canonical, args : _*) } }
    val (candidateResult, candidateTime) = time { safely { method.invoke(candidate, args : _*) } }

    if (candidateTime > canonicalTime) {
      Log.warn("Candidate time: %s slower than Canonical time: %s" %
        (Stopwatch.milliToHumanString(candidateTime), Stopwatch.milliToHumanString(canonicalTime)))
    } else {
      Log.info("Candidate time: %s faster than Canonical time: %s" %
        (Stopwatch.milliToHumanString(candidateTime), Stopwatch.milliToHumanString(canonicalTime)))
    }

    difference(canonicalResult, candidateResult).map {diff => {
      if (throwFailures) throw new Exception(message(method, args, canonicalResult, candidateResult, diff))
      else log.error(message(method, args, canonicalResult, candidateResult, diff))
    } }

    canonicalResult.left.map(_.getCause).getOrThrow
  }

  private def time[T](f: => T): (T, Long) = {
    val stopwatch = new Stopwatch()

    val result = f

    (result, stopwatch.ms())
  }

  def formatArgs(args: Array[AnyRef]): String = {
    (args ?? Array()).toList.map {
      arg => arg match {
        case l: List[_] => l.mkString(", ")
        case _ => arg
      }
    }.mkString("(", "), (", ")")
  }

  def mapDifference(canonicalMap: Map[Any, Any], candidateMap: Map[Any, Any]): List[(Any, Any)] = try {
    multiMapDifference(canonicalMap.asInstanceOf[Map[Any, Set[Any]]], candidateMap.asInstanceOf[Map[Any, Set[Any]]])
  } catch {
    case _ => (canonicalMap difference candidateMap).toList
  }

  def multiMapDifference(canonicalMultiMap: Map[Any, Set[Any]], candidateMultiMap: Map[Any, Set[Any]]): List[(Any, Set[Any])] = {
    canonicalMultiMap.zipMap(candidateMultiMap).mapValues(pair => pair._1 -- pair._2).filterValuesNot(_.isEmpty).toList
  }

  def difference(canonicalResult: Either[Throwable, AnyRef], candidateResult: Either[Throwable, AnyRef]): Option[String] = try {
    (canonicalResult, candidateResult) match {
      case (Right(canonicalList: List[Any]), Right(candidateList: List[Any])) => {
        val diff: List[Any] = canonicalList -- candidateList

        if (diff.isEmpty) None else Some(diff.mkString(", "))
      }
      case (Right(canonicalSet: Set[Any]), Right(candidateSet: Set[Any])) => {
        val diff: Set[Any] = canonicalSet -- candidateSet

        if (diff.isEmpty) None else Some(diff.mkString(", "))
      }
      case (Right(canonicalMap: Map[Any, Any]), Right(candidateMap: Map[Any, Any])) => {
        val diff: List[(Any, Any)] = mapDifference(canonicalMap, candidateMap)

        if (diff.isEmpty) None else Some(diff.mkString(", "))
      }
      case (Left(canonicalError), Left(candidateError)) => {
        if (canonicalError.getCause.getMessage == candidateError.getCause.getMessage) None else {
          Some("Canonical threw: %s, but candidate threw: %s" %
            (canonicalError.getCause.getMessage, candidateError.getCause.getMessage))
        }
      }
      case (Right(canonicalValue), Right(candidateValue)) => if (bothNull(canonicalValue, candidateValue) ||
        (neitherNull(canonicalValue, candidateValue) && canonicalValue.equals(candidateValue))) None else {

        Some("<unknown>")
      }
      case (Left(canonicalError), Right(_)) => {
        log.error("Canonical threw but candidate didn't", canonicalError)

        Some("<canonical throw: " + canonicalError.getMessage)
      }
      case (Right(_), Left(candidateError)) => {
        log.error("Candidate threw but canonical didn't", candidateError)

        Some("<candidate threw: " + candidateError.getMessage)
      }
      case _ => None
    }
  } catch {
    case e => { log.error("getDifference died", e); Some("<error getting difference>") }
  }

  private def sameResult(canonical: Either[Throwable, AnyRef], candidate: Either[Throwable, AnyRef]) = (canonical, candidate) match {
    case (Left(canonicalError), Left(candidateError)) => canonicalError.getCause.getMessage == candidateError.getCause.getMessage
    case (Right(canonicalResult), Right(candidateResult)) => bothNull(canonicalResult, candidateResult) ||
      (neitherNull(canonicalResult, candidateResult) && canonicalResult.equals(candidateResult))
    case _ => false
  }

  private def bothNull[T](canonical: T, candidate: T) = canonical == null && candidate == null
  private def neitherNull[T](canonical: T, candidate: T) = canonical != null && candidate != null

  def message(method: Method, args: Array[AnyRef], canonicalResult: Either[scala.Throwable, AnyRef],
              candidateResult: Either[scala.Throwable, AnyRef], diff: String): String = {
    "\nDiffering results for %s(%s) difference: %s\n\tcanonical: %s\n\tcandidate: %s" %
      (method, formatArgs(args), diff, canonicalResult, candidateResult)
  }
}