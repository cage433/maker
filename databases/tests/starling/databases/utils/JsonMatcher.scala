package starling.databases.utils

import collection.immutable.Map
import org.scalatest.matchers.{MatchResult, Matcher}
import java.lang.String
import starling.utils.ImplicitConversions

object JsonMatcher {
  def matchJSON(entries: (String, Object)*) : JsonMatcher = new JsonMatcher((Map(entries : _*)))
}

class JsonMatcher(entries : Map[String, Object]) extends Matcher[scala.Predef.String] {
  def apply(json: String) = {
    val trimmedJson = json.replace(" : ", ":")

    case class Match(key : String, value : Object) {
      import ImplicitConversions._

      val searchFor = value match {
        case s : String =>        "\"%s\":\"%s\"" % (key, s)
        case a : Array[String] => "\"%s\":%s" % (key, a.mkString("[\"", "\",\"", "\"]"))
        case a : Array[Double] => "\"%s\":%s" % (key, a.mkString("[\"", "\",\"", "\"]"))
        case m : Array[Array[Double]] => "\"%s\":%s" % (key, m.map(_.mkString("[\"", "\",\"", "\"]")).mkString("[", ",", "]"))
      }

      val result = trimmedJson.contains(searchFor)
    }

    val result = entries.toList.map(pair => Match(pair._1, pair._2))
    val failures = result.filterNot(_.result).map(_.searchFor)
    val failureMessage = json + " does not contain " + failures

    MatchResult(result.forall(_.result), failureMessage, failureMessage, failureMessage, failureMessage)
  }
}
