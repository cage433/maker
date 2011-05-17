package starling.db

import org.scalatest.matchers.{MatchResult, Matcher}
import starling.utils.ImplicitConversions._


object MapMatcher {
  def containEntries[K, V](entries: (K, V)*): MapMatcher[K, V] = new MapMatcher((Map(entries : _*)))
}

class MapMatcher[K, V](entries : Map[K, V]) extends Matcher[scala.collection.Map[K, V]] {
  def apply(map: scala.collection.Map[K, V]) = {
    val missingEntries = entries.filter { entry => !map.exists(_ == entry) }

    val failureMessage = "Could not find:\n" + missingEntries.map(typedString).mkString("\t", "\n\t", "\n") +
      "in:\n" + map.map(typedString).mkString("\t", "\n\t", "\n")

    MatchResult(missingEntries.isEmpty, failureMessage, failureMessage, failureMessage, failureMessage)
  }

  private def typedString[K,V](pair : (K, V)) = pair match {
    case (k:AnyRef, v:AnyRef) => ("[%s, %s] " % (k.getClass.getSimpleName, v.getClass.getSimpleName)) + pairString(pair)
    case _ => pairString(pair)
  }

  private def pairString[K,V](pair : (K, V)) = "%s -> %s" % (pair._1, pair._2)
}