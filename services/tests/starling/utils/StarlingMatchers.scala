package starling.utils

import starling.auth.User
import org.mockito.Matchers._
import java.lang.String
import collection.mutable.ArraySeq

object StarlingMatchers {
  def anySubgroupName: String = {
    anyString
  }

  def anyUser: User = {
    any(classOf[User])
  }

  def anyHeader: Array[String] = {
    any(classOf[Array[String]])
  }

  def anyTrades: scala.List[ArraySeq[Object]] = {
    any(classOf[List[ArraySeq[Object]]])
  }
}

