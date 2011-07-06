package starling.utils

import org.testng.annotations.{BeforeTest, Test}
import org.mockito.Mockito._
import org.scalatest.testng.TestNGSuite
import collection.immutable.{List, Map}
import starling.auth.User
import org.scalatest.matchers.ShouldMatchers
import JsonMatcher._
import swing.event.Event
import starling.gui.api._
import starling.daterange._

class RabbitBroadcasterTest extends TestNGSuite with ShouldMatchers {
  lazy val broadcaster = new RabbitBroadcaster(sender)

  // Collaborators
  var sender : RabbitMessageSender = _

  @BeforeTest
  def initialiseCollaborators {
    sender = mock(classOf[RabbitMessageSender])
  }

  @Test
  def shouldSendCSVMessageToRabbitMessageSender {
    val data = List(List("1", "2"), List("3", "4"))

    val user: User = User("<userName>", "<name>")
    val update: BlotterTradeUpdate = BlotterTradeUpdate(user, "<subGroupName>", data)
    broadcaster.broadcast(update)

    val expectedHeader = Map("userName" -> "<userName>", "subGroupName" -> "<subGroupName>")
    val expectedPayload = """{"userName" : "<userName>", "subGroupName" : "<subGroupName>", "data" : [["1", "2"], ["3", "4"]]}"""

    verify(sender).send(update.queueName, RabbitMessage(expectedPayload, expectedHeader))
  }

  @Test
  def shouldIgnoreNonRabbitEvents {
    case object NonRabbitEvent extends Event

    broadcaster.broadcast(NonRabbitEvent)
  }
}

class BlotterTradeUpdateTest extends TestNGSuite with ShouldMatchers {
  @Test
  def shouldBeConvertibleToJSON {
    val update = BlotterTradeUpdate(User("<userName>", "<name>"), "<subGroupName>", List(List("1", "2"), List("3", "4")))

    update.toJSON should equal(
      """{"userName" : "<userName>", "subGroupName" : "<subGroupName>", "data" : [["1", "2"], ["3", "4"]]}""")
  }
}


class UploadPricesUpdateTest extends TestNGSuite with ShouldMatchers {
  import starling.daterange.Day._

  @Test
  def shouldBeConvertibleToJSON {
    val update = UploadPricesUpdate(User("<userName>", "<name>"), "<label>",
      ObservationPoint(21 Jan 2011, ObservationTimeOfDay.Default), Array(1 Jan 2011, 2 Jan 2011), "<market>", Array(123.45, 456.12))

    update.toJSON should matchJSON(
      "userName" -> "<userName>",
      "label" -> "<label>",
      "observationDate" -> "21 Jan 2011",
      "marketName" -> "<market>",
      "dates" -> Array("01Jan2011","02Jan2011"),
      "prices" -> Array(123.45, 456.12)
    )
  }
}



class UploadStandardDeviationsUpdateTest extends TestNGSuite with ShouldMatchers {
  import starling.daterange.Day._

  @Test
  def shouldBeConvertibleToJSON {
    val standardDeviations: Array[Array[Double]] = Array(Array(0.5, 0, 1), Array(2, 2, 2))

    val update = UploadStandardDeviationsUpdate(
      User("<userName>", "<name>"), "<label>", Some(21 Jan 2011), Array(Spread(Month(2011, 1), Month(2011, 2)), Spread(Month(2011, 2), Month(2011, 3))),
      "<market>", standardDeviations)

    update.toJSON should matchJSON(
      "userName" -> "<userName>",
      "label" -> "<label>",
      "observationDate" -> "21 Jan 2011",
      "marketName" -> "<market>",
      "dates" -> Array("JANUARY 2011\\/FEBRUARY 2011","FEBRUARY 2011\\/MARCH 2011"),
      "standardDeviations" -> standardDeviations
    )
  }
}

class UploadVolsUpdateTest extends TestNGSuite with ShouldMatchers {
  import starling.daterange.Day._

  @Test
  def shouldBeConvertibleToJSON {
    val vols: Array[Array[Double]] = Array(Array(0.5, 0, 1), Array(2, 2, 2))

    val update = UploadVolsUpdate(
      User("<userName>", "<name>"), "<label>", Some(21 Jan 2011), Array(1 Jan 2011, 2 Jan 2011),
      "<market>", vols)

    update.toJSON should matchJSON(
      "userName" -> "<userName>",
      "label" -> "<label>",
      "observationDate" -> "21 Jan 2011",
      "marketName" -> "<market>",
      "dates" -> Array("01Jan2011","02Jan2011"),
      "vols" -> vols
    )
  }
}

class UploadInterestRatesUpdateTest extends TestNGSuite with ShouldMatchers {
  import starling.daterange.Day._

  @Test
  def shouldBeConvertibleToJSON {
    val standardDeviations: Array[Array[Double]] = Array(Array(0.5, 0, 1), Array(2, 2, 2))

    val update = UploadInterestRatesUpdate(
      User("<userName>", "<name>"), "<label>", Some(21 Jan 2011), Array(1 Jan 2011, 2 Jan 2011),
      "<currency>", Array(123.45, 456.12))

    update.toJSON should matchJSON(
      "userName" -> "<userName>",
      "label" -> "<label>",
      "observationDate" -> "21 Jan 2011",
      "currency" -> "<currency>",
      "dates" -> Array("01Jan2011","02Jan2011"),
      "interestRates" -> Array(123.45, 456.12)
    )
  }
}