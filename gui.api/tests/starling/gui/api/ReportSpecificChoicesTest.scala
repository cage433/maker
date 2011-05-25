package starling.gui.api

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test
import starling.utils.Describable

class ReportSpecificChoicesTest extends TestNGSuite with ShouldMatchers {
  @Test
  def validationShouldFailWhenKeyNotIncludedInOptions {
    val choices = new ReportSpecificChoices(Map("label" -> "value1"))

    choices.isSubsetOf(new ReportSpecificOptions(Nil)) should be ===
      Describable[Boolean](false, "ReportSpecificChoices contains invalid labels: 'label'")
  }

  @Test
  def validationShouldFailWhenValueNotIncludedInOptions {
    val choices = new ReportSpecificChoices(Map("label" -> "value1"))

    choices.isSubsetOf(new ReportSpecificOptions("label" -> List("value2") :: Nil)) should be ===
      Describable[Boolean](false, "ReportSpecificChoices contains invalid values: label -> value1")
  }

  @Test
  def canSupplyDefaultsFromReportSpecificOptions {
    val choices = new ReportSpecificChoices(Map("choice" -> "chosenValueShouldNotBeOverwritten"))
      .withDefaults(new ReportSpecificOptions(
        "choice" -> List("alreadyChosenValueShouldBeIgnored"),
        "option" -> List("firstValueIsDefault", "onlyFirstOptionShouldBeUsed"))
    )

    choices should be === new ReportSpecificChoices(Map(
      "choice" -> "chosenValueShouldNotBeOverwritten",
      "option" -> "firstValueIsDefault"))
  }
}