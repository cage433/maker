package starling.services

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.WordSpec


class QlikViewUpdaterTests extends WordSpec with ShouldMatchers {
  val impalaName = "Reload of Impala.UI.qvw (UAT)"
  val titanName = "Reload of Titan.CostsAndIncomes.MTM-PNL.Model.qvw"

  val xml =
  <ArrayOfTaskInfo xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns="http://trafigura.com/qv/qmsb">
    <TaskInfo>
      <ExtensionData />
      <Enabled>true</Enabled>
      <ID>Impala ID</ID>
      <Name>{impalaName}</Name>
      <QDSID>Impala QDSID</QDSID>
      <Type>DocumentTask</Type>
    </TaskInfo>

    <TaskInfo>
      <ExtensionData />
      <Enabled>true</Enabled>
      <ID>Titan ID</ID>
      <Name>{titanName}</Name>
      <QDSID>Titan QDSID</QDSID>
      <Type>DocumentTask</Type>
    </TaskInfo>
  </ArrayOfTaskInfo>

  "Can find tasks ids by name" in {
    QlikViewUpdater.extractTaskID("Impala", xml) should be === "Impala ID"
    QlikViewUpdater.extractTaskID("Titan", xml) should be === "Titan ID"
  }

  "Produce nice error when no task found" in {
    intercept[Exception](QlikViewUpdater.extractTaskID("Baloney", xml)).getMessage should be ===
      "No task with name containing: 'Baloney', available:\n\t" + List(impalaName, titanName).mkString("\n\t") + suffix
  }

  "Produce nice error when more than one task found" in {
    intercept[Exception](QlikViewUpdater.extractTaskID("Reload", xml)).getMessage should be ===
      "2 tasks with name containing: 'Reload'\n\t" + List(impalaName, titanName).mkString("\n\t") + suffix
  }

  private val suffix = "\nEither change property: QlikViewSpotFXTask or set QlikViewEnabled=false"
}