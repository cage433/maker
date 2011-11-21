package starling.webservice

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.WordSpec


class QlikViewUpdaterTests extends WordSpec with ShouldMatchers {
  val xml =
  <ArrayOfTaskInfo xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns="http://trafigura.com/qv/qmsb">
    <TaskInfo>
      <ExtensionData />
      <Enabled>true</Enabled>
      <ID>Impala ID</ID>
      <Name>Reload of Impala.UI.qvw (UAT)</Name>
      <QDSID>Impala QDSID</QDSID>
      <Type>DocumentTask</Type>
    </TaskInfo>

    <TaskInfo>
      <ExtensionData />
      <Enabled>true</Enabled>
      <ID>Titan ID</ID>
      <Name>Reload of Titan.CostsAndIncomes.MTM-PNL.Model.qvw</Name>
      <QDSID>Titan QDSID</QDSID>
      <Type>DocumentTask</Type>
    </TaskInfo>
  </ArrayOfTaskInfo>

  "Can find tasks ids by name" in {
    QlikViewUpdater.taskID("Impala", xml) should be === Some("Impala ID")
    QlikViewUpdater.taskID("Titan", xml) should be === Some("Titan ID")
  }
}