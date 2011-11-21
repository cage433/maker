package starling.webservice

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.WordSpec


class QlikViewUpdaterTests extends WordSpec with ShouldMatchers {
  val xml =
  <ArrayOfTaskInfo xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns="http://trafigura.com/qv/qmsb">
    <TaskInfo>
      <ExtensionData />
      <Enabled>true</Enabled>
      <ID>23596f40-6152-428c-9712-002d764d80a2</ID>
      <Name>Reload of Impala.UI.qvw (UAT)</Name>
      <QDSID>Impala ID</QDSID>
      <Type>DocumentTask</Type>
    </TaskInfo>

    <TaskInfo>
      <ExtensionData />
      <Enabled>true</Enabled>
      <ID>8feeabaa-8483-4b2a-a8c2-0041054883f0</ID>
      <Name>Reload of Titan.CostsAndIncomes.MTM-PNL.Model.qvw</Name>
      <QDSID>Titan ID</QDSID>
      <Type>DocumentTask</Type>
    </TaskInfo>
  </ArrayOfTaskInfo>

  "Can find tasks ids by name" in {
    QlikViewUpdater.taskID("Impala", xml) should be === Some("Impala ID")
    QlikViewUpdater.taskID("Titan", xml) should be === Some("Titan ID")
  }
}