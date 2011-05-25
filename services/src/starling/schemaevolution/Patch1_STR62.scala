package starling.schemaevolution

import system.{PatchUtils, Patch}
import starling.db.DBWriter
import starling.richdb.{RichDB}
import starling.services.StarlingInit

class Patch1_STR62 extends Patch {

  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    PatchUtils.executeUpdateSQLFileFromClasspath(writer, "/schemaevolution/Patch1_STR62_AlterVarReportHeaderAndVarReportToAllowNulls.sql")
  }


  override protected def checkRunPatchCompletedSuccessfully(starling: RichDB) = {

    //Check the VarReportHeader table
    PatchUtils.assertColumnAllowsNulls(starling, "VarReportHeader", "snapshotId")

    //Check that the trade attribute columns now do not allow nulls
    PatchUtils.assertColumnDoesNotAllowNulls(starling, "VarReport", "commodity")
    PatchUtils.assertColumnDoesNotAllowNulls(starling, "VarReport", "desk")
    PatchUtils.assertColumnDoesNotAllowNulls(starling, "VarReport", "groupCompany")
    PatchUtils.assertColumnDoesNotAllowNulls(starling, "VarReport", "location")
    PatchUtils.assertColumnDoesNotAllowNulls(starling, "VarReport", "portfolio")
    PatchUtils.assertColumnDoesNotAllowNulls(starling, "VarReport", "strategy")
    PatchUtils.assertColumnDoesNotAllowNulls(starling, "VarReport", "trader")

    //Check that the value columns now do allow nulls
    PatchUtils.assertColumnAllowsNulls(starling, "VarReport", "VaR95")
    PatchUtils.assertColumnAllowsNulls(starling, "VarReport", "VaR95UOM")
    PatchUtils.assertColumnAllowsNulls(starling, "VarReport", "CVaR95")
    PatchUtils.assertColumnAllowsNulls(starling, "VarReport", "CVaR95UOM")
    PatchUtils.assertColumnAllowsNulls(starling, "VarReport", "VaR99")
    PatchUtils.assertColumnAllowsNulls(starling, "VarReport", "VaR99UOM")
    PatchUtils.assertColumnAllowsNulls(starling, "VarReport", "CVaR99")
    PatchUtils.assertColumnAllowsNulls(starling, "VarReport", "CVaR99UOM")
    PatchUtils.assertColumnAllowsNulls(starling, "VarReport", "standardErrors95")
    PatchUtils.assertColumnAllowsNulls(starling, "VarReport", "standardErrors95UOM")
    PatchUtils.assertColumnAllowsNulls(starling, "VarReport", "netPosition")
    PatchUtils.assertColumnAllowsNulls(starling, "VarReport", "netPositionUOM")
  }
}