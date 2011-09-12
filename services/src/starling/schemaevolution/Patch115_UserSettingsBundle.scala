package starling.schemaevolution

import system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter
import starling.browser.internal.UserSettings
import starling.utils.StarlingXStream

class Patch115_UserSettingsBundle extends Patch {

  val convertingXStream = StarlingXStream.createXStream

  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.update("alter TABLE UserSettings add bundle varchar(128)")
    writer.update("alter TABLE UserSettings add description varchar(128)")

//    val allSettings = writer.queryWithNoResults("select starlinguser, settings from usersettings") {
//      rs => {
//        val user = rs.getString("starlinguser")
//        val settings = rs.getString("settings")
//        val userSettings = convertingXStream.fromXML(settings).asInstanceOf[UserSettings]
//        user -> userSettings
//      }
//    }
    writer.update("truncate table usersettings")
//    allSettings.foreach { case (user, settings) => {
//      settings.
//    } }
  }
}