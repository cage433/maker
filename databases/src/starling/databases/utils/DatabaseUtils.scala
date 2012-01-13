package starling.databases.utils

import starling.db.DB
import starling.dbx.DataSourceFactory
import starling.utils.Log


object DatabaseUtils {
  private val database = "jdbc:jtds:sqlserver://TTRAFLOCOSQL08.global.trafigura.com;instance=DB08"
  private val user = "starling"
  private val password = "ng1lr4ts123!Y^%&$"

  def refreshDatabase(fromDatabase:String, toDatabaseName:String) {
    println("")
    println("Refreshing " + fromDatabase + " => " + toDatabaseName)
    println("")
    val dataSource = DataSourceFactory.getDataSource(database, user, password)
    val starlingDB = new DB(dataSource)
    val sourceServer = if (fromDatabase == "starling_eai_prod") "'ttraflonsql12\\db12'" else "'ttraflocosql08\\db08'"
    // Copy the database from the last backup.
    starlingDB.inTransaction {
      writer => writer.update(
        "EXEC SQLAdmin.SelfServe.RefreshDB " +
          "@SourceServer = " + sourceServer + ", " +
          "@SourceDatabase = '" + fromDatabase + "', " +
          "@TargetDatabase = '" + toDatabaseName + "'"
        )
    }
    // Poll until the database refresh has finished.
    var refreshed = false
    while (!refreshed) {
      Thread.sleep(1000)
      starlingDB.query("EXEC SQLAdmin.SelfServe.ViewTaskHistory 'RefreshDB', @DetailedHist = 1", Map()) {
        rs => {
          if (!rs.getBoolean("RunNow")) {
            if (!rs.getString("LastOutcome").equalsIgnoreCase("SUCCESS")) {
              throw new Exception("The database didn't refresh properly")
            }
            refreshed = true
          }
        }
      }
    }
    println("Refresh of " + fromDatabase + " => " + toDatabaseName + " complete")
  }

  def backupDatabase(backupName:String) {
    println("")
    println("Backing up " + backupName)
    println("")
    val dataSource = DataSourceFactory.getDataSource(database, user, password)
    val starlingDB = new DB(dataSource)
    // Copy the database from the last backup.
    starlingDB.inTransaction {
      writer => writer.update("EXEC SQLAdmin.SelfServe.MarkTaskForExecution BackupDatabase, '\n" +
              "<ROOT>\n" +
              "<Param Name=\"Param1\" Value=\"" + backupName + "\"/>\n" +
              "</ROOT>'")
    }
    // Poll until the database refresh has finished.
    var refreshed = false
    while (!refreshed) {
      Thread.sleep(1000)
      starlingDB.query("EXEC SQLAdmin.SelfServe.ViewTaskHistory 'BackupDatabase', @DetailedHist = 1", Map()) {
        rs => {
          if (!rs.getBoolean("RunNow")) {
            if (!rs.getString("LastOutcome").equalsIgnoreCase("SUCCESS")) {
              throw new Exception("The database didn't backup properly")
            }
            refreshed = true
          }
        }
      }
    }
    println("Backup of " + backupName + " complete")
  }
}

object RefreshDatabase {
  System.setProperty("log4j.configuration", "utils/resources/log4j.properties")
  def main(args:Array[String]) {
    val from = "starling_eai_prod"
    val to = "starling_thomasr2"
    Log.infoWithTime("Backup") { DatabaseUtils.backupDatabase(from) }
    Log.infoWithTime("Copy") { DatabaseUtils.refreshDatabase(from, to) }
  }
}
