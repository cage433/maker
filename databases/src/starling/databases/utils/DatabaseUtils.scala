package starling.databases.utils

import starling.dbx.ConnectionParams
import starling.db.DB
import starling.dbx.DataSourceFactory


object DatabaseUtils {
  private val DefaultDatabase = "jdbc:jtds:sqlserver://TTRAFLOCOSQL08.global.trafigura.com;instance=DB08"
  private val DefaultUser = "starling"
  private val DefaultPass = "ng1lr4ts123!Y^%&$"

  private val DefaultBackupName = "starling_test"

  def refreshDatabase(databaseName:String, database:String = DefaultDatabase, user:String = DefaultUser, pass:String = DefaultPass) {
    println("")
    println("Refreshing " + databaseName)
    println("")
    val dataSource = DataSourceFactory.getDataSource(database, user, pass)
    val starlingDB = new DB(dataSource)
    // Copy the database from the last backup.
    starlingDB.inTransaction {
      writer => writer.update("EXEC SQLAdmin.SelfServe.RefreshDB @TargetDatabase = '" + databaseName + "'")
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
    println("Refresh of " + databaseName + " complete")
  }

  def backupDatabase(backupName:String = DefaultBackupName, database:String = DefaultDatabase, user:String = DefaultUser, pass:String = DefaultPass) {
    println("")
    println("Backing up " + backupName)
    println("")
    val dataSource = DataSourceFactory.getDataSource(database, user, pass)
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
  def main(args:Array[String]) {
    if ((args.size == 0) || ((args.size > 1) && (args.size != 4))) {
      println("Please specify either just the name of the database you want to refresh, or the 4 parameters shown below")
      println("Usage: name database user pass")
      return
    }
    if (args.size == 1) {
      DatabaseUtils.refreshDatabase(args(0))
    } else {
      DatabaseUtils.refreshDatabase(args(0), args(1), args(2), args(3))
    }
  }
}