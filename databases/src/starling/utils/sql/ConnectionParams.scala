package starling.utils.sql


import java.sql.DriverManager
import javax.sql.DataSource
import com.jolbox.bonecp.{BoneCPDataSource, BoneCPConfig}

case class ConnectionParams(url : String, dataSource: DataSource) {
  def this(url: String, username: String, password: String) {
    this(url, ConnectionParams.getDataSource(url, username, password))
  }

  if (url.endsWith("FOO")) {
    Thread.dumpStack
    println("Creating params " + url)
  }
}

object ConnectionParams {
  private var dataSources = Map[(String, String, String), BoneCPDataSource]()

  def getDataSource(url : String, username : String, password : String): DataSource = {
    dataSources.synchronized {
      dataSources.get((url, username, password)) match {
        case Some(ds) => ds
        case None => {
          // http://www.mchange.com/projects/c3p0/index.html
          val config = new BoneCPDataSource
          config.setPoolName("ConnectionPool")
          config.setLazyInit(true)

          config.setJdbcUrl(url)
          config.setUsername(username)
          config.setPassword(password)
          
          config.setStatementsCacheSize(200)

          config.setAcquireIncrement(5)
          config.setAcquireRetryAttempts(9999)
          config.setAcquireRetryDelayInMs(1500) // 1500 miliseconds wait before try to acquire connection again

          if(url.toLowerCase.contains("sqlserver")) {
            config.setConnectionTestStatement("select @@cpu_busy")
          } else if(url.toLowerCase.contains("oracle")) {
            config.setConnectionTestStatement("select TO_CHAR(sysdate, 'DD-MON-YYYY HH:MI:SS') FROM DUAL")
          }

          config.setConnectionTimeoutInMs(0) // 0 = wait indefinitely for new connection
          config.setIdleConnectionTestPeriodInSeconds(60) // test idle connection every 60 sec
          config.setIdleMaxAgeInMinutes(10)

          config.setPartitionCount(5)
          config.setMinConnectionsPerPartition(2)
          config.setMaxConnectionsPerPartition(5)

          dataSources += ((url, username, password) -> config)
          config
        }
      }
    }
  }

  def shutdown = dataSources.values.map(_.close)

  DriverManager.registerDriver(new oracle.jdbc.driver.OracleDriver())
  Class.forName("net.sourceforge.jtds.jdbc.Driver")
}