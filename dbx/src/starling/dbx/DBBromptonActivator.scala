package starling.dbx

import starling.manager.{BromptonContext, BromptonActivator}
import com.jolbox.bonecp.BoneCPDataSource
import javax.sql.DataSource
import java.sql.DriverManager

class DBProps

object DataSourceFactory {
  private var dataSources = Map[(String, String, String), BoneCPDataSource]()

  def getDataSource(url : String, username : String, password : String): DataSource = {
    dataSources.synchronized {
      dataSources.get((url, username, password)) match {
        case Some(ds) => ds
        case None => {
          // http://www.mchange.com/projects/c3p0/index.html
          val config = new BoneCPDataSource
//          val cl = classOf[net.sourceforge.jtds.jdbc.Driver].getClassLoader
//          config.setClassLoader(cl)
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

  def shutdown = {
    dataSources.synchronized {
      dataSources.values.map(_.close)
      dataSources = Map[(String, String, String), BoneCPDataSource]()
    }
  }

  DriverManager.registerDriver(new net.sourceforge.jtds.jdbc.Driver())
  DriverManager.registerDriver(new oracle.jdbc.driver.OracleDriver())
//  try {
//    Class.forName("net.sourceforge.jtds.jdbc.Driver")
//    println("Loaded jtds")
//  } catch {
//    case e => {
//      println("Can't load jtds")
//      e.printStackTrace()
//    }
//  }
}

//class DBBromptonActivator extends BromptonActivator {
//
//  type Props = DBProps
//
//  def defaults = new DBProps
//
//  val datasourceFactory = new DataSourceFactory()
//
//  def init(context: BromptonContext, props: DBProps) = { }
//
//  def start(context: BromptonContext) = {
//    context.registerService(classOf[DataSourceFactory], datasourceFactory)
//  }
//
//  override def stop(context: BromptonContext) = {
//    datasourceFactory.shutdown
//  }
//}