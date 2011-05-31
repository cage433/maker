package starling.http

import javax.servlet.http.HttpServlet
import javax.servlet.ServletConfig
import starling.services.StarlingInit
import java.io.File
import starling.props.{PropsHelper, Props}
import java.lang.String


class JBossStarlingServlet extends HttpServlet{

  import JBossStarlingServlet._

  override def destroy {
    starling match {
      case Some(stg) => {
        println("**** CAlling stop on Starling")
        stg.stop
        println("**** Finished CAlling stop on Starling")
        starling = None
      }
      case _ =>
    }
  }

  override def init(config: ServletConfig) {
    starling = Some(new StarlingInit(
      props,
      // Migration requires reflection which doesn't currently work in a War.
      dbMigration = false,
      startXLLoop = false,
      startStarlingJMX = false,
      startRMI = true,
      startHttp = false,
      forceGUICompatability = true,
      startEAIAutoImportThread = false,
      runningInJBoss = true).start
    )

  }
}

object JBossStarlingServlet{

  var starling : Option[StarlingInit] = None

  val trafProperties = PropsHelper.propsFromFile(new File(System.getenv("HOME") + "/trafigura.properties")) ++ PropsHelper.propsFromFile(new File(System.getenv("JBOSS_HOME") + "/trafigura.properties"))

  val defaultEnvironment: String = "users/alex.mcguire"
  val databases = {
    Map[String, String](
      defaultEnvironment -> "jdbc:jtds:sqlserver://TTRAFLOCOSQL08.global.trafigura.com/starling_AlexM1;instance=DB08 starling ng1lr4ts123!Y^%&$",
      "integration-test" -> "jdbc:jtds:sqlserver://TTRAFLOCOSQL08.global.trafigura.com/starling_ThomasR1;instance=DB08 starling ng1lr4ts123!Y^%&$",
      "pre-prod" -> "jdbc:jtds:sqlserver://TTRAFLOCOSQL08.global.trafigura.com/starling_AlexM1;instance=DB08 starling ng1lr4ts123!Y^%&$",
      "prod" -> "jdbc:jtds:sqlserver://TTRAFLOCOSQL08.global.trafigura.com/starling_AlexM1;instance=DB08 starling ng1lr4ts123!Y^%&$"
    )
  }
  val jbossEnv = trafProperties.getOrElse("trafigura.env", "users/alex.mcguire")
  val jbossStarlingDB = databases.getOrElse(jbossEnv, databases(defaultEnvironment))

  val props = new Props(Map[String, String](
    "StarlingDatabase" -> jbossStarlingDB,
    "ServerName" -> jbossEnv,
    "RmiPort" -> "31415"
  ))
}
