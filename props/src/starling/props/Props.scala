package starling.props

import java.net.InetAddress
import starling.props.PropsHelper._
import java.io.File
import scalaz.Scalaz._
import starling.api.utils.PropertyValue


class Props(starlingProps:Map[String,PropertyValue], trafiguraProps : Map[String, PropertyValue]) extends PropsHelper(starlingProps, trafiguraProps) {
  object ServerName extends ServerNameStringProperty()
  object ServerType extends EnumProperty(ServerTypeLabel.instances.map(_.name) : _*){
    def apply() : ServerTypeLabel = {
      super.validateValue()
      ServerTypeLabel.fromName(value)
    }
  }
  object ServerNameOrBlank extends StringProperty(ServerName())
  object ServerColour extends StringProperty(PropsHelper.createColourString(ServerName()))
  object UseProductionColour extends BooleanProperty(false)

  object Production extends BooleanProperty(false)

  object ReadonlyMode extends BooleanProperty(false)
  object VarReportEmailFrom extends StringProperty("var-reports-test@trafigura.com")

  object FreightEmailAddress extends EmailProperty
  object MetalsEmailAddress extends EmailProperty
  object OilEmailAddress extends EmailProperty
  object AzuriteEmailAddress extends EmailProperty
  object GalenaEmailAddress extends EmailProperty
  object MalachiteEmailAddress extends EmailProperty
  object GMFEmailAddress extends EmailProperty
  object GSAEmailAddress extends EmailProperty
  object WuXiEmailAddress extends EmailProperty
  object LimEmailAddress extends EmailProperty

  object EnabledDesks extends StringProperty("")
  object EnableVerificationEmails extends BooleanProperty(true)

  object SmtpServerHost extends StringProperty("londonsmtp.mail.trafigura.com")
  object SmtpServerPort extends IntProperty(25)
  object HttpPort extends LocalPort(1024 + ((ServerName().hashCode.abs % 6400) * 10) + 0)
  object RmiPort extends LocalPort(1024 + ((ServerName().hashCode.abs % 6400) * 10) + 1)
  object XLLoopPort extends LocalPort(1024 + ((ServerName().hashCode.abs % 6400) * 10) + 2)
  object JmxPort extends LocalPort(1024 + ((ServerName().hashCode.abs % 6400) * 10) + 3) //used by start.sh
  object RegressionPort extends LocalPort(1024 + ((ServerName().hashCode.abs % 6400) * 10) + 4)
  object LoopyXLPort extends LocalPort(1024 + ((ServerName().hashCode.abs % 6400) * 10) + 5)
  object HttpServicePort extends LocalPort(1024 + ((ServerName().hashCode.abs % 6400) * 10) + 6)
  object StarlingServiceRmiPort extends LocalPort(1024 + ((ServerName().hashCode.abs % 6400) * 10) + 7)

  object ExternalHostname extends StringProperty(InetAddress.getLocalHost.getHostName) {
    override def validate(value: String) = Set("localhost", "127.0.0.1").contains(value.trim.toLowerCase).option("is not valid")
  }

  object ExternalUrl extends StringProperty("http://" + ExternalHostname() + ".global.trafigura.com:" + HttpPort())
  object HttpServiceExternalUrl extends StringProperty("http://" + ExternalHostname() + ":" + HttpServicePort())
  object XLLoopUrl extends StringProperty(ExternalHostname() + ":" + XLLoopPort())

  object RabbitEnabled extends BooleanProperty(true)
  object RabbitHost extends StringProperty("")
  def rabbitHostSet = RabbitHost() != ""

  object StarlingDatabase extends DatabaseProperty("jdbc:jtds:sqlserver://TTRAFLOCOSQL08.global.trafigura.com/starling_test;instance=DB08", "starling", "ng1lr4ts123!Y^%&$")
  object FCASqlServer extends DatabaseProperty("jdbc:jtds:sqlserver://forwardcurves.dev.sql.trafigura.com:6321;databaseName=ForwardCurves", "curveUser", "jha12wsii")
//  object FCASqlServer extends DatabaseProperty("jdbc:jtds:sqlserver://TTRAFLONSQL12.global.trafigura.com/ForwardCurves;instance=DB12", "starling", "ng1lr4ts123!Y^%&$")
//  object FCASqlServer extends DatabaseProperty("jdbc:jtds:sqlserver://TTRAFLONSQL02.global.trafigura.com/ForwardCurves;instance=DB02", "curveUser", "jha12wsii")

  /**
   * EAI Starling is a copy of EAI Archive with some changes to allow versioning. It contains trades and book closes for Books 43 and 173
   */
  object EAIStarlingSqlServer extends DatabaseProperty("jdbc:jtds:sqlserver://TTRAFLONSQL12.global.trafigura.com/EAIStarling;instance=DB12", "starling", "ng1lr4ts123!Y^%&$")
  //  object EAIArchiveSqlServer extends DatabaseProperty("jdbc:jtds:sqlserver://TTRAFLONSQL02.global.trafigura.com/EAIArchive;instance=DB02", "starling", "ng1lr4ts123!Y^%&$")
  
  object TrinityDatabase extends DatabaseProperty("jdbc:oracle:thin:@LondonTrinityLiveDB.global.trafigura.com:1521:Trinity", "EXEC_IMP", "EXEC_IMP")
  object TrinityUploadDirectory extends StringProperty("/tmp/starling/trinity-upload")
  object TrinityServiceUrl extends StringProperty("http://ttraflon2k196:9100")
  object EAIReplica extends DatabaseProperty("jdbc:jtds:sqlserver://TTRAFLONSQL12.global.trafigura.com/EAI;instance=DB12", "starling", "ng1lr4ts123!Y^%&$")
  object SoftmarDatabase extends DatabaseProperty("jdbc:jtds:sqlserver://TTRAFLONSQL01.global.trafigura.com/Softmar;instance=DB01", "starling", "ng1lr4ts123!Y^%&$")

  object GalenaDatabase extends DatabaseProperty("jdbc:oracle:thin:@//galenaprdcl01:1521/trinitygalenastarlingprd01.global.trafigura.com", "EXEC_IMP", "EXEC_IMP")

  // VarSqlServer refers to a database engine - not a database. All uses of it then specify database and table.
  object VarSqlServer extends DatabaseProperty("jdbc:jtds:sqlserver://TTRAFLONSQL12.global.trafigura.com/;instance=DB12", "starling", "ng1lr4ts123!Y^%&$")

  object DeltaCSVDir extends StringProperty(".")

  object KerberosPassword extends StringProperty("suvmerinWiv0")
  object ServerPrincipalName extends StringProperty("STARLING-TEST/dave-linux")
  object UseAuth extends BooleanProperty(true)
  object NoMP extends BooleanProperty(false)
  object ImportsBookClosesFromEAI extends BooleanProperty(true)
  object ImportMarketDataAutomatically extends BooleanProperty(true)

  object LIMHost extends StringProperty("lim-london-live")
  object LIMPort extends IntProperty(6400)

  object NeptuneDatabase extends DatabaseProperty(
    "jdbc:jtds:sqlserver://TTRAFLONSQL12.global.trafigura.com/;instance=db12",
    "starling_neptune",
    "1234142dfSdfS&%&^%£)"
   )

  object QlikViewServerUrl extends StringProperty("http://qvdev:8080/qmsb/Task.asmx")
  object QlikViewSpotFXTask extends StringProperty("Reload of Spot FX")

  // Titan related configuration
  object ServiceInternalAdminUser extends StringProperty("refined.metalsadm") // admin user for service to service access (permission requirements here for service calls TBD)
  object EdmTradeServiceLocation extends StringProperty("http://localhost:8080/tradeservice")
  object RefDataServiceLocation extends StringProperty("http://localhost:8080/referencedata")
  object LogisticsServiceLocation extends StringProperty("http://localhost:8080/logistics")

  private def RestEasyRpcMount = "/RPC"
  object EdmTradeServiceUrl extends StringProperty(EdmTradeServiceLocation() + RestEasyRpcMount)
  object TacticalRefDataServiceUrl extends StringProperty(RefDataServiceLocation() + RestEasyRpcMount)
  object TitanLogisticsServiceUrl extends StringProperty(LogisticsServiceLocation() + RestEasyRpcMount)

  // Titan Rabbit related configuration
  object TitanRabbitBrokerHost  extends StringProperty("")
  object TitanRabbitUserName  extends StringProperty("trafiguraDev")
  object TitanRabbitPassword  extends StringProperty("trafiguraDev")

  def titanRabbitHostSet = TitanRabbitBrokerHost() != ""
}

object Props {
  type PropAccessor = (Props) => PropsHelper#Property

  def main(args:Array[String]) {
    PropsHelper.main(args)
  }
}
