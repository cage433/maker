import sbt._
import sbt.Process._
import java.io.File


trait Repositories extends BasicScalaProject {

  val nexusGroupsRep = "Nexus-Groups" at "http://nexus.global.trafigura.com:8081/nexus/content/groups/mirror/"                                          
  val nexusPublicRep = "Nexus-Public" at "http://nexus.global.trafigura.com:8081/nexus/content/repositories/public/"
  val nexusThirdPartyRep = "Nexus-ThirdParty" at "http://nexus.global.trafigura.com:8081/nexus/content/repositories/thirdparty/"

  // To add a repository to publish to, uncomment and modify these lines
  //override def managedStyle = ManagedStyle.Maven
  //val publishTo = "Nexus-Groups" at "http://nexus.global.trafigura.com:8081/nexus/content/groups/mirror/"
  // Add credentials here
  //Credentials.add("Sonatype Nexus Repository Manager", "nexus.global.trafigura.com:8081", "admin", "admin123")

}


trait CoreDependencies extends Repositories {
  // Dependencies
  override def libraryDependencies = Set(

    // Compile dependencies
    //"org.scala-lang" % "scala-library" % "2.8.1",
    //"org.scala-lang" % "scala-compiler" % "2.8.1" % "provided",

    "org.scalatest" % "scalatest" % "1.3",

    "log4j" % "log4j" % "1.2.16" % "provided",
    "javax.servlet" % "servlet-api" % "2.5" % "provided",
    "org.slf4j" % "slf4j-simple" % "1.5.10" % "provided",

    "joda-time" % "joda-time" % "1.6",
    "org.codehaus.jettison" % "jettison" % "1.1",

    "commons-lang" % "commons-lang" % "2.5",
    "commons-codec" % "commons-codec" % "1.4",
    "commons-io" % "commons-io" % "1.2",
    "org.apache.commons" % "commons-math" % "2.1",

    "org.hibernate" % "hibernate-core" % "3.3.2.GA",
    "org.hibernate" % "hibernate-annotations" % "3.4.0.GA",
    "org.hibernate" % "hibernate-entitymanager" % "3.5.1-Final",
    "org.hibernate" % "hibernate-validator" % "3.1.0.GA",
    "org.hibernate" % "hibernate-search" % "3.2.0.CR1",

    "org.springframework" % "spring-web" % "3.0.5.RELEASE",
    "org.springframework" % "spring-orm" % "3.0.5.RELEASE",
    "org.springframework" % "spring-context" % "3.0.0.RELEASE",
    "org.springframework" % "spring-context-support" % "3.0.5.RELEASE",
    "org.springframework" % "spring-tx" % "3.0.5.RELEASE",
    "org.springframework" % "spring-test" % "3.0.5.RELEASE",

    "org.springframework.ldap" % "spring-ldap-core" % "1.3.0.RELEASE",
    "org.springframework.security" % "spring-security-core" % "3.0.5.RELEASE",
    "org.springframework.security" % "spring-security-config" % "3.0.5.RELEASE",
    "org.springframework.security" % "spring-security-web" % "3.0.5.RELEASE",

    "org.jboss.resteasy" % "resteasy-jaxrs" % "1.2.GA",
    "org.jboss.resteasy" % "resteasy-spring" % "1.2.GA",

    "com.oracle" % "ojdbc6" % "11.2.0.1.0",
    "com.oracle" % "ucp" % "11.2.0.1.0",
    "com.oracle" % "ons" % "10.2.0.3",
    "commons-dbcp" % "commons-dbcp" % "1.4",


    "org.dbunit" % "dbunit" % "2.4.8",
    "org.testng" % "testng" % "5.8" classifier "jdk15",


    // Test dependencies
    "org.mockito" % "mockito-all" % "1.8.4" % "test",
    "org.apache.derby" % "derby" % "10.4.2.0" % "test",
    "ch.qos.logback" % "logback-classic" % "0.9.17" % "test",
    "junit" % "junit" % "4.4" % "test"

  ) ++ super.libraryDependencies
 
}


// Model

trait ModelDependencies extends BasicScalaProject {
  override def libraryDependencies = Set(
    "com.rabbitmq" % "amqp-client" % "1.7.0.3",
    "com.rabbitmq" % "rabbitmq-messagepatterns" % "0.1.3" intransitive()
  ) ++ super.libraryDependencies
}


// Titan core

trait TitanDependencies extends BasicScalaProject {
  override def libraryDependencies = Set(
    "com.rabbitmq" % "amqp-client" % "1.7.2",
    "com.rabbitmq" % "rabbitmq-messagepatterns" % "0.1.3",
    "net.sf.ehcache" % "ehcache-core" % "2.3.0"
  ) ++ super.libraryDependencies
}


trait TitanSecurityDependencies extends TitanDependencies {
  override def libraryDependencies = Set(
  ) ++ super.libraryDependencies
}



// Services

trait ConfigurationDependencies extends BasicScalaProject {
  override def libraryDependencies = Set(
  ) ++ super.libraryDependencies
}


trait LogisticsDependencies extends BasicScalaProject {
  override def libraryDependencies = Set(
	"org.jboss.resteasy" % "resteasy-multipart-provider" % "1.2.GA"
  ) ++ super.libraryDependencies
}


trait CostsIncomesDependencies extends BasicScalaProject {
  override def libraryDependencies = Set(
    "commons-dbutils" % "commons-dbutils" % "1.3",
    "com.microsoft.sqlserver" % "sqljdbc4" % "2.0" % "test",
    "com.github.joel1di1" % "spring-jndi-initializer" % "1.0.1" % "test"
  ) ++ super.libraryDependencies
}


trait MtmpnlDependencies extends BasicScalaProject {
  override def libraryDependencies = Set(

  ) ++ super.libraryDependencies
}


trait PermissionDependencies extends BasicScalaProject {
  override def libraryDependencies = Set(
  ) ++ super.libraryDependencies
}


trait TradeServiceDependencies extends BasicScalaProject {
  override def libraryDependencies = Set(
  ) ++ super.libraryDependencies
}


trait ReferenceDataNewDependencies extends BasicScalaProject {
  override def libraryDependencies = Set(
    "joda-time" % "joda-time-hibernate" % "1.2" intransitive()  // Pulls in hibernate-3.0.5 otherwise
  ) ++ super.libraryDependencies
}


trait MurdochDependencies extends BasicScalaProject {
  override def libraryDependencies = Set(
    "com.thunderhead" % "thunderhead-api" % "5.0.19.7",
    "org.alfresco" % "web-service-client" % "3.4.0.beta",
    "axis" % "axis" % "1.4",
    "javax.mail" % "mail" % "1.4",
    "wss4j" % "wss4j" % "1.5.0",
    "wsdl4j" % "wsdl4j" % "1.6.2",
    "org.apache" % "xmlsec" % "1.4.2",
    "com.sun.jersey" % "jersey-client" % "1.4-ea06",
    "xerces" % "xercesImpl" % "2.9.1",
    "javax.jms" % "jms" % "1.1"
  ) ++ super.libraryDependencies
}



