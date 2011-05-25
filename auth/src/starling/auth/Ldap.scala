package starling.auth

import org.springframework.ldap.core.simple.{ParameterizedContextMapper, SimpleLdapTemplate}
import org.apache.commons.io.IOUtils

class Ldap {
  /**
   * filter is like an sql query e.g. (&(objectCategory=person)(sAMAccountName=David.Corcoran))
   */
  def query(filter: String, f: (Map[String, Set[String]]) => Unit) {
    val proc = Runtime.getRuntime.exec((Ldap.ldapsearch :+ filter).toArray)

    // TODO- fixme this will break if there's lots of output. need threads
    val stdOut = IOUtils.toByteArray(proc.getInputStream)
    val stdErr = IOUtils.toByteArray(proc.getErrorStream)

    proc.waitFor

    if (proc.exitValue != 0) {
      throw new Exception("Failed to query ldap (" + proc.exitValue + "), error: " + new String(stdErr))
    }
    val lines = new String(stdOut).replaceAll("\\n ", "").split("\\n")

    var map = new scala.collection.mutable.HashMap[String,scala.collection.mutable.Set[String]] with scala.collection.mutable.MultiMap[String,String]
    for (line <- lines) {
      if (line.startsWith("dn: ")) {
        if (!map.isEmpty) {
          f(convertMap(map))
        }
        map = new scala.collection.mutable.HashMap[String,scala.collection.mutable.Set[String]] with scala.collection.mutable.MultiMap[String,String]
      }
      line.split(": ", 2) match {
        case Array(key, value) => map.addBinding(key, value)
        case _ =>
      }
    }
    if (!map.isEmpty) {
      f(convertMap(map))
    }
  }

  private def convertMap(old: scala.collection.mutable.HashMap[String,scala.collection.mutable.Set[String]]): Map[String, Set[String]] = {
    Map() ++ old.map{
      case (k, v) => (k -> v.toSet) 
    }
  }
}

object Ldap {
  val ldapsearch = List("ldapsearch", "-LLL", "-H", "ldap://londonad.global.trafigura.com:389", "-b", "dc=global,dc=trafigura,dc=com", "-D", "cn=Starling London,ou=system accounts,ou=users,ou=london,ou=emea,dc=global,dc=trafigura,dc=com", "-x", "-w", "NeHopsoalAx0")
}