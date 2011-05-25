package starling.services.jmx

import management.ManagementFactory
import javax.management.ObjectName
import starling.auth.User
import java.util.{Set => JSet}
import collection.JavaConversions._

trait UsersMBean {
  def getUserDetails:JSet[String]
}

class Users(users0:JSet[User]) extends UsersMBean {
  def getUserDetails:JSet[String] = new java.util.TreeSet[String](users0.map(user => user.name + " (" + user.phoneNumber + ")"))
}

class StarlingJMX(users:JSet[User]) {
  val mbs = ManagementFactory.getPlatformMBeanServer
  val usersMBean = new Users(users)
  val usersName = new ObjectName("Starling:name=Users")

  def start {
    mbs.registerMBean(usersMBean, usersName)
  }
}