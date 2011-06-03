package starling.services.jmx

import management.ManagementFactory
import javax.management.ObjectName
import starling.auth.User
import java.util.{Set => JSet}
import collection.JavaConversions._
import starling.services.{ScheduledTask, Scheduler}
import starling.daterange.Day

trait UsersMBean {
  def getUserDetails:JSet[String]
}

class Users(users0:JSet[User]) extends UsersMBean {
  def getUserDetails:JSet[String] = new java.util.TreeSet[String](users0.map(user => user.name + " (" + user.phoneNumber + ")"))
}

class StarlingJMX(users:JSet[User], scheduler: Scheduler) {
  val mbs = ManagementFactory.getPlatformMBeanServer
  val usersMBean = new Users(users)
  val usersName = new ObjectName("Starling:name=Users")

  def start {
    mbs.registerMBean(usersMBean, usersName)
    scheduler.tasks.foreach(task =>
      mbs.registerMBean(new Task(task.task), new ObjectName("Starling.ScheduledTasks:name=" + task.name)))
  }

  trait TaskMBean {
    def runNow: Unit
    def runNow(observationDay: String): Unit
  }

  class Task(task: ScheduledTask) extends TaskMBean {
    def runNow = task.execute(Day.today)
    def runNow(observationDay: String) = task.execute(Day.parse(observationDay))
  }
}