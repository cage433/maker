package starling.services.jmx

import management.ManagementFactory
import javax.management.ObjectName
import starling.auth.User
import collection.JavaConversions._
import starling.services.{ScheduledTask, Scheduler}
import starling.daterange.Day
import starling.utils.Stopable
import java.util.{UUID, Set => JSet, Map => JMap}

trait UsersMBean {
  def getUserDetails:JSet[String]
}

class Users(users0:JMap[UUID,User]) extends UsersMBean {
  def getUserDetails:JSet[String] = new java.util.TreeSet[String](users0.values.map(user => user.name + " (" + user.phoneNumber + ")"))
}

class StarlingJMX(users:JMap[UUID,User], scheduler: Scheduler) extends Stopable {
  val mbs = ManagementFactory.getPlatformMBeanServer
  val usersMBean = new Users(users)
  val usersName = new ObjectName("Starling:name=Users")

  override def start {
    super.start
    mbs.registerMBean(usersMBean, usersName)
    scheduler.tasks.foreach(task =>
      mbs.registerMBean(new Task(task.task), new ObjectName("Starling.ScheduledTasks:name=" + task.name)))
  }

  trait TaskMBean {
    def activate
    def deactivate
    def isActive: Boolean
    def runNow
    def runNow(observationDay: String)
  }

  class Task(task: ScheduledTask) extends TaskMBean {
    def activate   { task.start }
    def deactivate { task.stop  }
    def isActive   = task.isRunning
    def runNow = task.perform(Day.today)
    def runNow(observationDay: String) = task.perform(Day.parse(observationDay))
  }
}