package starling.utils

import org.apache.commons.io.{IOUtils => ApacheIOUtils}
import java.lang.reflect.Modifier
import scala.collection.JavaConversions._


object Reflection {
  def listClassesInPackage(packageName: String): List[Class[_]] = listClassesOfType[Object](packageName, classOf[Object])

  def listClassesOfType[T](packageName : String, klass : Class[T]) : List[Class[T]] = {
    def isT(someClass: Class[_]) = klass.isAssignableFrom(someClass) && !Modifier.isAbstract(someClass.getModifiers)

    val packagePath = "/" + packageName.replace('.', '/') + "/"
    val lines = ApacheIOUtils.readLines(getClass.getResourceAsStream(packagePath), "UTF-8").toList.asInstanceOf[List[String]]
    lines.collect { case line if line.endsWith(".class") => Class.forName(packageName + "." + line.stripSuffix(".class")) }
         .collect { case someClass if isT(someClass) => someClass.asInstanceOf[Class[T]]
    }
  }

  def allTypes(klass:Class[_]) = {
    var allClasses = new scala.collection.mutable.ArrayBuffer[Class[_]]()
    var k = klass
    do {
      allClasses += k
      allClasses ++= (k.getInterfaces)
      k = k.getSuperclass
    } while ( k != null)
    allClasses.toList
  }
}
