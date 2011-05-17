package starling.utils

import org.apache.commons.io.IOUtils
import java.lang.reflect.Modifier


class Reflection {

  def listClassesOfType[T](packageName : String, klass : Class[T]) : List[Class[T]] = {
    var classes = List[Class[T]]()
    val packagePath = "/" + packageName.replace('.', '/') + "/"
    val iterator = IOUtils.lineIterator(getClass.getResourceAsStream(packagePath), "UTF-8")
    while (iterator.hasNext) {
      val line = iterator.nextLine
      if (line.endsWith(".class")) {
        val someClass = Class.forName(packageName + "." + line.substring(0, line.length-6))
        if (klass.isAssignableFrom(someClass) && !Modifier.isAbstract(someClass.getModifiers)) {
          classes = someClass.asInstanceOf[Class[T]] :: classes
        }
      }
    }
    classes
  }

}
