package starling.browser.internal

import java.awt.Rectangle
import starling.browser.service.internal.HeterogeneousMap
import starling.browser.service.{UserSettingsLabel, UserSettingsEntry}
import starling.browser.ServerContext

object UserSettings {
  val MainWindowBounds = Key[Rectangle]("The location and size of the main window", RootBrowserContext.bundleName)
  val LiveDefault = new Key[Boolean]("LiveDefault", RootBrowserContext.bundleName)
}

class UserSettings extends Serializable {
  val map = new HeterogeneousMap[ManifestedKey]

  def settingExists[T](key: Key[T])(implicit m: Manifest[T]):Boolean = {
    map.contains(ManifestedKey(m,key))
  }

  def getSetting[T](key: Key[T], default: => T)(implicit m: Manifest[T]): T = {
    map.getOrElse(ManifestedKey(m,key), default)
  }

  def getSetting[T](key: Key[T])(implicit m: Manifest[T]): T = {
    map(ManifestedKey(m,key))
  }

  def getSettingOption[T](key: Key[T])(implicit m: Manifest[T]): Option[T] = {
    map.get(ManifestedKey(m,key))
  }

  def putSetting[T](key: Key[T], value: T)(implicit m: Manifest[T]) {
    map(ManifestedKey(m,key)) = value
  }

  def toLabel(serverContext:ServerContext) = {
    UserSettingsLabel(map.underlying.map { case (ManifestedKey(_,Key(description,bundleName)), value) => {
      val bundle = serverContext.browserBundles.find(_.bundleName == bundleName).get
      UserSettingsEntry(bundleName, description, bundle.marshal(value.asInstanceOf[AnyRef]))
    }}.toList)
  }

  override def toString = map.toString()
}

case class ManifestedKey[K](manifest:String, key:Key[K])
object ManifestedKey {
  def apply[K](manifest:Manifest[K], key:Key[K]):ManifestedKey[K] = ManifestedKey(manifest.toString, key)
}

/**
 * Key for user settings.
 * <P>
 * Because of a bug with the current version of scala, the description is effectively used as the hash map key so please
 * ensure they are unique.
 */
case class Key[T](description: String, bundle:String) {
  override def hashCode = description.hashCode

  override def equals(obj: Any) = obj match {
    case Key(desc, _) => desc == description
    case _ => false
  }
}