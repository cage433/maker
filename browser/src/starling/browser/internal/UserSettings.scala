package starling.browser.internal

import java.awt.Rectangle
import starling.browser.service.internal.HeterogeneousMap
import starling.browser.service.{UserSettingsLabel, UserSettingsEntry}
import starling.browser.{BrowserBundle, ServerContext}
import collection.mutable.{HashMap => MMap}
import swing.Publisher
import starling.browser.osgi.{BundleRemoved, BundleAdded}

object UserSettings {
  val MainWindowBounds = Key[Rectangle]("The location and size of the main window", RootBrowserBundle.bundleName)
  val LiveDefault = new Key[Boolean]("LiveDefault", RootBrowserBundle.bundleName)
}

class UserSettingsValue(var value:Option[AnyRef], var data:String)

class UserSettings(initialSettings:UserSettingsLabel, publisher:Publisher) extends Serializable {

  var bundles = new MMap[String,BrowserBundle]()
  val indexedEntries = new MMap[String, MMap[String, UserSettingsValue]]()
  initialSettings.userSettings.groupBy(_.bundle).foreach { case(bundle,values) => {
    val valuesForBundle = new MMap[String,UserSettingsValue]()
    values.foreach(entry => valuesForBundle(entry.key) = new UserSettingsValue(None, entry.value))
    indexedEntries(bundle) = valuesForBundle
  } }

  publisher.reactions += {
    case BundleAdded(bundle) => {
      bundles(bundle.bundleName) = bundle
      indexedEntries.get(bundle.bundleName).foreach { settings => {
        settings.foreach { case (_, value) => {
          value.value = Some(bundle.unmarshal(value.data))
        }
      } } }
    }
    case BundleRemoved(bundle) => {
      bundles.remove(bundle.bundleName)
      indexedEntries.get(bundle.bundleName).foreach { settings => {
        settings.foreach { case (_, value) => {
          value.value = None
        }
      } } }
    }
  }

  private def get[T](key: Key[T]): Option[T] = {
    indexedEntries.get(key.bundle) match {
      case Some(bundleSettings) => {
        bundleSettings.get(key.description) match {
          case None => None
          case Some(value) => {
            value.value match {
              case v@Some(_) => v.asInstanceOf[Some[T]]
              case None => throw new Exception("There is a value for the " + key.description + " setting but the bundle " + key.bundle + " is not avaliable")
            }
          }
        }
      }
      case None => None
    }
  }

  private def put[T](key: Key[T], value: T) {
    val bundleSettings = indexedEntries.getOrElseUpdate(key.bundle, new MMap[String,UserSettingsValue]())
    val v = bundleSettings.getOrElseUpdate(key.description, new UserSettingsValue(Some(value.asInstanceOf[AnyRef]), ""))
    v.data = bundles(key.bundle).marshal(value.asInstanceOf[AnyRef])
  }

  def settingExists[T](key: Key[T]):Boolean = {
    get(key) != None
  }

  def getSetting[T](key: Key[T], default: => T): T = {
    get(key).getOrElse(default)
  }

  def getSetting[T](key: Key[T]): T = {
    get(key).getOrElse(throw new Exception("No setting for " + key))
  }

  def getSettingOption[T](key: Key[T]): Option[T] = {
    get(key)
  }

  def putSetting[T](key: Key[T], value: T) {
    put(key, value)
  }

  def toLabel(serverContext:ServerContext) = {
    UserSettingsLabel(indexedEntries.flatMap { case (bundleName, settings) => {
      settings.map { case (description, value) => UserSettingsEntry(bundleName, description, value.data) }
    } }.toList)
  }

  override def toString = indexedEntries.toString()
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