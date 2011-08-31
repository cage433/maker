package starling.utils.cache

import starling.utils.Log
import net.spy.memcached.{AddrUtil, BinaryConnectionFactory, MemcachedClient}
import java.util.concurrent.FutureTask
import com.google.common.collect.MapMaker
import starling.utils.CollectionUtils._
import org.apache.commons.codec.digest.DigestUtils
import starling.utils.cache.CacheFactory.{StatsListener, Cache, CacheImpl}

private class Memcached(simple: SimpleCacheImpl) extends CacheImpl with Log {
  private val caches = new MapMaker().concurrencyLevel(16).makeMap[String, FutureTask[Cache]]

  lazy val connection: Option[MemcachedClient] = {
    val memcached = new MemcachedClient(new BinaryConnectionFactory(), AddrUtil.getAddresses("localhost:11211"))
    Thread.sleep(200)
    if (!memcached.getUnavailableServers.isEmpty) {
      log.warn("Failed to connect to memcached instance")
      memcached.shutdown
      None
    } else {
      Some(memcached)
    }
  }

  def get(name: String, statsListener: StatsListener, unique: Boolean): Cache = {
    connection match {
      case Some(c) => if(!unique) {
        putIfAbsent(caches, name, new MemcachedCache(statsListener, c, simple, name))._1
      } else {
        new MemcachedCache(statsListener, c, simple, name)
      }
      case None => {
        log.warn("Creating non-memcached cache for `" + name + "`")
        putIfAbsent(caches, name, simple.get(name, statsListener))._1
      }
    }
  }

  override def stop() = {
    connection match {
      case Some(c) => c.shutdown
      case None =>
    }
  }
}

class MemcachedCache(statsListener: StatsListener, connection: MemcachedClient, simple: SimpleCacheImpl, name: String)
  extends Cache(statsListener) with Log {

  val prefix = name + "::"
  val simpleCache = simple.get(name, statsListener)

  def get[K, V](key: K): Option[V] = {
    val option = simpleCache.get(key)
    option match {
      case None => {
        try {
          connection.get(key.toString) match {
            case null => None
            case v => Some(v.asInstanceOf[V])
          }
        } catch {
          case e => {
            log.warn("Problem dealing with memcache", e)
            None
          }
        }
      }
      case v => {
        v
      }
    }
  }

  def remove[K](key: K) = {
    simpleCache.remove(key)
    try {
      connection.delete(makeKey(key))
    }
    catch {
      case e => {
        log.warn("Problem dealing with memcache", e)
      }
    }
  }

  def keys[K]() = simpleCache.keys()

  def getIfComplete[K, V](key: K) = simpleCache.getIfComplete(key)

  def memoize[K, V](key: K, f: => V): V = {
    // only calculated if we need it
    lazy val calculatedValue = simpleCache.memoize(key, f)

    // check if the simple cache has the value
    simpleCache.get(key) match {
      case Some(v) => v
      case None => {
        // simple cache doesn't have the value so check against memcached
        try {
          connection.get(makeKey(key)) match {
            case null => {
              // store in memcache and has side-effect of also storing in simplecache
              val k: String = makeKey(key)
              val v: Object = calculatedValue.asInstanceOf[Object]
              connection.set(k, 216000, v)
              calculatedValue
            }
            case v => {
              val value = v.asInstanceOf[V]
              simpleCache.memoize(key, value)
              value
            }
          }
        } catch {
          case e => {
            log.warn("Problem dealing with memcache", e)
            calculatedValue
          }
        }
      }
    }
  }
  private def makeKey[K](key: K) = DigestUtils.md5Hex(key.toString)
  def clear = {simpleCache.clear}
}
