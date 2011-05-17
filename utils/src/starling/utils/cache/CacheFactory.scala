package starling.utils.cache

import java.lang.String
import starling.utils.CollectionUtils._
import java.util.UUID
import starling.utils.Log
import java.util.concurrent.atomic.AtomicInteger
import com.google.common.collect.MapMaker

/**
 * Usage:
 * CacheFactory.getCache("some namespace").put("my key", 1 + 1)
 *
 * 1 + 1 will only be calculated if there is no entry in the cache for "my key"
 */
object CacheFactory {
  private val caches:Map[CacheParams, CacheImpl] = Map(CacheParams(true, true) -> new SimpleCacheImpl(true), 
    CacheParams(true, false) -> new SimpleCacheImpl(false),
    CacheParams(false, true) -> new Memcached(new SimpleCacheImpl(true)))

  private val cacheCount = new AtomicInteger(0)
  private val cacheStats = new MapMaker().concurrencyLevel(16).makeMap[String, Stats]

  private val statsThread = {
    import scala.collection.JavaConversions._
    
    val t = new Thread("CacheStats") {
      setDaemon(true)
      @volatile
      var running = true

      override def run = {
        var lastMsg = ""
        while (running) {
          Thread.sleep(5 * 1000)
          var out = "-----------------\n"
          cacheStats.values.foreach(s=> out += s + "\n")
          out += "-----------------\n"
          if(lastMsg != out) {
            lastMsg = out
            Log.debug("Cache Stats\n" + out)
          }
        }
      }
    }
    t.start
    t
  }

  /**
   * Documented on getCache
   */
  case class CacheParams(local: Boolean, soft: Boolean) {
    assert((local || soft) != false) // You can't have local = fase and soft = false. Remote caches have weak behaviour.
  }

  trait StatsListener {
    def hit

    def miss
  }

  object StatsListener {
    val Null = new StatsListener {
      def miss = {}
      def hit = {}
    }
  }

  case class Stats(name: String) extends StatsListener {
    private var instances = new AtomicInteger(1)
    private var hits = new AtomicInteger(0)
    private var misses = new AtomicInteger(0)

    def numHits = hits.intValue
    def numMisses = misses.intValue
    def numInstances = instances.intValue

    def miss = misses.incrementAndGet

    def hit = hits.incrementAndGet
    
    def instance = instances.incrementAndGet

    def hitRate = hits.doubleValue / (hits.doubleValue + misses.doubleValue)

    override def toString = name+": instances " + instances + "\thits " + hits + "\tmisses " + misses + "\thitrate " + hitRate
  }

  /**
   * The user of the cache sees this
   */
  abstract class Cache(statsListener: StatsListener) {

    /**
     * Puts the value in the cache and calculates it if necessary. Returns the result of the calculation either
     * from the cache or from calling f(key)
     *
     * @param key any non-null key
     * @param f Lazy on-demand calculation of f. Called with key as the param
     */
    def memoize[K, V](key: K, f: (K => V)): V = memoize(key, f(key))

    def keys[K]() : Set[K]
    /**
     * @see memoize
     */
    def memoize[K, V](key: K, f: => V): V

    /**
     * Written (I believe) to tell the compiler we really don't mean to call
     *  memoize[K, V](key: K, f: (K => V))
     * TODO - get rid of
     *  memoize[K, V](key: K, f: => V)
     */
    def memoizeX[K, V](key: K, f: => V): V = memoize(key, f)

    /**
     * Returns the value for the key if the cache contains it
     *
     * For some reason may need to be called as
     * cache.get[String,String]("some key")
     * as Scala is sometimes unable to infer the correct type which causes a class cast exception
     * at runtime.
     */
    def get[K, V](key: K): Option[V]

   /**
    * Returns the value for the key if the cache contains it and it has completed
    */
    def getIfComplete[K, V](key: K): Option[V]

    def getOrElse[K, V](key : K, default : V) : V = get(key) match {
      case Some(value) => value
      case none => default
    }
    
    def clear:Unit

    def remove[K](key: K)
  }

  /**
   * How many caches have been created in the lifetime of this factory
   */
  def numberOfCaches = cacheCount.get

  /**
   * returns a cache for a given name space and params 
   * @param local If true creates a cache that stays local and in this instance of the JVM. If false
   * may use a remote cache.
   * @param soft If true uses a cache that removes entries when they are not referenced and garbage
   * collection happens. If false acts like a map and never removes references.
   * @param unique If true this means even if others call getCache with the same name each will
   * see a different cache. Useful for local throw-away cache.
   */
  def getCache(name: String, local: Boolean = true, soft: Boolean = true, unique: Boolean = false): Cache = {
    val params = CacheParams(local, soft)
    caches.get(params) match {
      case Some(c) => {
        val statsListener = cacheStats.synchronized{
          cacheStats.get(name) match {
            case null => {
              val listener: Stats = Stats(name)
              cacheStats.put(name, listener)
              listener
            }
            case s => {
              s.instance
              s
            }
          }
        }
        cacheCount.incrementAndGet
        c.get(name, statsListener, unique)
      }
      case None => throw new Exception("No cache matches the params " + params)
    }
  }

  def shutdown = {
    statsThread.running = false
    caches.foreach(_._2.stop)
    statsThread.join
  }

  def stats(name: String) = cacheStats.get(name)

  /**
   * The implentations of Caches extend this
   */
  trait CacheImpl {
    def get(name: String, statsListener: StatsListener, unique: Boolean = false): Cache
    def stop() {}
  }
}