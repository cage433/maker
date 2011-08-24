package starling.bouncyrmi

import scala.swing.event.Event
import org.testng.Assert._

import java.util.concurrent.atomic.AtomicInteger
import org.scalatest.testng.TestNGSuite
import org.testng.annotations.{BeforeMethod, AfterMethod, AfterTest, Test}
import starling.gui.api.{PricingGroup, PricingGroupMarketDataUpdate}
import starling.utils.{Broadcaster, StarlingTest}
import java.util.concurrent.{Executors, CopyOnWriteArraySet, TimeUnit}
import starling.auth.{LdapUserLookup, User}
import starling.services.ChannelLoggedIn

class ClassWhichDoesNotImplementSerializable(val name: String) {
  override def equals(other: Any): Boolean = {
    other match {
      case x: ClassWhichDoesNotImplementSerializable => name == x.name
      case _ => false
    }
  }
}
case class HelloEvent(name: String) extends Event
class ExplosionException extends RuntimeException

trait Service {
  def reply(value: Object): Object

  def foo(name: String): String

  def explode(): Unit

  def fakeMethodWithBroadcast(name:String):String
}
class SomeService extends Service {
  def reply(value: Object) = value

  def foo(name: String): String = "#" + name

  def explode() {throw new ExplosionException}

  def methodNotOnTrait() = "yes"

  var broadcaster:Broadcaster = null
  def fakeMethodWithBroadcast(name:String) = {
    for (i <- 1 to 100) {
      broadcaster.broadcast(PricingGroupMarketDataUpdate(PricingGroup.System, i))
    }
    Thread.sleep(10)
    name + " is bad"
  }
}

class WaitForFlag {
  private val timeout = 100
  private var flag = false
  private val lock = new Object
  private var result = false

  def flip = {
    lock.synchronized {result = true; flag = true; lock.notify()}
  }

  def waitForFlip = lock.synchronized {
    if (!flag) {
      lock.wait(timeout)
      result
    } else {
      true
    }
  }
}

object NettyTest {
  def main(args: Array[String]) {
    //StdOut.init
    //junit.textui.TestRunner.run(classOf[BouncyRMITest])
  }
}

object BouncyRMITestsObj {
  private var port = 12345
  val lock = new Object
  def nextPort = lock.synchronized {
    port += 1
    port
  }
}
class BouncyRMITests extends StarlingTest {

  val auth = new ServerAuthHandler[User](new AuthHandler[User] {
    def authorized(ticket: Option[Array[Byte]]) = Some(User.Test)
  }, new CopyOnWriteArraySet[User], new LdapUserLookup() with BouncyLdapUserLookup[User], x=>(), ChannelLoggedIn)

  var someService:SomeService = null
  var server:BouncyRMIServer[User] = null
  var client: BouncyRMIClient = null
  val port1 = BouncyRMITestsObj.nextPort
  val port2 = BouncyRMITestsObj.nextPort

  @BeforeMethod
  def before() {
    someService = new SomeService()
    server = new BouncyRMIServer(port1, auth, BouncyRMI.CodeVersion, new CopyOnWriteArraySet[User], Set(), ChannelLoggedIn, "", someService)
    client = new BouncyRMIClient("localhost", port1, auth = Client.Null, overriddenUser = None)
  }

  def tearDown() {
    if (server != null && server.isStarted) {server.stop()}
    if (client != null) {client.stop}
  }

  @Test
  def testServerStartAndStop() {
    server.start
    server.stop()
  }

  @Test
  def testClientStartAndStop() {
    client = new BouncyRMIClient(
      "localhost", port1,
      auth = Client.Null, overriddenUser = None)

    var gotException = false
    try {
      client.startBlocking
    } catch {
      case e: CannotConnectException => gotException = true
    }
    assertTrue(gotException)
    client.stop
  }

  @Test
  def testClientConnectingToServerWhenServerStartsBeforeClient() {
    val connected = new WaitForFlag
    val disconnected = new WaitForFlag
    server.start
    client = new BouncyRMIClient(
      "localhost", port1,
      auth = Client.Null, overriddenUser = None)
    client.reactions += {case StateChangeEvent(_, ClientDisconnected) => disconnected.flip}
    client.reactions += {case StateChangeEvent(_, ClientConnected) => connected.flip}
    client.startBlocking
    connected.waitForFlip
    client.stop
    server.stop()
    disconnected.waitForFlip
  }

  @Test
  def testClientConnectFailsIfVersionsDoNotMatch() {
    val server = new BouncyRMIServer(port1, auth, "Two", new CopyOnWriteArraySet[User], Set(), ChannelLoggedIn, "", new SomeService())
    server.start

    val needsReboot = new WaitForFlag
    client = new BouncyRMIClient(
      "localhost", port1,
      auth = Client.Null, overriddenUser = None)
    client.reactions += {case StateChangeEvent(_, ServerUpgrade(_)) => needsReboot.flip}
    client.start

    needsReboot.waitForFlip
    client.stop
    server.stop()
  }

  @Test
  def testClientCallsDisconnectedIfServerIsShutdown() {
    server.start
    val disconnected = new WaitForFlag
    client = new BouncyRMIClient(
      "localhost", port1,
      auth = Client.Null, overriddenUser = None)
    client.reactions += {case StateChangeEvent(_, ServerDisconnected(_)) => disconnected.flip}
    client.startBlocking
    server.stop()
    disconnected.waitForFlip
    client.stop
  }

  @Test
  def testClientReconnectsIfServerIsShutdown() {
    server.start
    val firstConnect = new WaitForFlag
    val secondConnect = new WaitForFlag
    val disconnected = new WaitForFlag
    val connectedCount = new AtomicInteger(0)
    client = new BouncyRMIClient(
      "localhost", port1,
      auth = Client.Null, overriddenUser = None)
    client.reactions += {case StateChangeEvent(_, ServerDisconnected(_)) => disconnected.flip}
    client.reactions += {
      case StateChangeEvent(_, ClientConnected) => {
        if (connectedCount.incrementAndGet == 1) firstConnect.flip else secondConnect.flip
      }
    }
    client.startBlocking
    firstConnect.waitForFlip
    server.stop()
    disconnected.waitForFlip
    Thread.sleep(500) // client has to try a few times
    server = new BouncyRMIServer(port1, auth, BouncyRMI.CodeVersion, new CopyOnWriteArraySet[User], Set(), ChannelLoggedIn, "", new SomeService())
    server.start
    secondConnect.waitForFlip
    client.stop
    Thread.sleep(1)
    server.stop()
  }

  @Test
  def testInvokeMethod() {
    server.start
    client = new BouncyRMIClient(
      "localhost", port1,
      auth = Client.Null, overriddenUser = None)
    client.startBlocking
    val resulta = client.proxy(classOf[Service]).foo("a")
    val resultb = client.proxy(classOf[Service]).foo("b")
    val resultc = client.proxy(classOf[Service]).foo("c")
    client.stop
    server.stop()
    assertEquals(resulta, "#a")
    assertEquals(resultb, "#b")
    assertEquals(resultc, "#c")
  }

  @Test
  def testInvokeBigMethod() {
    val builder = new StringBuilder
    for (i <- (0 to 1000)) {
      builder.append("hhhhhhhhhhhhhhhhhh,")
    }
    val message = builder.toString
    server.start
    client = new BouncyRMIClient(
      "localhost", port1,
      auth = Client.Null, overriddenUser = None)
    client.startBlocking
    val result = client.proxy(classOf[Service]).foo(message)
    client.stop
    server.stop()
    assertEquals(result, "#" + message)
  }

  // NOTE: commmented out because run-in-file doesn't account for the "broken" test group
  //  @Test(groups = Array("broken")) // At the moment we do require classes to be serializable.
  //	def testClassesDoNotNeedToBeSerializable() {
  //		server.start
  //		client = new BouncyRMIClient(
  //				"localhost", port1,
  //				classOf[Service], auth = ClientAuth)
  //		client.startBlocking
  //		val result = client.proxy.reply(new ClassWhichDoesNotImplementSerializable("a"))
  //		client.stop
  //		server.stop()
  //		assertEquals(result, new ClassWhichDoesNotImplementSerializable("a"))
  //	}

  @Test
  def testInvokeMethodRetriesIfDisconnected() {
    val disconnected = new WaitForFlag
    server.start
    client.reactions += {
      case StateChangeEvent(_, ServerDisconnected(_)) => {
        disconnected.flip
      }
    }
    client.startBlocking

    server.stop()
    Thread.sleep(200)
    disconnected.waitForFlip

    server = new BouncyRMIServer(port1, auth, BouncyRMI.CodeVersion, new CopyOnWriteArraySet[User], Set(), ChannelLoggedIn, "", new SomeService())
    server.start

    Thread.sleep(2000)
    val result = client.proxy(classOf[Service]).foo("a")
    client.stop
    server.stop()
    assertEquals(result, "#a")
  }

  @Test
  def testInvokeMethodWhichThrowsException() {
    server.start
    client = new BouncyRMIClient(
      "localhost", port1,
      auth = Client.Null, overriddenUser = None)
    client.startBlocking
    try {
      client.proxy(classOf[Service]).explode
      fail("should have thrown exception")
    } catch {
      case e: Exception => {}
      case e => {
        e.printStackTrace
        fail("Threw " + e + " instead of Exception")
      }
    }
    client.stop
    server.stop()
  }

  @Test
  def testInvokeMethodWithManyClientsAndManyThreads() {
    val threadCount = 5
    val exceptions = new java.util.concurrent.ConcurrentHashMap[Int, Throwable]()
    server.start
    val clients = (for (i <- 1 to 2) yield {
      val client = new BouncyRMIClient(
        "localhost", port1,
        auth = Client.Null, overriddenUser = None)
      client.startBlocking
      client
    }).toArray
    val countdownLatch = new java.util.concurrent.CountDownLatch(threadCount)
    val threads = for (i <- 1 to threadCount) yield new Thread(new Runnable() {
      def run() {
        try {
          for (x <- 1 to 10) {
            val client = clients(x % clients.length)
            assertEquals("#a", client.proxy(classOf[Service]).foo("a"))
          }
        } catch {
          case e: Throwable => exceptions.put(i, e)
        }
        countdownLatch.countDown
      }
    }) {
      start()
    }
    val reachedZero = countdownLatch.await(4, TimeUnit.SECONDS)
    for (client <- clients) client.stop
    server.stop()
    if (!reachedZero) {
      fail("Not all threads completed after 4s")
    }
    if (!exceptions.isEmpty) {
      fail(exceptions.size + " threads threw exceptions. " + exceptions)
    }
  }

  @Test
  def testInvokeMethodOnProxiedConcreteClass() {
    server.start
    val client1 = new BouncyRMIClient(
      "localhost", port1,
      auth = Client.Null, overriddenUser = None)
    client1.startBlocking
    val result = client1.proxy(classOf[SomeService]).methodNotOnTrait
    client1.stop
    server.stop()
    assertEquals(result, "yes")
  }

  @Test
  def testInvokingMethodWhenOfflineThrowsOfflineException() {
    val disconnected = new WaitForFlag
    val connected = new WaitForFlag
    client = new BouncyRMIClient(
      "localhost", port1,
      auth = Client.Null, overriddenUser = None)
    client.reactions += {case StateChangeEvent(_, ServerDisconnected("patch release")) => disconnected.flip}
    client.reactions += {case StateChangeEvent(_, ClientConnected) => connected.flip}
    try {
      client.startBlocking
      client.proxy(classOf[Service]).foo("f")
      fail("Expected exception as server is not running")
    } catch {
      case e: CannotConnectException => {assertEquals("Can not connect to server localhost:" + port1, e.getMessage)}
      case e: Exception => fail("Expected OfflineException, got " + e)
    }
    //also check exception works after a connect and disconnect
    server.start
    //client.startBlocking
    connected.waitForFlip
    server.stop("patch release")
    disconnected.waitForFlip
    try {
      client.proxy(classOf[Service]).foo("f")
      fail("Expected exception as server is not running")
    } catch {
      case e: OfflineException => {
        //assertEquals(e.getMessage, "patch release") This message is lost when a reconnect fails
        //I have spent some time trying to fix this but as it is not shown in the GUI it is not so important
      }
      case e: Exception => fail("Expected OfflineException, got " + e)
    }

    client.stop
  }

  @Test
  def testInvokingMethodWhenVersionsDontMatchThrowsRebootRequiredException() {
    val disconnected = new WaitForFlag
    val connected = new WaitForFlag    
    client.reactions += {case StateChangeEvent(_, ServerDisconnected(_)) => disconnected.flip}
    client.reactions += {case StateChangeEvent(_, ClientConnected) => connected.flip}
    server.start
    client.startBlocking
    connected.waitForFlip
    server.stop()
    disconnected.waitForFlip
    val serverOne = new BouncyRMIServer(port1, auth, "One", new CopyOnWriteArraySet[User], Set(), ChannelLoggedIn, "", new SomeService())
    serverOne.start
    try {
      Thread.sleep(2000) // wait for reconnect
      client.proxy(classOf[Service]).foo("f")
      fail("Expected ServerUpgradeException")
    } catch {
      case e: ServerUpgradeException => {assertEquals("One", e.serverVersion)}
      case e: Exception => fail("Expected ServerUpgradeException, got " + e)
    }
    serverOne.stop()
    client.stop
  }

  @Test
  def testBroadcast() {
    server.start
    val client1 = new BouncyRMIClient(
      "localhost", port1,
      auth = Client.Null, overriddenUser = None)
    val client2 = new BouncyRMIClient(
      "localhost", port1,
      auth = Client.Null, overriddenUser = None)
    val ping1 = new WaitForFlag
    val ping2 = new WaitForFlag
    client1.reactions += {case HelloEvent(name) => ping1.flip}
    client1.startBlocking
    client2.reactions += {case HelloEvent(name) => ping2.flip}
    client2.startBlocking
    server.publish(HelloEvent("j"))
    ping1.waitForFlip
    ping2.waitForFlip
    client1.stop
    client2.stop
    server.stop()
  }

  @Test
  def callAMethodWhilstDoingALotOfBroadcastsManyTimes() {
    server.start
    val broadcaster = new Broadcaster {
      val executor = Executors.newCachedThreadPool()
      def broadcast(event: Event) = {
        executor.execute(new Runnable() { def run() {
          server.publish(event)
        }})
      }
    }
    someService.broadcaster = broadcaster
    client.startBlocking
    (1 to 25).toArray.foreach{ i =>
      val result = client.proxy(classOf[Service]).fakeMethodWithBroadcast(i + " Dave")
    }
    client.stop
    server.stop()
    broadcaster.executor.shutdown
  }
}
