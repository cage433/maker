package com.trafigura.permissionservice.rpc

import com.trafigura.tradecapture.internal.permissionservice._
import _root_.com.trafigura.services.permission.{TestUserToSidMap, PermissionCreatorForTests}
import com.trafigura.services.log.Logger
import org.jboss.resteasy.client.ProxyFactory
import com.trafigura.services.rpc.RPCConfiguration
import org.springframework.context.support.ClassPathXmlApplicationContext
import org.testng.Assert._
import org.testng.annotations._
import com.trafigura.services.security.ComponentTestClientExecutor


class TestPermissionRPCService() extends Logger {

  var context:ClassPathXmlApplicationContext = null
  var proxy:PermissionServiceResourceProxy = null
  var rpcURL:String = null
  protected var componentTestClient : ComponentTestClientExecutor = null
  var userIdentifier: String = null

  val testRole = "AGTradeEntry"
  val agPermission = "permission.trade.read.TrafiguraAG"
  val nonAgPermission = "permission.trade.read.TrafiguraBeheerBV"

  @BeforeClass
  def setup {

    context = new ClassPathXmlApplicationContext("permission-unit-tests.xml")
    val rpcConfiguration = context.getBean("testPermissionRPCService", classOf[RPCConfiguration])

    componentTestClient = context.getBean("clientExecutor").asInstanceOf[ComponentTestClientExecutor]
    userIdentifier = TestUserToSidMap.getSidFromUserId("refined.metals-testuser")

    //Construct the proxy for RPC stuff
    rpcURL = rpcConfiguration.getUrl + "/RPC"
    proxy = new PermissionServiceResourceProxy(ProxyFactory.create(classOf[PermissionServiceResource], rpcURL, componentTestClient))

    //Print out where we are connecting to
    Log.debug(rpcConfiguration.toString)

    val userOid = PermissionCreatorForTests.createUserAndAgTradeEntryRole(rpcURL, componentTestClient)
    Log.debug("User oid = " + userOid)
  }

  @AfterClass
  def destroy {
    //Ensure that the spring context is closed down nicely
    if(context != null) {
      context.close
    }
  }

  @Test
  def getAll() =
    testGetAll("getAll", userIdentifier, { () => gotSome({ () => proxy.getAllUser(userIdentifier) }) })

  @Test
  def getEffective() =
    testGetAll("getEffective", userIdentifier, () => testGetEffective())

  private def testGetEffective() {
    val p1 = new Permission
    p1.name = agPermission
    val p2 = new Permission
    p2.name = nonAgPermission
    val effectivePermissions = proxy.getEffective(userIdentifier, List(p1, p2))
    assertNotNull(effectivePermissions, "null efffective permissions list!")
    assertEquals(effectivePermissions.size, 1, "Wrong number of effective permissions retrieved")
    assertEquals(effectivePermissions.head.name, agPermission, "Wrong effective permission retrieved")
    effectivePermissions foreach(g => Log.info("Permissions: " + g.toString))
  }

  @Test
  def checkPermission() =
    testGetAll("checkPermission", userIdentifier, () => testCheckPermission())

  private def testCheckPermission() {
    val agP = new Permission
    agP.name = agPermission
    assertTrue(proxy.checkPermission(userIdentifier, List(agP)),"Got no AG permission")
    val nonAgP = new Permission
    nonAgP.name = nonAgPermission
    assertFalse(proxy.checkPermission(userIdentifier, List(nonAgP)), "Got non-AG permission")
  }

  @Test
  def getAllUser() =
    testGetAll("getAllUser", userIdentifier, { () => gotSome({ () => proxy.getAllUser(userIdentifier) }) })

  private def gotSome(get: () => List[Permission]) {
    val permissions = get()
    assertNotNull(permissions, "null permissions list!")
    assertFalse(permissions.size == 0, "Failed to retrieve permissions")
    assertTrue(permissions.exists( p => p.name == agPermission),"AG permission unexpectedly absent")
    assertFalse(permissions.exists( p => p.name == nonAgPermission),"nonAG permission unexpectedly present")
    permissions foreach(g => Log.info("Permissions: " + g.toString))
  }

  private def testGetAll(testName: String, userName: String, g: () => Unit) {
    Log.info(">>" + testName)

    val foundAgTradeEntryRoleOid = TestPermissionRoleRPCService.findRoleInDbByName("AGTradeEntry", rpcURL, componentTestClient)

    val createdAgTradeEntryRoleOid : Option[Int] =
      foundAgTradeEntryRoleOid match {
        case Some(o) => None
        case None => Some(TestPermissionRoleRPCService.createRoleInDb("AGTradeEntry", rpcURL, componentTestClient))
      }

    val agTradeEntryRoleOid =
       foundAgTradeEntryRoleOid match {
        case Some(o) => o
        case None => createdAgTradeEntryRoleOid match {
          case Some(o) => o
          case None => throw new Exception("agTradeEntryRoleOid disappeared")
        }
      }

    val userOid = TestPermissionUserRPCService.findUserInDbByName(userIdentifier, rpcURL, componentTestClient)
    val roleOid = TestPermissionUserRPCService.addRoleInDb(userOid, agTradeEntryRoleOid, rpcURL, componentTestClient)

    try {
      g()
    }
    finally {
      TestPermissionUserRPCService.removeRoleInDb(userOid, agTradeEntryRoleOid, rpcURL, componentTestClient)
      createdAgTradeEntryRoleOid match {
        case Some(o) => TestPermissionRoleRPCService.deleteRoleInDb(o, rpcURL, componentTestClient)
        case None =>
      }
      Log.info("<<" + testName)
    }
  }
}
