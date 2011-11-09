package com.trafigura.permissionservice.rpc

import com.trafigura.trademgmt.internal.permission._
import _root_.com.trafigura.permissionservice.util.RandomName
import com.trafigura.services.log.Logger
import org.jboss.resteasy.client.ProxyFactory
import com.trafigura.services.rpc.RPCConfiguration
import org.springframework.context.support.ClassPathXmlApplicationContext
import org.testng.Assert._
import org.testng.annotations._
import com.trafigura.services.security.ComponentTestClientExecutor


class TestPermissionRoleRPCService() extends Logger {

  var context:ClassPathXmlApplicationContext = null
  var proxy:RoleServiceResourceProxy = null
  var rpcURL:String = null
  var adminComponentTestClient : ComponentTestClientExecutor = null

  @BeforeClass
  def setup {

    //Get some spring configuration
    context = new ClassPathXmlApplicationContext("permission-unit-tests.xml")
    val rpcConfiguration = context.getBean("testPermissionRPCService", classOf[RPCConfiguration])

    //Create / get component test clients
    adminComponentTestClient = context.getBean("clientExecutor").asInstanceOf[ComponentTestClientExecutor]

    //Construct the proxy for RPC stuff
    rpcURL = rpcConfiguration.getUrl + "/RPC"
    proxy = new RoleServiceResourceProxy(ProxyFactory.create(classOf[RoleServiceResource], rpcURL, adminComponentTestClient))

    //Print out where we are connecting to
    Log.debug(rpcConfiguration.toString)
  }

  @AfterClass
  def destroy {

    //Ensure that the spring context is closed down nicely
    if(context != null) {
      context.close
    }
  }

  /**
   * Simple test cases to test basic invocation of the service operations
   * such as create, get and update operations
   */
  @Test
  def createRole() = {

    Log.debug(">>createRole")
    val oid = createRoleInDb()

    try {
      val role = proxy.get(oid)
      Log.debug("New Role = " + role.toString)
    }
    finally {
      deleteRoleInDb(oid)
      Log.debug("<<createRole")
    }
  }

  @Test
  def createRoleWithExistingName() = {

    Log.debug(">>createRoleWithExistingName")
    val oid = createRoleInDb()

    try {
      val role = proxy.get(oid)

      val result = proxy.create(new Role {name = role.name})

      assertFalse(result.success)
      assertEquals("Existing role with the name " + role.name, result.message)
    }
    finally {
      deleteRoleInDb(oid)
      Log.debug("<<createRoleWithExistingName")
    }
  }

  @Test
  def getAllRoles() = {

    Log.debug(">>getAllRoles")
    val oid = createRoleInDb

    try {
      val roles = proxy.getAll()
      assert(roles != null, "null role list!")
      assert(roles.size > 0, "Failed to retrieve roles")
      roles foreach(r => Log.info("Roles: " + r.toString))
    }
    finally {
      deleteRoleInDb(oid)
      Log.debug("<<getAllRoles")
    }
  }

  @Test
  def updateRole() = {

    Log.debug(">>updateRole")
    val oid = createRoleInDb()
    Log.debug("oid = " + oid)

    try {
      val role = proxy.get(oid)
      role.name = RandomName.next("updated role name", 32)
      Log.info("updating role: " + role.toString)
      proxy.update(role)
      val updatedRole = proxy.get(role.oid)
      Log.info("updated role: " + updatedRole.toString)
      assertEquals(role.oid, updatedRole.oid, "update role, oid changed!")
      assertEquals(role.name, updatedRole.name, "update role, name update failed")
    }
    finally {
      deleteRoleInDb(oid)
      Log.debug("<<updateRole")
    }
  }

  @Test
  def updateRoleWithExistingName() = {

    Log.debug(">>updateRoleWithExistingName")
    val existingRole = createRoleInDb()
    val oid = createRoleInDb()
    Log.debug("oid = " + oid)

    try {
      val role = proxy.get(oid)
      role.name = proxy.get(existingRole).name
      val result = proxy.update(role)

      assertFalse(result.success)
      assertEquals("Existing role with the name " + role.name, result.message)
    }
    finally {
      deleteRoleInDb(oid)
      deleteRoleInDb(existingRole)
      Log.debug("<<updateRoleWithExistingName")
    }
  }

  @Test
  def deleteRole() = {

    Log.debug(">>deleteRole")
    val oid = createRoleInDb()
    Log.info("deleting role, oid = " + oid)

    try {

      val user = proxy.get(oid)
      val res = proxy.delete(oid)

      assertEquals(res, true, "failed to delete, response = false")

      try {

        val deletedUser = proxy.get(oid)
        assert(false, "failed to delete role, user still retrievable")
      }
      catch {
        case ex : Exception => {
          Log.info("Retrieving deleted role, role not found")
        }
      }
    }
    finally
      Log.debug("<<deleteRole")
  }

  @Test
  def roleGroups() = {

    Log.debug(">>roleGroups")
    val oid = createRoleInDb()
    Log.info("create role in db, oid = " + oid)

    val groupOid = createGroupInDb
    assertTrue(addRoleInDb(groupOid, oid), "Unable to add role group")

    try {

      val groups = proxy.getRoleGroups(oid)
      assertEquals(groups.size, 1, "We should only have one group for the role")
      assertEquals(groups.head.oid, groupOid, "We do not have the same group that was expected")
    }
    finally {
      removeRoleInDb(groupOid, oid)
      deleteGroupInDb(groupOid)
      deleteRoleInDb(oid)

      Log.debug("<<roleGroups")
    }
  }

  def createRoleInDb() = TestPermissionRoleRPCService.createRoleInDb(rpcURL, adminComponentTestClient)
  def deleteRoleInDb(roleOid: Int) = TestPermissionRoleRPCService.deleteRoleInDb(roleOid, rpcURL, adminComponentTestClient)

  def createGroupInDb() = TestPermissionGroupRPCService.createGroupInDb(rpcURL, adminComponentTestClient)
  def deleteGroupInDb(groupOid: Int) = TestPermissionGroupRPCService.deleteGroupInDb(groupOid, rpcURL, adminComponentTestClient)

  def addRoleInDb(groupOid : Int, roleOid : Int) = TestPermissionGroupRPCService.addRoleInDb(groupOid, roleOid, rpcURL, adminComponentTestClient)
  def removeRoleInDb(groupOid : Int, roleOid : Int) = TestPermissionGroupRPCService.removeRoleInDb(groupOid, roleOid, rpcURL, adminComponentTestClient)

}

object TestPermissionRoleRPCService extends Logger {

  val NAME_PREFIX = "componentTestRoleName_"
  val NAME_LENGTH = 32

  def createRoleInDb(rpcRoleURL: String, adminComponentTestClient : ComponentTestClientExecutor) : Int =
    createRoleInDb(RandomName.next(NAME_PREFIX,NAME_LENGTH), rpcRoleURL, adminComponentTestClient)

  def createRoleInDb(name: String, rpcRoleURL: String, adminComponentTestClient : ComponentTestClientExecutor) : Int = {

    Log.debug(">>createRoleInDb")

    val proxy = new RoleServiceResourceProxy(ProxyFactory.create(classOf[RoleServiceResource], rpcRoleURL, adminComponentTestClient))

    val newRole = new Role()
    newRole.name = name

    val oid = proxy.create(newRole).identifier
    Log.debug("New role created, oid = " + oid)

    Log.debug("<<createRoleInDb")

    oid
  }

  def findRoleInDbByName(roleName: String, rpcRoleURL : String, adminComponentTestClient : ComponentTestClientExecutor) : Option[Int] = {

    Log.debug(">>findRoleInDbByName")

    val proxy = new RoleServiceResourceProxy(ProxyFactory.create(classOf[RoleServiceResource], rpcRoleURL, adminComponentTestClient))
    val role : Option[Role] = proxy.getAll().find(r => r.name == roleName && !r.disabled)
    Log.debug("Role found = " + role.toString)

    Log.debug("<<findRoleInDbByName")

    role match { case Some(r) => Some(r.oid); case _ => None }
  }

  def deleteRoleInDb(oid: Int, rpcRoleURL : String, adminComponentTestClient : ComponentTestClientExecutor) : Int = {

    Log.debug(">>removeRoleInDb")

    val proxy = new RoleServiceResourceProxy(ProxyFactory.create(classOf[RoleServiceResource], rpcRoleURL, adminComponentTestClient))
    val result = proxy.delete(oid)
    Log.debug("Delete role result = " + result)

    Log.debug("<<removeRoleInDb")

    oid
  }

}
