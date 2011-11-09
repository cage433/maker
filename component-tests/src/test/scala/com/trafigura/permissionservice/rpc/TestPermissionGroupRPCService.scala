package com.trafigura.permissionservice.rpc

import com.trafigura.tradecapture.internal.permissionservice._
import _root_.com.trafigura.permissionservice.util.RandomName
import com.trafigura.services.log.Logger
import org.jboss.resteasy.client.ProxyFactory
import com.trafigura.services.rpc.RPCConfiguration
import org.springframework.context.support.ClassPathXmlApplicationContext
import org.testng.Assert._
import org.testng.annotations._
import com.trafigura.services.security.ComponentTestClientExecutor


class TestPermissionGroupRPCService() extends Logger {

  var context:ClassPathXmlApplicationContext = null
  var proxy:GroupServiceResourceProxy = null
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
    proxy = new GroupServiceResourceProxy(ProxyFactory.create(classOf[GroupServiceResource], rpcURL, adminComponentTestClient))

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
  def createGroup() = {

    Log.debug(">>createGroup")
    val oid = createGroupInDb()

    try {
      val group = proxy.get(oid)
      Log.debug("New group = " + group.toString)
    }
    finally {
      deleteGroupInDb(oid)
      Log.debug("<<createGroup")
    }
  }

  @Test
  def createGroupWithExistingName() = {

    Log.debug(">>createGroupWithExistingName")
    val oid = createGroupInDb()


    try {
      val group = proxy.get(oid)
      val result = proxy.create(new Group {name = group.name})
      assertFalse(result.success)
      assertEquals("Existing group with the name " + group.name, result.message)
    }
    finally {
      deleteGroupInDb(oid)
      Log.debug("<<createGroupWithExistingName")
    }
  }

  @Test
  def getAllGroups() = {

    Log.debug(">>getAllGroups")
    val oid = createGroupInDb()

    try {
      val groups = proxy.getAll()
      assert(groups != null, "null group list!")
      assert(groups.size > 0, "Failed to retrieve groups")
      groups foreach(g => Log.info("Groups: " + g.toString))
    }
    finally {
      deleteGroupInDb(oid)
      Log.debug("<<getAllGroups")
    }
  }

  @Test
  def updateGroup() = {

    Log.debug(">>updateGroup")
    val oid = createGroupInDb()
    Log.debug(">>oid = " + oid)

    try {

      val group = proxy.get(oid)
      group.name = RandomName.next("updated group name", 32)
      Log.info("updating group: " + group.toString)
      proxy.update(group)
      val updatedGroup = proxy.get(group.oid)
      Log.info("updated group: " + updatedGroup.toString)
      assertEquals(group.oid, updatedGroup.oid, "update, oid changed!")
      assertEquals(group.name, updatedGroup.name, "update, name update failed")
    }
    finally {
      deleteGroupInDb(oid)
      Log.debug("<<updateGroup")
    }
  }

  @Test
  def updateGroupWithExisting() = {

    Log.debug(">>updateGroupWithExisting")
     val existingGroup = createGroupInDb()
    val oid = createGroupInDb()
    Log.debug(">>oid = " + oid)

    try {

      val group = proxy.get(oid)
      group.name = proxy.get(existingGroup).name
      val result = proxy.update(group)

      assertFalse(result.success)
      assertEquals("Existing group with the name " + group.name, result.message)

    }
    finally {
      deleteGroupInDb(oid)
      deleteGroupInDb(existingGroup)
      Log.debug("<<updateGroupWithExisting")
    }
  }

  @Test
  def deleteGroup() = {

    Log.debug(">>deleteGroup")
    val oid = createGroupInDb()
    Log.info("deleting group, oid = " + oid)

    try {
      val user = proxy.get(oid)
      val res = proxy.delete(oid)
      assertEquals(res, true, "failed to delete, response = false")

      try {
        val deletedgroup = proxy.get(oid)
        assert(false, "failed to delete group, user still retrievable")
      }
      catch {
        case ex : Exception => {
          Log.info("Retrieving deleted group, group not found")
        }
      }
    }
    finally {
      Log.debug("<<deleteGroup")
    }
  }

  @Test
  def createGroupRoles() = {

    Log.debug(">>createGroupRoles")
    val groupOid = createGroupInDb()
    val group = proxy.get(groupOid)
    Log.debug("group " + group.toString)
    val roleOid1 = createRoleInDb()
    Log.debug("role oid " + roleOid1)
    val roleOid2 = createRoleInDb()
    Log.debug("role oid " + roleOid2)

    try {
      val result1 = proxy.addRole(groupOid, roleOid1)
      assert(result1 == true, "addRoles successful")
      val result2 = proxy.addRole(groupOid, roleOid2)
      assert(result2 == true, "addRoles successful")

      try {
        var groupRoles = proxy.getGroupRoles(groupOid)
        Log.debug("got group roles ")
        assert(groupRoles != null, "null group roles list!")
        assertEquals(2, groupRoles.size, "Failed to retrieve correct number of group roles")
        groupRoles foreach(role => Log.info("Group Role: " + role.toString))
      }
      finally {
        proxy.removeRole(groupOid, roleOid2)
        proxy.removeRole(groupOid, roleOid1)
      }
    }
    finally {
      deleteRoleInDb(roleOid2)
      deleteRoleInDb(roleOid1)
      deleteGroupInDb(groupOid)
      Log.debug("<<createGroupRoles")
    }
  }

  @Test
  def createGroupUsers() = {
  
    Log.debug(">>createGroupUsers")
    val groupOid = createGroupInDb()
    val group = proxy.get(groupOid)
    Log.debug("group " + group.toString)
    val userOid1 = createUserInDb()
    Log.debug("role oid " + userOid1)
    val userOid2 = createUserInDb()
    Log.debug("role oid " + userOid2)

    try {
      val result1 = proxy.addUser(groupOid, userOid1)
      assert(result1 == true, "addUsers successful")
      val result2 = proxy.addUser(groupOid, userOid2)
      assert(result2 == true, "addUsers successful")

      try {
        val groupUsers = proxy.getGroupUsers(groupOid)
        Log.debug("got group users ")
        assert(groupUsers != null, "null group users list!")
        assertEquals(2, groupUsers.size, "Failed to retrieve correct number of group users")
        groupUsers foreach(user => Log.info("Group Users: " + user.toString))
      }
      finally {
        proxy.removeUser(groupOid, userOid2)
        proxy.removeUser(groupOid, userOid1)
      }
    }
    finally {
      deleteUserInDb(userOid2)
      deleteUserInDb(userOid1)
      deleteGroupInDb(groupOid)
      Log.debug("<<createGroupRoles")
    }
  }



  protected def createRoleInDb() = TestPermissionRoleRPCService.createRoleInDb(rpcURL, adminComponentTestClient)

  protected def deleteRoleInDb(roleOid: Int) = TestPermissionRoleRPCService.deleteRoleInDb(roleOid, rpcURL, adminComponentTestClient)

  protected def createGroupInDb() = TestPermissionGroupRPCService.createGroupInDb(rpcURL, adminComponentTestClient)

  protected def deleteGroupInDb(groupOid: Int) = TestPermissionGroupRPCService.deleteGroupInDb(groupOid, rpcURL, adminComponentTestClient)

  protected def createUserInDb() = TestPermissionUserRPCService.createUserInDb(rpcURL ,adminComponentTestClient)

  protected def deleteUserInDb(roleOid: Int) = TestPermissionUserRPCService.deleteUserInDb(roleOid, rpcURL, adminComponentTestClient)

}


object TestPermissionGroupRPCService extends Logger {
   def createGroupInDb(rpcURL: String, adminComponentTestClient : ComponentTestClientExecutor) : Int = {

    Log.debug(">>createGroupInDb")

    val NAME_PREFIX = "componentTestGroupName_"
    val NAME_LENGTH = 32
    val proxy = new GroupServiceResourceProxy(ProxyFactory.create(classOf[GroupServiceResource], rpcURL, adminComponentTestClient))


    val oid = proxy.create(new Group {name = RandomName.next(NAME_PREFIX, NAME_LENGTH)}).identifier
    Log.debug("New group created, oid = " + oid)
    Log.debug("<<createGroupInDb")

    oid
  }

  def deleteGroupInDb(oid: Int, rpcURL: String, adminComponentTestClient : ComponentTestClientExecutor) : Boolean = {

    val proxy = new GroupServiceResourceProxy(ProxyFactory.create(classOf[GroupServiceResource], rpcURL, adminComponentTestClient))

    Log.debug(">>deleteGroupInDb")

    val group = proxy.get(oid)
    val success = proxy.delete(oid)
    Log.debug("Group deleted, oid = " + oid)
    Log.debug("<<deleteGroupInDb")

    success
  }
  def addRoleInDb(groupOid: Int, roleOid : Int, rpcURL: String, adminComponentTestClient : ComponentTestClientExecutor) : Boolean = {

    val proxy = new GroupServiceResourceProxy(ProxyFactory.create(classOf[GroupServiceResource], rpcURL, adminComponentTestClient))

    Log.debug(">>addRoleInDb")

    val success = proxy.addRole(groupOid, roleOid)

    Log.debug("<<addRoleInDb")

    success
  }

  def removeRoleInDb(groupOid: Int, roleOid : Int, rpcURL: String, adminComponentTestClient : ComponentTestClientExecutor) : Boolean = {

    val proxy = new GroupServiceResourceProxy(ProxyFactory.create(classOf[GroupServiceResource], rpcURL, adminComponentTestClient))

    Log.debug(">>removeRoleInDb")

    val success = proxy.removeRole(groupOid, roleOid)

    Log.debug("<<removeRoleInDb")

    success
  }
}
