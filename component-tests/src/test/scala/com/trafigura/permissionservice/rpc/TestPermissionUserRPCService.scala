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


class TestPermissionUserRPCService() extends Logger {

  import TestPermissionUserRPCService._

  var context:ClassPathXmlApplicationContext = null
  var proxy:UserServiceResourceProxy = null
  var rpcURL : String = null
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
    proxy = new UserServiceResourceProxy(ProxyFactory.create(classOf[UserServiceResource], rpcURL, adminComponentTestClient))

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
  def createUser() = {

    Log.debug(">>createUser")
    val userOid = createUserInDb()

    try {
       val user = proxy.get(userOid)
       Log.debug("New user = " + user.toString)
    }
    finally {
       deleteUserInDb(userOid)
       Log.debug("<<createUser")
    }
  }

  @Test
  def createUserWithNameAlreadyUsed() = {

    Log.debug(">>createUserWithNameAlreadyUsed")
    val userOid = createUserInDb()

    try {
       val user = proxy.get(userOid)
       
       val newUser = new User {name = user.name; userId = new UserId {identifier = RandomName.next(ID_PREFIX, ID_LENGTH)} }
       val result = proxy.create(newUser)

       assertFalse(result.success)
       assertEquals("Existing user with the name " + user.name, result.message)
    }
    finally {
       deleteUserInDb(userOid)
       Log.debug("<<createUserWithNameAlreadyUsed")
    }
  }

  @Test
  def createUserWithIdAlreadyUsed() = {

    Log.debug(">>createUserWithIdAlreadyUsed")
    val userOid = createUserInDb()

    try {
       val user = proxy.get(userOid)

       val newUser = new User {name = RandomName.next(NAME_PREFIX, NAME_LENGTH); userId = new UserId {identifier = user.userId.identifier} }
       val result = proxy.create(newUser)

       assertFalse(result.success)
       assertEquals("Existing user with the id " + user.userId.identifier, result.message)
    }
    finally {
       deleteUserInDb(userOid)
       Log.debug("<<createUserWithIdAlreadyUsed")
    }
  }

  @Test
  def getAllUsers() = {

    Log.debug(">>getUsers")
    val userOid = createUserInDb()
    
    try {
      val users = proxy.getAll()
      assert(users != null, "null user list!")
      assert(users.size > 0, "Failed to retrieve users")
      users foreach(u => Log.info("User: " + u.toString))
    }
    finally {
      deleteUserInDb(userOid)
      Log.debug("<<getUsers")
    }
  }

  @Test
  def updateUser() = {

    Log.debug(">>updateUser")
    val userOid = createUserInDb()
    Log.debug(">>userOid = " + userOid)

    try {
      val user = proxy.get(userOid)
      user.name = RandomName.next("updated user name", 32)
      Log.info("updating user: " + user.toString)
      val result = proxy.update(user)

      assertTrue(result.success)
      assertEquals(user.oid, result.identifier, "wrong id in result object")

      val updatedUser = proxy.get(user.oid)
      Log.info("updated user: " + updatedUser.toString)
      assertEquals(user.oid, updatedUser.oid, "update user, oid changed!")
      assertEquals(user.name, updatedUser.name, "update user, name update failed")
    }
    finally {
      deleteUserInDb(userOid)
      Log.debug("<<updateUser")
    }
  }

  @Test
  def updateUserWithExistingName() = {

    Log.debug(">>updateUserWithExistingName")
    val previousExisting = createUserInDb()
    val userOid = createUserInDb()
    Log.debug(">>userOid = " + userOid)

    try {
      val user = proxy.get(userOid)
      user.name = proxy.get(previousExisting).name
      Log.info("updating user: " + user.toString)
      val result = proxy.update(user)

      assertFalse(result.success)
      assertEquals("Existing user with the name " + user.name, result.message)
    }
    finally {
      deleteUserInDb(userOid)
      deleteUserInDb(previousExisting)
      Log.debug("<<updateUserWithExistingName")
    }
  }

  @Test
  def updateUserWithExistingId() = {

    Log.debug(">>updateUserWithExistingId")
    val previousExisting = createUserInDb()
    val userOid = createUserInDb()
    Log.debug(">>userOid = " + userOid)

    try {
      val user = proxy.get(userOid)
      user.userId.identifier = proxy.get(previousExisting).userId.identifier
      Log.info("updating user: " + user.toString)
      val result = proxy.update(user)

      assertFalse(result.success)
      assertEquals("Existing user with the id " + user.userId.identifier, result.message)
    }
    finally {
      deleteUserInDb(userOid)
      deleteUserInDb(previousExisting)
      Log.debug("<<updateUserWithExistingId")
    }
  }

  @Test
  def deleteUser() = {

    Log.debug(">>deleteUser")
    val userOid = createUserInDb()
    Log.info("deleting user, userOid = " + userOid)

    try {
      val user = proxy.get(userOid)
      val res = proxy.delete(userOid)
      assertEquals(res, true, "failed to delete, response = false")

      try {
        val deletedUser = proxy.get(userOid)
        assert(false, "failed to delete user, user still retrievable")
      }
      catch {
        case ex : Exception =>
          Log.info("Retrieving deleted user, user not found")
      }
    }
    finally
      Log.debug("<<deleteUser")
  }


  @Test
  def createUserRoles() = {

    Log.debug(">>createUserRoles")
    val userOid = createUserInDb()
    val user = proxy.get(userOid)
    Log.debug("user " + user.toString)

    val roleOid1 = createRoleInDb()
    Log.debug("role oid " + roleOid1)
    val roleOid2 = createRoleInDb()
    Log.debug("role oid " + roleOid2)

    try {

      val result1 = proxy.addRole(userOid, roleOid1)
      assert(result1 == true, "addRoles successful")
      val result2 = proxy.addRole(userOid, roleOid2)
      assert(result2 == true, "addRoles successful")

      try {
        var userRoles = proxy.getUserRoles(userOid)
        Log.debug("got user roles ")
        assert(userRoles != null, "null user roles list!")
        assertEquals(2, userRoles.size, "Failed to retrieve correct number of user roles")
        userRoles foreach(role => Log.info("User Role: " + role.toString))
      }
      finally {
        proxy.removeRole(userOid, roleOid2)
        proxy.removeRole(userOid, roleOid1)
      }
    }
    finally {
      deleteRoleInDb(roleOid2)
      deleteRoleInDb(roleOid1)
      deleteUserInDb(userOid)
      Log.debug("<<createUserRoles")
    }

  }

  def createUserInDb() = TestPermissionUserRPCService.createUserInDb(rpcURL, adminComponentTestClient)

  def createUserInDb(name : String) = TestPermissionUserRPCService.createUserInDb(name, rpcURL, adminComponentTestClient)

  def deleteUserInDb(userOid: Int) = TestPermissionUserRPCService.deleteUserInDb(userOid, rpcURL, adminComponentTestClient)

  def createRoleInDb() = TestPermissionRoleRPCService.createRoleInDb(rpcURL, adminComponentTestClient)

  def deleteRoleInDb(roleOid: Int) = TestPermissionRoleRPCService.deleteRoleInDb(roleOid, rpcURL, adminComponentTestClient)

}

object TestPermissionUserRPCService extends Logger {

  val NAME_PREFIX = "componentTestUserName_"
  val NAME_LENGTH = 32
  val ID_PREFIX = "componentTestUserId_"
  val ID_LENGTH = 32

  def createUserInDb(rpcUserURL : String, adminComponentTestClient : ComponentTestClientExecutor) : Int =
    createUserInDb(RandomName.next(NAME_PREFIX,NAME_LENGTH), rpcUserURL, adminComponentTestClient)

  def createUserInDb(name : String, rpcUserURL : String, adminComponentTestClient : ComponentTestClientExecutor) : Int = {

    Log.debug(">>createUserInDb")

    val proxy = new UserServiceResourceProxy(ProxyFactory.create(classOf[UserServiceResource], rpcUserURL, adminComponentTestClient))

    var newUser = new User()
    var newUserId = new UserId

    newUserId.identifier = RandomName.next(ID_PREFIX, ID_LENGTH)

    newUser.userId = newUserId
    newUser.name = name

    val userOid = proxy.create(newUser).identifier

    Log.debug("New user created, userOid = " + userOid)

    Log.debug("<<createUserInDb")

    userOid
  }

  def findUserInDbByName(identifier : String, rpcUserURL : String, adminComponentTestClient : ComponentTestClientExecutor) : Int = {

    Log.debug(">>findUserInDbByName")

    val proxy = new UserServiceResourceProxy(ProxyFactory.create(classOf[UserServiceResource], rpcUserURL, adminComponentTestClient))

    var userId = new UserId

    userId.identifier = identifier

    val user = proxy.getByUserId(userId)
    val userOid = user.oid

    Log.debug("Found user, userOid = " + userOid)

    Log.debug("<<findUserInDbByName")

    userOid
  }

  def deleteUserInDb(userOid: Int, rpcUserURL: String, adminComponentTestClient : ComponentTestClientExecutor) : Boolean = {

    Log.debug(">>deleteUserInDb")

    val proxy = new UserServiceResourceProxy(ProxyFactory.create(classOf[UserServiceResource], rpcUserURL, adminComponentTestClient))
    Log.info("deleting user, userOid = " + userOid)
    val user = proxy.get(userOid)
    val res = proxy.delete(userOid)
    Log.debug("user delete success = " + res)
    
    Log.debug("<<deleteUserInDb")

    res
  }

  def getInDb(userOid: Int, rpcUserURL: String, adminComponentTestClient : ComponentTestClientExecutor) : User = {

    Log.debug(">>getInDb")

    val proxy = new UserServiceResourceProxy(ProxyFactory.create(classOf[UserServiceResource], rpcUserURL, adminComponentTestClient))
    Log.info("getting user, userOid = " + userOid)
    val user = proxy.get(userOid)
    Log.debug("got user = " + user)

    Log.debug("<getInDb")

    user
  }

  def addRoleInDb(userOid:Int, roleOid:Int, rpcUserURL : String, adminComponentTestClient : ComponentTestClientExecutor) : Boolean = {

    Log.debug(">>addRoleInDb")

    val proxy = new UserServiceResourceProxy(ProxyFactory.create(classOf[UserServiceResource], rpcUserURL, adminComponentTestClient))
    Log.info("adding role, roleOid = " + roleOid + " to user, userOid = " + userOid)
    val res = proxy.addRole(userOid, roleOid)
    Log.debug("add role success = " + res)

    Log.debug("<<addRoleInDb")

    res
  }

  def removeRoleInDb(userOid:Int, roleOid:Int, rpcUserURL : String, adminComponentTestClient : ComponentTestClientExecutor) : Boolean = {

     Log.debug(">>removeRoleInDb")

     val proxy = new UserServiceResourceProxy(ProxyFactory.create(classOf[UserServiceResource], rpcUserURL, adminComponentTestClient))
     Log.info("removing role, roleOid = " + roleOid + " from user, userOid = " + userOid)
     val res = proxy.removeRole(userOid, roleOid)
     Log.debug("remove role success = " + res)

     Log.debug("<<removeRoleInDb")

     res
   }

}
