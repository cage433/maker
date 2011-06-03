package starling.pivot.model

import collection.Seq
import starling.pivot._
import starling.auth.LdapUserLookup

object OrgPivotTableDataSource {
  private val ldapUser = new LdapUserLookup
  lazy val Users = {
    ('A' to 'Z').flatMap(letter => ldapUser.usersStartingWith(letter)).toList 
  }
}

class OrgPivotTableDataSource extends UnfilteredPivotTableDataSource {
  val name = FieldDetails("Name")
  val phone = FieldDetails("Phone")
  val email = FieldDetails("Email")
  val manager = FieldDetails("Manager")
  val department = FieldDetails("Department")

  def fieldDetailsGroups = {
    List(FieldDetailsGroup("Organisation", name,phone,email,manager,department))
  }
  def unfilteredData(pfs : PivotFieldsState) = {
    val maps = OrgPivotTableDataSource.Users.map(user => {
      Map(
        name.field -> user.name,
        phone.field -> user.phoneNumber,
        email.field -> user.email,
        manager.field -> user.manager.getOrElse(""),
        department.field -> user.department)
    })
    maps
  }

  override def initialState = {
    PivotFieldsState(rowFields = List(name.field), dataFields = List(phone.field,email.field,manager.field,department.field))
  }
}