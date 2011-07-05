package secure

//import starling.utils.ImplicitConversions._
import collection.immutable.LinearSeq

trait User
object User {
  def get: User = null
}

trait AuthorizationChecker {
  def isAuthorized(user: User): Boolean
}

object AuthorizationChecker {
  val Allow = new FixedAuthorizationChecker(true)
  val Deny = new FixedAuthorizationChecker(false)
}

class FixedAuthorizationChecker(result: Boolean) extends AuthorizationChecker {
  def isAuthorized(user: User) = result
}

class SecureValue[+A](value: A, checker: AuthorizationChecker) {
  def get: Option[A] = get(User.get)
  def get(user: User): Option[A] = if (checker.isAuthorized(user)) Some(value) else None
  def map[B](f: (A) => B): SecureValue[B] = new SecureValue(f(value), checker)
}

class SecureSeq[+A](seq: Seq[SecureValue[A]]) extends LinearSeq[A] {
  def get: Seq[A] = get(User.get)
  def get(user: User): Seq[A] = seq.flatMap(_.get(user).toList)
  def map[B](f: (A) => B): SecureSeq[B] = new SecureSeq(seq.map(_.map(f)))
  def apply(idx: Int): A = get(User.get).apply(idx)
  def length: Int = get(User.get).length
}

case class SecureMap[A, +B](map: Map[A, SecureValue[B]]) extends Map[A, B] {
  def get: Map[A, B] = get(User.get)
  def get(user: User): Map[A, B] = /*map.collectValuesO(_.get(user))*/ null
  def get(user: User, key: A): Option[B] = /*map.get(key).map(_.get(user)).flatOpt*/ null
  override def mapValues[C](f: (B) => C): SecureMap[A, C] = new SecureMap[A, C](map.mapValues(_.map(f)))
  def +[B1 >: B](kv: (A, B1)): SecureMap[A, B1] = copy(map = map + ((kv._1, new SecureValue(kv._2, AuthorizationChecker.Allow))))
  def -(key: A): SecureMap[A, B] = copy(map - key)
  def iterator: Iterator[(A, B)] = get(User.get).iterator
  def get(key: A): Option[B] = get(User.get, key)
}

trait Trade

trait ExampleService {
  // On behalf of a user
  def getAllTrades(user: User): List[Trade]

  // Not on behalf of a user, i.e. to populate a cache
  def getAllTrades: SecureSeq[Trade]
}