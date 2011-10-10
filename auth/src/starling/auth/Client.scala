package starling.auth

trait Client {
  def ticket: Option[Array[Byte]]
}

object Client {
  val Null: Client = new Client {
    def ticket = Some(Array())
  }
}