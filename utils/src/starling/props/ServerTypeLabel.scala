package starling.props

case class ServerTypeLabel(name : String){
  override def toString = name
}

object ServerTypeLabel {
  val Dev = new ServerTypeLabel("Dev"){}
  val FC2 = new ServerTypeLabel("FC2"){}
  val Oil = new ServerTypeLabel("Oil"){}
  val instances = List(Dev, FC2, Oil)
  def fromName(name : String) = instances.find(_.name == name).getOrElse(throw new Exception("Unrecognised server name " + name))
}

