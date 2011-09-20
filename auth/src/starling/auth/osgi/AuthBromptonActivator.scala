package starling.auth.osgi

import starling.manager.{BromptonContext, BromptonActivator}
import starling.auth.internal.KerberosAuthHandler
import starling.auth.{AuthHandler, LdapUserLookup}

class AuthProps {
  def useAuth = false
}
class AuthBromptonActivator extends BromptonActivator {
  type Props = AuthProps
  def defaults = new AuthProps
  def init(context: BromptonContext, props: AuthProps) {
    val auth = if (props.useAuth) new KerberosAuthHandler("suvmerinWiv0", new LdapUserLookup()) else AuthHandler.Dev
    context.registerService(classOf[AuthHandler], auth)
  }
  def start(context: BromptonContext) {

  }
  def stop(context: BromptonContext) {

  }
}