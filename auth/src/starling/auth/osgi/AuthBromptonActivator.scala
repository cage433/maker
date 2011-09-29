package starling.auth.osgi

import starling.manager.{BromptonContext, BromptonActivator}
import starling.auth.{AuthHandler, LdapUserLookup}
import starling.auth.internal.{LdapUserLookupImpl, KerberosAuthHandler}

class AuthBromptonActivator extends BromptonActivator {
  def start(context: BromptonContext) {
    val props = context.awaitService(classOf[starling.props.Props])
    val ldapUserLookup = new LdapUserLookupImpl()
    val auth = if (props.UseAuth()) new KerberosAuthHandler("suvmerinWiv0", ldapUserLookup) else AuthHandler.Dev
    context.registerService(classOf[AuthHandler], auth)
    context.registerService(classOf[LdapUserLookup], ldapUserLookup)
  }
  def stop(context: BromptonContext) {
  }
}