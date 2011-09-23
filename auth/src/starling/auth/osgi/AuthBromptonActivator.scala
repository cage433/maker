package starling.auth.osgi

import starling.manager.{BromptonContext, BromptonActivator}
import starling.auth.{AuthHandler, LdapUserLookup}
import starling.auth.internal.{LdapUserLookupImpl, KerberosAuthHandler}

/**
 * Defines the useAuth=true property.
 * 
 * @documented
 */
class AuthProps {
  def useAuth = true
}

/**
 * AuthBromptonActivator is an implementation that creates then registers a KerberosAuthHandler on initialisation.
 *
 * @documented
 */
class AuthBromptonActivator extends BromptonActivator {
  type Props = AuthProps
  def defaults = new AuthProps
  /**
   * Creates then registers with the given context a KerberosAuthHandler unless useAuth is overridden to false, when
   * the AuthHandler.Dev instance is used, instead.
   *
   * @param context The service context.
   */
  def init(context: BromptonContext, props: AuthProps) {
    val realProps = context.awaitService(classOf[starling.props.Props])
    val ldapUserLookup = new LdapUserLookupImpl()
    val auth = if (realProps.UseAuth()) new KerberosAuthHandler("suvmerinWiv0", ldapUserLookup) else AuthHandler.Dev
    context.registerService(classOf[AuthHandler], auth)
    context.registerService(classOf[LdapUserLookup], ldapUserLookup)
  }
  /** Does nothing. */
  def start(context: BromptonContext) {
  }
  /** Does nothing. */
  def stop(context: BromptonContext) {
  }
}
