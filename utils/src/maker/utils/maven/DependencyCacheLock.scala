package maker.utils.maven

/**
  * Globally locks _all_ ivy interaction.
  *   Current Ivy based cache is not thread-safe to concurrent updates (to the same shared cache)
  */
case object DependencyCacheLock
