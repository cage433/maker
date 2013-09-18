package maker.task.publish

/**
  * Globally locks _all_ ivy interaction.
  *   Current Ivy based cache is not thread-safe to concurrent updates (to the same shared cache)
  */
case object IvyLock
