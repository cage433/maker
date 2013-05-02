package maker.utils.maven

/**
  * Globally locks _all_ ivy interaction. Appears that ivy is highly non-threas safe
  */
case object IvyLock
