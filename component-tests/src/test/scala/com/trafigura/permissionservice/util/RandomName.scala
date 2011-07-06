package com.trafigura.permissionservice.util

import com.trafigura.services.log.Logger
import util.Random

// pseud's corner :=^)

object RandomName extends Random with Logger {

  def next(prefix:String, length:Int) : String = {
    val randomLength = length - prefix.length
    assert(randomLength > 0)
    val name = prefix + nextString(randomLength)
    Log.debug("Next random name = " + name)
    name
  }

}
