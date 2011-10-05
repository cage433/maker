package com.trafigura.tradinghub.support

import org.joda.time.DateTime

trait RdEnumeratedItemLike {

  var identifier: String
  var shortCode: String
  var description: String
  var statusId: String
  var rank: Option[Int]
  var verStart: DateTime
  var verEnd: DateTime

}