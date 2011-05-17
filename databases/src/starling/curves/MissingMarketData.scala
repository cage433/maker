package starling.curves


/**
 * throw when we can't value a deal - should occur as late as possible (I think)
 * as there may be useful stuff that can be done with a deal even if it can't be valued.
 */
case class MissingMarketData(e : String) extends RuntimeException(e){

}
