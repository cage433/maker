package maker.utils

case class Timings(times : List[(Long, Long)]) { // (start, end)

  def countConcurrency : List[(Long, Long, Int)] = {
    if (times.isEmpty)
      return Nil

    var result = List[(Long, Long, Int)]() 

    var startTimes = times.map(_._1).sorted
    var endTimes = times.map(_._2).sorted

    var concurrencyCount = 0
    var intervalStartTime : Option[Long] = None

    def closeInterval(time : Long){
      if (time > intervalStartTime.get)
        result = (intervalStartTime.get, time, concurrencyCount) :: result
      concurrencyCount -= 1
      if (concurrencyCount == 0)
        intervalStartTime = None
      else
        intervalStartTime = Some(time)
    }

    def openInterval(time : Long){
      intervalStartTime match {
        case Some(startTime) if startTime < time => 
          result = (startTime, time, concurrencyCount) :: result
        case _ => 
      }
      intervalStartTime = Some(time)
      concurrencyCount += 1
    }

    while (endTimes.nonEmpty){
      (startTimes.headOption, endTimes.head) match {

        case (Some(tStart), tEnd) if tEnd < tStart =>
          // finishing a task before starting the next
          closeInterval(tEnd)
          endTimes = endTimes.tail

        case (Some(tStart), tEnd) if tStart < tEnd =>
          // Starting a new task before finishing another
          openInterval(tStart)
          startTimes = startTimes.tail

        case (Some(tStart), tEnd)  if tStart == tEnd => 
          // Starting one task and finishing another at the same time
          closeInterval(tEnd)
          endTimes = endTimes.tail

          openInterval(tStart)
          startTimes = startTimes.tail

        case (None, tEnd) => 
          // No new tasts to start - just finishing existing tasks
          closeInterval(tEnd)
          endTimes = endTimes.tail
      }
    }
    result.reverse
  }

  def clockAndCPUTime : (Long, Long) = {
    val cc = countConcurrency
    val clockTime = cc.map{
      case (start, end, _) => end - start
    }.sum
    val cpuTime = cc.map{
      case (start, end, count) => (end - start) * count
    }.sum
    (clockTime, cpuTime)
  }
}
