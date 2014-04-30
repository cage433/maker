/*
 * Copyright (c) 2011-2012, Alex McGuire, Louis Botterill
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met: 
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer. 
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution. 
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package maker.utils

import java.text.SimpleDateFormat
import java.util.Date

case class Stopwatch(
  startTime : Long = System.nanoTime,
  name : String = "", 
  private var snapshots_ : Map[String, Long] = Map()
){
  def snapshot(name : String) = {snapshots_ = snapshots_ + (name -> currentTime); this}
  def snapshots = Map[String, Long]() ++ snapshots_
  def snapshotTime(name : String) = snapshots.get(name)
  def currentTime() = System.nanoTime
  def nanos() = currentTime - startTime
  def ms() : Long = (nanos) / 1000000
  def s() : Long = ms / 1000
  def toStringSeconds = s() + "(s)"
  override def toString : String = name + "  " +Stopwatch.milliToHumanString(ms())

}

object Stopwatch {
  val global = new Stopwatch

  def time[A](name : String)(f: =>A) : A = {
    val stopwatch = new Stopwatch(name = name)
    val result = f
    println("Time: " + stopwatch)
    result
  }

  def time[A](f: =>A) : A = time("")(f)

  def milliToHumanString(milli:Long):String = {
    if (milli < 1000) {
      milli + "(ms)"
    } else if (milli < 60*1000) {
      (milli / 1000) + "(s) " + (milli%1000) + "(ms)"
    } else {
      (milli / (60*1000)) + "(m) " + ((milli/1000)%60) + "(s)"
    }
  }
  private val Format = new SimpleDateFormat("HH:mm.ss")
  def milliToTimeString(milli:Long) = {
    Format.format(new Date(milli))
  }
  def timeWithInfo[T](f: =>T) = {
    val stopWatch = new Stopwatch
    val result = f
    (TimingInfo(stopWatch.startTime, stopWatch.currentTime), result)
  }
}

case class TimingInfo(startTime:Long, endTime:Long) {
  val timeTaken = (endTime - startTime) / 1000000
  val timeTakenInMilliSeconds : Double = (endTime - startTime) / 1000000.0 
}
