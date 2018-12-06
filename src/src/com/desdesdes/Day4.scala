package com.desdesdes

import java.text.SimpleDateFormat
import java.time.Duration
import java.util.Date

import scala.collection.mutable.ListBuffer
import scala.io.StdIn

object Day4 {

  def main(args: Array[String]): Unit = {
    val guards: List[Guard] = readGuards()
    guards.foreach(guard => println(s"Guard slept ${guard.number} ${guard.sleepTime}"))
    val laziestGuard = guards.maxBy(_.sleepTime)
    println(s"Best minute to sneak: ${laziestGuard.mostPossibleMinuteToSneak}")
    println(s"Result: ${laziestGuard.number * laziestGuard.mostPossibleMinuteToSneak._1}")

    val mostSleepyGuard = guards.maxBy(_.mostPossibleMinuteToSneak._2)
    println(s"Result #2: ${mostSleepyGuard.number * mostSleepyGuard.mostPossibleMinuteToSneak._1}")

  }

  def readGuards(): List[Guard] = {
    var messages: ListBuffer[String] = new ListBuffer[String]()
    var input: String = ""
    while (input != "q") {
      input = StdIn.readLine()
      if (input != "q") {
        messages += input
      }
    }

    messages = messages.sorted

    val guards: collection.mutable.Map[Int, Guard] = collection.mutable.Map[Int, Guard]()

    var currentGuardNumber: Int = 0

    val guardMessagePattern = """Guard #(\d+).*""".r
    val recordPattern = """\[(.{16})\] (.*)""".r
    val dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm")

    for (message <- messages) {
      val recordPattern(date, action) = message
      action match {
        case s if s.matches(guardMessagePattern.toString()) => {
          if (guards.nonEmpty) {
            guards(currentGuardNumber).closeShift(dateFormat.parse(date))
          }

          val guardMessagePattern(newGuardNumber) = s
          currentGuardNumber = newGuardNumber.toInt

          val guard: Guard = guards.getOrElseUpdate(currentGuardNumber, new Guard(currentGuardNumber))
          guard.openShift(dateFormat.parse(date))
        }
        case s if s == "falls asleep" => {
          guards(currentGuardNumber).sleep(dateFormat.parse(date))
        }
        case s if s == "wakes up" => {
          guards(currentGuardNumber).awake(dateFormat.parse(date))
        }
        case _ => println("!!! INVALID INPUT !!!")
      }
    }

    guards.values.toList
  }

}

class Guard(val number: Int) {

  var shifts: ListBuffer[Shift] = new ListBuffer[Shift]

  def openShift(date: Date): Unit = {
    val newShift = new Shift()
    newShift.start = date

    shifts += newShift
  }

  def closeShift(date: Date): Unit = {
    shifts.last.end = date
  }

  def sleep(date: Date): Unit = {
    val lastShift = shifts.last

    if (lastShift.sleepSessions.isEmpty || lastShift.sleepSessions.last.end != null) {
      lastShift.sleepSessions += new SleepSession(date)
    }

  }

  def awake(date: Date): Unit = {
    val lastShift = shifts.last
    lastShift.sleepSessions.last.end = date
  }

  def sleepTime: Long = {
    var result: Long = 0
    for (shift <- shifts) {
      for (sleepSession <- shift.sleepSessions) {
        result += Duration.between(sleepSession.start.toInstant, sleepSession.end.toInstant).toMinutes
      }
    }
    result
  }

  def mostPossibleMinuteToSneak: (Int, Int) = {
    val occurrence: Array[Int] = new Array[Int](60)
    val minutesTemplate = new SimpleDateFormat("mm")
    for (shift <- shifts) {
      for (sleepSession <- shift.sleepSessions) {
        val startMinute: Int = minutesTemplate.format(sleepSession.start).toInt
        val endMinute: Int = minutesTemplate.format(sleepSession.end).toInt

        for (i <- startMinute until endMinute) {
          occurrence(i) = occurrence(i) + 1
        }
      }
    }
    (occurrence.indexOf(occurrence.max), occurrence.max)
  }



}

class Shift() {

  var start: Date = _
  var end: Date = _
  var sleepSessions: ListBuffer[SleepSession] = new ListBuffer[SleepSession]

}

class SleepSession(val start: Date) {

  var end: Date = _

}