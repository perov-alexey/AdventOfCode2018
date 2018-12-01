package com.desdesdes

import scala.collection.mutable.ListBuffer
import scala.io.StdIn

object Day1 {

  def main(args: Array[String]): Unit = {
    val input: List[Int] = inputReader()
    println(countFrequency(input))
    println(findLoop(input))
  }

  def inputReader(): List[Int] = {
    val fullInput: ListBuffer[Int] = new ListBuffer[Int]()
    val numberRegex = "[+-]{1}\\d+"
    var input: String = ""
    do {
      input = StdIn.readLine()
      if (input.matches(numberRegex)) {
        fullInput += input.toInt
      }
    } while (input != "q")
    fullInput.toList
  }

  def countFrequency(shifts: List[Int]): Int = {
    var shift = 0
    shifts.foreach(shift += _)
    shift
  }

  def findLoop(shifts: List[Int]): Int = {
    var result: Option[Int] = None
    var shift = 0
    val intermediateResults: ListBuffer[Int] = ListBuffer[Int](shift)
    do {
      iterate(shifts, intermediateResults, shift) match {
        case (a, b) => shift = a; result = b
      }
    } while (result.isEmpty)
    result.get
  }

  def iterate(shifts: List[Int], intermediateResults: ListBuffer[Int], initialValue: Int): (Int, Option[Int]) = {
    var result: Int = initialValue
    for (shift <- shifts) {
      result += shift
      if (intermediateResults.contains(result)) {
        return (result, Some(result))
      } else {
        intermediateResults += result
      }
    }
    (result, None)
  }

}
