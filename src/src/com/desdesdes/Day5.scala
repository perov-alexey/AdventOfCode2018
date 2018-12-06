package com.desdesdes

import scala.io.StdIn

object Day5 {

  def main(args: Array[String]): Unit = {
    val rawPolymer: String = StdIn.readLine()
    val polymer: String = react(rawPolymer)
    println(polymer)
    println(s"Result #1: ${polymer.length}")
    println(s"Result #2: ${findStablePolymer(rawPolymer).length}")
  }

  def react(polymer: String): String = {
    var result: String = ""

    var newPolymer = ""
    var skipNext: Boolean = false

    // So ugly :(
    for (i <- 0 until polymer.length) {
     if (skipNext) {
       skipNext = false
     } else if (i == polymer.length - 1 && !skipNext) {
       newPolymer = newPolymer + polymer(i)
     } else if (isReact(polymer(i), polymer(i + 1))) {
       skipNext = true
     } else {
       newPolymer = newPolymer + polymer(i)
     }
    }

    if (newPolymer != polymer) {
      newPolymer = react(newPolymer)
    }
    newPolymer
  }

  def isReact(first: Char, second: Char): Boolean = {
    (first.isLower && second.isUpper && first.toString.equalsIgnoreCase(second.toString)) ||
      (first.isUpper && second.isLower && first.toString.equalsIgnoreCase(second.toString))
  }

  def findStablePolymer(polymer: String): String = {
    val units: Array[Char] = polymer.toLowerCase().distinct.toCharArray
    val unstableUnit: Char = units.minBy(unit => react(polymer.replaceAll(unit.toString, "").replaceAll(unit.toString.toUpperCase, "")).length)
    react(polymer.replaceAll(unstableUnit.toString, "").replaceAll(unstableUnit.toString.toUpperCase, ""))
  }

}
