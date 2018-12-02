package com.desdesdes

import scala.collection.mutable.ListBuffer
import scala.io.StdIn

object Day2 {

  def main(args: Array[String]): Unit = {
    val input: List[String] = inputReader()
    println(s"Checksum: ${calculateChecksum(input)}")
    println(s"Common names: ${findCommonNames(input)}")
  }

  def inputReader(): List[String] = {
    val fullInput: ListBuffer[String] = new ListBuffer[String]()
    var input: String = ""
    while (input != "q") {
      input = StdIn.readLine()
      if (input != "q") fullInput += input
    }
    fullInput.toList
  }

  def calculateChecksum(boxNames: List[String]): Int = {
    val repeatedTwice = getBoxNamesWithRepeatedLetters(boxNames, 2)
    val repeatedThreeTimes = getBoxNamesWithRepeatedLetters(boxNames, 3)
    repeatedTwice.size * repeatedThreeTimes.size
  }

  def getBoxNamesWithRepeatedLetters(boxNames: List[String], repeatCount: Int): List[String] = {
    boxNames.filter(name => {
      name.toCharArray.groupBy(_.toString).exists(_._2.size == repeatCount)
    })
  }

  def findCommonNames(boxNames: List[String]): String = {
    val similarNames = boxNames.filter(name => {
      boxNames.count(innerName => isOnlyOneDifferentChar(name, innerName)) > 0
    })
    replaceDifferentChars(similarNames)
  }

  def isOnlyOneDifferentChar(first: String, second: String): Boolean = {
    var differentCharsAmount: Int = 0
    for (i <- 0 until first.length) {
      if (first(i) != second(i)) differentCharsAmount += 1
    }
    differentCharsAmount == 1
  }

  def replaceDifferentChars(boxNames: List[String]): String = {
    var result: StringBuilder = new StringBuilder
    val firstName = boxNames.head
    for (i <- 0 until firstName.length) {
      if (boxNames.forall(_.charAt(i) == firstName.charAt(i))) result += firstName.charAt(i)
    }
    result.toString()
  }

}
