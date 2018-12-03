package com.desdesdes

import scala.collection.mutable.ListBuffer
import scala.io.StdIn
import util.control.Breaks._

object Day3 {

  def generateFabric(claims: List[Claim]): Fabric = {
    val claimWithBiggestWidth: Claim = claims.sortWith((a, b) => a.x + a.width > b.x + b.width).head
    val claimWithBiggestHeight: Claim = claims.sortWith((a, b) => a.y + a.height > b.y + b.height).head

    val maxWidth: Int = claimWithBiggestWidth.x + claimWithBiggestWidth.width
    val maxHeight: Int = claimWithBiggestHeight.y + claimWithBiggestHeight.height

    val fabric: Fabric = new Fabric(maxWidth, maxHeight)
    claims.foreach(fabric.addClaim)

    fabric
  }

  def main(args: Array[String]): Unit = {
    val claims: List[Claim] = readClaims()
    val fabric: Fabric = generateFabric(claims)

    println(s"Amount of intersected squares: ${fabric.amountOfIntersectedSquares}")
    println(s"Claim number without intersect: ${fabric.claimWithoutIntersection.number}")
  }

  def readClaims(): List[Claim] = {
    val fullInput: ListBuffer[Claim] = new ListBuffer[Claim]()
    var input: String = ""
    val claimPattern = "#(\\d*) @ (\\d*),(\\d*): (\\d*)x(\\d*)".r
    while (input != "q") {
      input = StdIn.readLine()
      if (input != "q") {
        val claimPattern(number, x, y, width, height) = input
        fullInput += new Claim(number.toInt, x.toInt, y.toInt, width.toInt, height.toInt)
      }
    }
    fullInput.toList
  }

}

class Fabric(width: Int, height: Int) {

  val fabric: Array[Array[Int]] = Array.ofDim[Int](width, height)
  val claims: ListBuffer[Claim] = new ListBuffer[Claim]

  def addClaim(claim: Claim): Unit = {
    claims += claim
    for (i <- claim.y until claim.y + claim.height) {
      for (j <- claim.x until claim.x + claim.width) {
        fabric(j)(i) += 1
      }
    }
  }

  def amountOfIntersectedSquares: Int = {
    fabric.flatten.count(_ > 1)
  }

  def claimWithoutIntersection: Claim = {
    claims.filter(!_.isIntersected(this)).head
  }
}

class Claim(val number: Int, val x: Int, val y: Int, val width: Int, val height: Int) {

  def isIntersected(fabric: Fabric): Boolean = {
    var result: Boolean = false
    breakable {
      for (i <- this.y until this.y + this.height) {
        for (j <- this.x until this.x + this.width) {
          if (fabric.fabric(j)(i) > 1) {
            result = true
          }
        }
      }
    }
   result
  }

}
