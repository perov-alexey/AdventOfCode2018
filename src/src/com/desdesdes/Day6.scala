package com.desdesdes

import scala.collection.mutable.ListBuffer
import scala.io.StdIn

object Day6 {

  def main(args: Array[String]): Unit = {
    val coordinates: List[Coordinate] = readCoordinates()
    val distanceMap: Array[Array[Coordinate]]= buildDistanceMap(coordinates)
    printDistanceMap(distanceMap)
  }

  def readCoordinates(): List[Coordinate] = {
    val coordinates: ListBuffer[Coordinate] = new ListBuffer[Coordinate]
    var input: String = ""
    while (input != "q") {
      input = StdIn.readLine()
      if (input != "q") {
        val coordinateTuple: Array[Int] = input.split(", ").map(_.toInt)
        coordinates += new Coordinate(coordinateTuple(0), coordinateTuple(1), coordinates.length, 0)
      }
    }
    coordinates.toList
  }

  def buildDistanceMap(coordinates: List[Coordinate]): Array[Array[Coordinate]] = {
    val width: Int = coordinates.maxBy(_.x).x
    val height: Int = coordinates.maxBy(_.y).y
    var distanceMap: Array[Array[Coordinate]]  = Array.ofDim[Coordinate](width + 1, height + 1)

    coordinates.foreach(coordinate => {
      distanceMap = fillDistanceMap(distanceMap, coordinate)
    })

    distanceMap = removeInfinityAreas(distanceMap)

    distanceMap
  }

  def removeInfinityAreas(distanceMap: Array[Array[Coordinate]]): Array[Array[Coordinate]] = {
    for (x <- distanceMap.indices) {
        if (distanceMap(x)(0).number != -1) {
//          distanceMap.flatten.filter(_.number == distanceMap(x)(0).number).map(coordinate => new Coor)
        }
    }
    distanceMap
  }

  def fillDistanceMap(distanceMap: Array[Array[Coordinate]], newCoordinate: Coordinate): Array[Array[Coordinate]] = {
    for (i <- distanceMap(0).indices) {
      for (j <- distanceMap.indices) {
        val coordinate = distanceMap(j)(i)

        val distance: Int = newCoordinate.distanceTo(new Coordinate(j, i, -1, -1))

        if (coordinate == null || coordinate.value > distance) {
          distanceMap(j)(i) = new Coordinate(j, i, newCoordinate.number, distance)
        } else if (coordinate.value == distance) {
          distanceMap(j)(i) = new Coordinate(j, i, -1, distance)
        }
      }
    }

    distanceMap
  }

  def printDistanceMap(distanceMap: Array[Array[Coordinate]]): Unit = {
    for (i <- distanceMap(0).indices) {
      for (j <- distanceMap.indices) {
        val element = distanceMap(j)(i)
        if (element == null) {
          print(s"  *  \t")
        } else {
          print(s"(${element.number} ${element.value})\t")
        }
      }
      println()
    }
  }

  def clearScreen(): Unit = {
    for (i <- 1 to 20) { println() }
  }

}

class Coordinate(val x: Int, val y: Int, val number: Int, val value: Int) {

  def distanceTo(other: Coordinate): Int = {
    Math.abs(this.x - other.x) + Math.abs(this.y - other.y)
  }

}
