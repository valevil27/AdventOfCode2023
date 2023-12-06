package com.valevil27.advent2023.Day06

import io.Source
import math.{sqrt, pow, BigInt}

val input = Source.fromFile("./Day06/a06").getLines.toList
val testInput = Source.fromFile("./Day06/a06test").getLines.toList

def solveSecondGrade(a: Double, b: Double, c: Double) =
  val disc = b * b - 4 * a * c
  Array( (-b - sqrt(disc))/2, (-b + sqrt(disc))/2)

object Star11:
  def getNumbers(line: String) =
    line.split(':')(1).trim.split(' ').filter(!_.isBlank()).map(_.trim.toDouble)
  def solve(input: List[String]) =
    val List(times, dist) = input.map(getNumbers(_))
    val res = for (t, d) <- times.zip(dist) yield
      val tlimit = solveSecondGrade(1, -t, d)
      math.ceil(tlimit(1)) - math.floor(tlimit(0) + 1).toLong
    println(res.product)
  @main def solve11 =
    solve(input)
object Star12:
  def getNumbers(line: String) = line.split(':')(1).replace(" ", "").toDouble
  def solve(input: List[String]) =
    val List(time, dist) = input.map(getNumbers(_))
    val tlimit = solveSecondGrade(1, -time, dist)
    math.ceil(tlimit(1)) - math.floor(tlimit(0) + 1).toLong
  @main def solve12 =
    println(solve(input))
