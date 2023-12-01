package com.valevil27.advent2023

import scala.io.Source

object Star01 extends App:
  val lines = Source.fromFile("./Day1/a01").getLines.toList
  def extractNumbers(lines: Seq[String]) =
    for line <- lines yield
      val digits = line.filter(_.isDigit)
      (digits.head.toString + digits.last.toString).toInt
  @main def solve1 =
    println(extractNumbers(lines).sum)

object Star02 extends App:
  val mapNumbers = Map(
    "one" -> "1",
    "two" -> "2",
    "three" -> "3",
    "four" -> "4",
    "five" -> "5",
    "six" -> "6",
    "seven" -> "7",
    "eight" -> "8",
    "nine" -> "9"
  )
  def parseNumbers(line: String) =
    var result = line
    for (k, v) <- mapNumbers do result = result.replaceAll(k, k + v + k)
    result
  @main def solve2 =
    println(Star01.extractNumbers(Star01.lines.map(parseNumbers)).sum)
