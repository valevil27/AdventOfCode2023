package com.valevil27.advent2023

import scala.io.Source

object Star01 extends App:
  val exampleStr = List(
    "1abc2" ,
    "pqr3stu8vwx" ,
    "a1b2c3d4e5f" ,
    "treb7uchet" ,
  )
  val lines = Source.fromFile("./a01").getLines.toList
  def extractNumbers(lines: Seq[String]) = 
    for line <- lines yield
      val digits = line.filter(_.isDigit)
      (digits.head.toString + digits.last.toString).toInt
  println(extractNumbers(exampleStr).sum)
  println(extractNumbers(lines).sum)

object Star02 extends App:
  val exampleStr = List(
    "two1nine",
    "eightwothree",
    "abcone2threexyz",
    "xtwone3four",
    "4nineeightseven2",
    "zoneight234",
    "7pqrstsixteen",
  )
  val mapNumbers = Map(
    "one" -> "1",
    "two" -> "2",
    "three" -> "3",
    "four" -> "4",
    "five" -> "5",
    "six" -> "6",
    "seven" -> "7",
    "eight" -> "8",
    "nine" -> "9",
  )
  def parseNumbers(line: String) = 
    var result = line
    for (k,v) <- mapNumbers do
      result = result.replaceAll(k, k+v+k)
    result
  
  val parsedString = exampleStr.map(parseNumbers)
  val parsedNumbers = Star01.extractNumbers(parsedString)
  println(parsedString.mkString("\n"))
  println(parsedNumbers.mkString("\n"))
  println(parsedNumbers.sum)
  println(Star01.extractNumbers(Star01.lines.map(parseNumbers)).sum)