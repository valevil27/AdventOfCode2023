package com.valevil27.advent2023.Day03

import scala.io.Source
import scala.util.boundary

object Star05:
  class Symbol(var symbol: Char = ' ', var row: Int = 0, var col: Int = 0)
  class Number(
      val value: Int,
      val row: Int,
      val startIndex: Int,
      val endIndex: Int
  ) {
    var symbol = Symbol()
    def hasSurround(lines: Seq[String]): Boolean =
      val fromRow = if row > 0 then row - 1 else row
      val toRow = if row < lines.length - 1 then row + 1 else row
      val fromIndex = if startIndex > 0 then startIndex - 1 else startIndex
      val toIndex =
        if endIndex < lines(0).length - 1 then endIndex + 1 else endIndex
      var has = false
      boundary:
        for r <- fromRow to toRow; i <- fromIndex to toIndex do
          lines(r)(i) match
            case c if c.isDigit => {}
            case c if c == '.'  => {}
            case c: Char =>
              has = true
              symbol = Symbol(c,r,i)
              boundary.break()
      has
    override def toString(): String =
      s"- $value, row: $row, col: $startIndex to $endIndex, symbol: $symbol"
  }
  val input = Source.fromFile("./Day03/a03").getLines().toVector
  def getNumbers(line: String, row: Int): List[Number] =
    var retList = List[Number]()
    var current = ""
    for i <- 0 until line.length do
      line(i): Char match
        case d if d.isDigit =>
          current += d
          if i == line.length - 1 then
            retList =
              retList :+ Number(current.toInt, row, i - current.length+1, i)
            current = ""
        case _ if current != "" =>
          retList =
            retList :+ Number(current.toInt, row, i - current.length, i - 1)
          current = ""
        case _ =>
    retList
  @main def solve05 =
    val listNumbers = input.zipWithIndex.flatMap((s, r) => getNumbers(s, r))
    println(listNumbers.filter(_.hasSurround(input)).foldLeft(0)(_ + _.value))

object Star06:
  val listNumbers: Vector[Star05.Number] = Star05.input.zipWithIndex.flatMap((s, r) => Star05.getNumbers(s, r))
  listNumbers.filter(_.symbol.symbol == '*')