package com.valevil27.advent2023.Day03

import scala.io.Source
import scala.util.boundary
import math.{max, min}
import scala.collection.mutable.ArrayBuffer

object Star05:
  class Symbol(var symbol: Char = ' ', var row: Int = 0, var col: Int = 0):
    var Numbers = List[Number]()
    override def toString(): String = s"$symbol"
    override def equals(x: Any): Boolean = x match
      case s: Symbol => s.symbol == symbol && s.col == col && s.row == row
      case _         => false

  class Number(
      val value: Int,
      val row: Int,
      val startIndex: Int,
      val endIndex: Int
  ) {
    var symbolList = List[Symbol]()
    var hasSymbol = false
    def hasSurround(
        lines: Seq[String],
        totalSymbols: ArrayBuffer[Symbol] = ArrayBuffer[Symbol]()
    ): Boolean =
      val fromRow = max(row - 1, 0)
      val toRow = min(row + 1, lines.length - 1)
      val fromIndex = max(startIndex - 1, 0)
      val toIndex = min(lines(0).length - 1, endIndex + 1)
      for r <- fromRow to toRow; i <- fromIndex to toIndex do
        lines(r)(i) match
          case c if c.isDigit => {}
          case c if c == '.'  => {}
          case c: Char =>
            hasSymbol = true
            val symbol = Symbol(c, r, i)
            if totalSymbols.contains(symbol) then
              val s = totalSymbols.find(_ == symbol).get
              s.Numbers = s.Numbers :+ this
            else
              symbol.Numbers = symbol.Numbers :+ this
              totalSymbols += symbol
      hasSymbol
    override def toString(): String =
      s"- $value, row: $row, col: $startIndex to $endIndex, hasSymbol: $hasSymbol"
  }

  val input = Source.fromFile("./Day03/a03").getLines().toVector
  val testInput = Source.fromFile("./Day03/a03test").getLines().toVector
  def getNumbers(line: String, row: Int): List[Number] =
    var retList = List[Number]()
    var current = ""
    for i <- 0 until line.length do
      line(i): Char match
        case d if d.isDigit =>
          current += d
          if i == line.length - 1 then
            retList =
              retList :+ Number(current.toInt, row, i - current.length + 1, i)
            current = ""
        case _ if current != "" =>
          retList =
            retList :+ Number(current.toInt, row, i - current.length, i - 1)
          current = ""
        case _ =>
    retList
  @main def solve05 =
    val listNumbers = input.zipWithIndex.flatMap((s, r) => getNumbers(s, r))
    val solution = listNumbers.filter(_.hasSurround(input)).foldLeft(0)(_ + _.value)
    println(solution)

object Star06:
  import Star05.{Number, Symbol, getNumbers}
  def getGears(input: Vector[String]) =
    var totalSymbols = ArrayBuffer[Symbol]()
    val listNumbers = input.zipWithIndex.flatMap((s, r) => getNumbers(s, r))
    listNumbers.filter(_.hasSurround(input, totalSymbols))
    totalSymbols
      .filter(_.symbol == '*')
      .filter(_.Numbers.length == 2)
      .map(_.Numbers.map(_.value).product)
      .sum

  @main def solve06 =
    val solution = getGears(Star05.input)
    println(solution)
