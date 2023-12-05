package com.valevil27.advent2023.Day04

import scala.io.Source

val input = Source.fromFile("./Day04/a04").getLines.toList
val testInput = Source.fromFile("./Day04/a04test").getLines.toList

object Star07:
  def extractWiningNumbers(line: String) =
    line
      .split(':')(1)
      .split('|')(0)
      .split(' ')
      .filter(!_.isBlank())
      .map(_.trim.toInt)
  def extractCardNumbers(line: String) =
    line
      .split(':')(1)
      .split('|')(1)
      .split(' ')
      .filter(!_.isBlank())
      .map(_.trim.toInt)

  def getPoints(wining: Seq[Int], card: Seq[Int]) =
    card.count(wining.contains(_)) match
      case i if i == 0 => 0
      case i           => 1 << i - 1

  def solve(input: Seq[String]) =
    input.map { line =>
      val wining = extractWiningNumbers(line)
      val card = extractCardNumbers(line)
      getPoints(wining, card)
    }.sum
  @main def solve07 =
    println(solve(input))

object Star08:
  import Star07.{extractCardNumbers, extractWiningNumbers}
  def extractCardID(line: String) =
    line.split(':')(0).split(' ').filter(!_.isBlank)(1).toInt
  class Card(val int: Int, val winning: Seq[Int], val card: Seq[Int]):
    var quantity = 1
    def getExtras = card.count(winning.contains(_))

  def solve(input: Seq[String]) =
    val cards = input.map(line =>
      Card(
        extractCardID(line),
        extractWiningNumbers(line),
        extractCardNumbers(line)
      )
    )
    var total = 0
    for (card, i) <- cards.zipWithIndex do
      total += card.quantity
      if card.getExtras != 0 then
        (1 to card.getExtras).foreach(j =>
          cards(i + j).quantity += card.quantity
        )
    total

  @main def solve08 =
    println(solve(input))
