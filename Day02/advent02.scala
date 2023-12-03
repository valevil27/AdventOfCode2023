package com.valevil27.advent2023.Day02

import io.Source
object Star03 extends App:
  val lines = Source.fromFile("./Day02/a02").getLines.toList
  type Throw = (Int, String)
  def getBalls(line: String): Array[Throw] =
      line
        .split(": ")(1) // Get the game info
        .split(";") // Separate different throws
        .flatMap(l => l.split(", ")) // List with each ball we dont care what throw it is
        .map(_.strip.split(" ")) // Separate ball number and color
        .map(a => (a(0).toInt, a(1))) // Parse number to Int

  def possibleGame(game: Seq[Throw]) =
    val bag = Map(
      "red" -> 12,
      "green" -> 13,
      "blue" -> 14
    )
    game.forall(t => t(0) <= bag(t(1)))

  def getValidIds(games: Seq[String]) =
    val validIds =
      for (game, id) <- games.zipWithIndex
      yield
        if possibleGame(getBalls(game)) then Some(id + 1)
        else None
    validIds.flatten
  @main def solve03 =
    println(getValidIds(lines).sum)

object Star04 extends App:
  def getBalls(line: String) =
    line
      .split(": ")(1) // Get the game info
      .split(";") // Separate different throws
      .flatMap(l => l.split(", ")) // List with each ball we dont care what throw it is
      .map(_.strip.split(" ")) // Separate ball number and color
      .groupBy(x => x(1)) // Group by color
      .mapValues(x => x.map(_(0).toInt).max) // Get max for each color

  @main def solve04 =
    println(Star03.lines.map(getBalls(_).foldLeft(1)((p, n) => p * n._2)).sum)
