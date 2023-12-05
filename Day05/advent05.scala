package com.valevil27.advent2023.Day05

import io.Source

val input = Source.fromFile("./Day05/a05").getLines.toList
val testInput = Source.fromFile("./Day05/a05test").getLines.toList

object Star09:
  def separateMaps(lines: Seq[String]) =
    var ret = List[List[String]]()
    var current = List[String]()
    lines foreach { l =>
      if l.isBlank then
        ret = ret :+ current
        current = List()
      else if l(0).isDigit then current = current :+ l
    }
    (ret :+ current).filter(!_.isEmpty)

  def parseMaps(list: Seq[String]) =
    list.map(_.split(" ").map(_.toLong).take(3))

  def parseSeeds(line: String) = line.split(" ").tail.map(_.toLong)

  def passThroughMap(from: Long, map: Seq[Long]) =
    val Seq(destStart, sourceStart, range) = map
    val idx = from - sourceStart
    if idx >= 0 && idx < range then idx + destStart
    else -1

  def passThroughState(from: Long, state: Seq[Array[Long]]) =
    state.map(passThroughMap(from, _)).filter(_ >= 0).headOption.getOrElse(from)

  def solve(input: Seq[String]) =
    val totalMaps = separateMaps(input)
    val seeds = parseSeeds(input(0))
    val states = totalMaps.map(parseMaps)
    seeds.map { s =>
      var input = s
      states.foreach(state => input = passThroughState(input, state))
      input
    }

  @main def solve09 =
    println(solve(input).min)

object Star10:
  ???
