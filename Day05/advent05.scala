package com.valevil27.advent2023.Day052

import io.Source
import scala.util.boundary
import scala.collection.mutable.Stack

val input = Source.fromFile("./Day05/a05").getLines.toList
val testInput = Source.fromFile("./Day05/a05test").getLines.toList

type State = List[Array[Long]]

object Star09 extends App:
  def parseSeeds(input: List[String]) =
    input(0).split(": ")(1).split(" ").map(_.toLong)
  def parseMaps(input: List[String]) =
    var ret = List[State]()
    var current: State = List[Array[Long]]()
    for line <- input.filter(!_.isBlank()).drop(1) do
      if line(0).isLetter && !current.isEmpty then
        ret = ret :+ current
        current = List()
      else if line(0).isDigit then
        current = current :+ line.split(" ").map(_.toLong)
    ret :+ current
  def solve(input: List[String]) =
    val seeds = parseSeeds(input)
    val states = parseMaps(input)
    var locs = Array[Long]()
    for seed <- seeds do
      var curr = seed
      for state <- states do
        boundary:
          for Array(d, s, r) <- state do
            if s <= curr && curr < s + r then
              val idx = curr - s
              curr = d + idx
              boundary.break()
      locs = locs :+ curr
    locs.min
  def solve09 = 
    println(solve(input))

object Star10:
  import Star09.{parseSeeds, parseMaps}
  def parseSeedPairs(seeds: Array[Long]) =
    seeds.grouped(2).toArray.map(a => Array(a(0), a(0) + a(1)))
  def solve(input: List[String]) =
    val seeds = parseSeeds(input)
    val seedPairs = parseSeedPairs(seeds)
    val states = parseMaps(input)
    var locs = Array[(Long,Long)]()
    for pair <- seedPairs do
      var remain = Stack[(Long,Long)]((pair(0), pair(1)))
      var res = Array[(Long, Long)]()
      for state <- states do
        while !remain.isEmpty do
          val curr = remain.pop()
          var broken = true
          boundary:
            broken = true
            for Array(d, s, r) <- state do // a = s | b = s + r - 1
              if curr(1) < s || s + r <= curr(0) then {} // x-y-a-b or a-b-x-y
              else if s <= curr(0) && curr(0) <= curr(1) && curr(1) <= s + r // a-x-y-b
              then
                val offset = curr(0) - s
                res = res :+ (d + offset, d + offset + curr(1) - curr(0))
                boundary.break()
              else if curr(0) < s && s <= curr(1) && curr(1) < s+r then // x-a-y-b
                val offset = curr(1) - s
                res = res :+ (d, d + offset)
                remain.push((curr(0), s-1))
                boundary.break()
              else if s <= curr(0) && curr(0) < s+r && s+r < curr(1) then// a-x-b-y
                val offset = curr(0) - s
                res = res :+ (d+offset, d+r-1)
                remain.push((s+r, curr(1)))
                boundary.break()
              else if curr(0) < s && s + r <= curr(1) then // x-a-b-y
                res = res :+ (d, d + r - 1)
                remain.push((curr(0), s-1))
                remain.push((s+r, curr(1)))
                boundary.break()
            broken = false
          if !broken then res = res:+curr
        remain.popAll()
        remain.pushAll(res)
        res = Array()
      locs = locs :++ remain.toArray
    locs.map(_(0)).min
  @main def solve10 = 
    println(solve(input))


              
              
