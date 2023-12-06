package com.valevil27.advent2023.Day06

object Alternative:
  def solve11(input: List[String]) =
    def getNumbers(line: String) =
      line.split(':')(1).trim.split(' ').filter(!_.isBlank()).map(_.trim.toLong)
    val List(times, distances) = input.map(getNumbers(_))
    times.zip(distances).map((t,d) => ways(t,d)).product
    
  
  def solve12(input: List[String]) =
    def getNumbers(line: String) = line.split(':')(1).replace(" ", "").toLong
    val List(time, distance) = input.map(getNumbers(_))
    ways(time, distance)

  def ways(t: Long, d: Long) =
    var count = 0L
    val races = (0L to t).foreach(i => 
      count += (if (t-i)*i > d then 1L else 0L)
      )
    count
  
  @main def solve = 
    println(solve11(input))
    println(solve12(input))

