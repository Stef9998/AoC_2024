package stef9998.aoc2024
package day1

@main
def main(): Unit =
  val lines = ReadInput.readIn(1, "input")
  val linesAsNumbers = FileParser.parseLines(lines, FileParser.twoIntParser)

  val result1: Int = part1(linesAsNumbers)
  assert(result1 == 1580061)
  println(result1)

  val sample2: Int = part2(FileParser.parseLines(ReadInput.readIn(1, "sample"), FileParser.twoIntParser))
  assert(sample2 == 31)
  val result2: Int = part2(linesAsNumbers)
  assert(result2 == 23046913)
  println(result2)


import scala.math.abs

def part1(parsedLines: List[(Int, Int)]) = {
  val (first, second) = parsedLines.unzip
  val zipped = first.sorted.zip(second.sorted)
  val difference = zipped.map { case (a, b) => abs(a - b) }
  difference.sum
}

def part2(parsedLines: List[(Int, Int)]) = {
  val (first, second) = parsedLines.unzip
  val counts = second.groupBy(identity).view.mapValues(_.size).toMap
  val numberTimesAppearances = first.map(x => counts.getOrElse(x, 0) * x)
  numberTimesAppearances.sum
}