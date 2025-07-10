package stef9998.aoc2024
package day2

@main
def main(): Unit =
  val sampleLines = ReadInput.readIn(2, "sample")
  val lines = ReadInput.readIn(2, "input")
  val sampleParsed = FileParser.parseLines(sampleLines, FileParser.parseInts)
  val linesParsed = FileParser.parseLines(lines, FileParser.parseInts)

  val sample1: Int = part1(sampleParsed)
  assert(sample1 == 2)
  val result1: Int = part1(linesParsed)
  assert(result1 == 490)
  println(result1)

  val sample2: Int = part2(sampleParsed)
  assert(sample2 == 4)
  val result2: Int = part2(linesParsed)
  assert(result2 == 536)
  println(result2)


def part1(parsedLines: List[List[Int]]) = {
  parsedLines.map(isMonotonicDiffAtMost3).count(identity)
}

def isMonotonicDiffAtMost3(lst: List[Int]): Boolean = {
  if (lst.isEmpty) return false
  if (lst.length == 1) return true

  val increasing = lst.sliding(2).forall { case List(a, b) => a < b }
  val decreasing = lst.sliding(2).forall { case List(a, b) => a > b }

  if (increasing) lst.sliding(2).forall { case List(a, b) => b - a <= 3 }
  else if (decreasing) lst.sliding(2).forall { case List(a, b) => a - b <= 3 }
  else false
}

def part2(parsedLines: List[List[Int]]) = {
  parsedLines.map { lst =>
    val variants = lst.indices.map(i => lst.patch(i, Nil, 1)) :+ lst
    variants.exists(isMonotonicDiffAtMost3)
  }.count(identity)
}
