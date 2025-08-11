package stef9998.aoc2024
package day7

val dayNumber: Int = 7
val results = Map(
  "part1" -> Map("sample" -> 3749, "input" -> 850435817339L),
  "part2" -> Map("sample" -> 11387, "input" -> 104824810233437L)
)

@main
def main(): Unit =
  val sampleLines = ReadInput.readIn(dayNumber, "sample")
  val lines = ReadInput.readIn(dayNumber, "input")

  def run(lines: List[String], partFunction: List[(Long, List[Long])] => Long) = {
    val splitLines = lines.map(FileParser.splitBy(":"))
    val dataLines = splitLines.map(line => Tuple2(line(0).toLong, FileParser.parseInts(line(1)).map(_.toLong)))
    partFunction(dataLines)
  }

  val sample1 = run(sampleLines, part1)
  assert(sample1 == results("part1")("sample"), s"Part1 Sample: $sample1, expected = ${results("part1")("sample")}")
  val result1 = run(lines, part1)
  println(result1)
  assert(result1 == results("part1")("input"), s"Part1 Run: $result1, expected = ${results("part1")("input")}")

  val sample2 = run(sampleLines, part2)
  assert(sample2 == results("part2")("sample"), s"Part2 Sample: $sample2, expected = ${results("part2")("sample")}")
  val result2 = run(lines, part2)
  println(result2)
  assert(result2 == results("part2")("input"), s"Part2 Run: $result2, expected = ${results("part2")("input")}")


def part1(lines: List[(Long, List[Long])]): Long = {
  val operators: List[(Long, Long) => Long] = List(_ + _, _ * _)
  lines.map(calc(_, operators)).sum
}


def calc(line: (Long, List[Long]), operators: List[(Long, Long) => Long]) = {
  val testValue = line._1
  val currentCalc = line._2.head
  val numbersToStillCalc = line._2.tail

  def recSolve(currentValue: Long, remainingNumbers: List[Long]): Boolean = {
    remainingNumbers match {
      case Nil => currentValue == testValue
      case head :: tail =>
        val nextValues = operators.map(op => recSolve(op(currentValue, head), tail))
        nextValues.exists(identity)
    }
  }

  if recSolve(currentCalc, numbersToStillCalc) then testValue else 0
}


def part2(lines: List[(Long, List[Long])]): Long = {
  val concat: (Long, Long) => Long = (a, b) => (a.toString + b.toString).toLong
  val operators: List[(Long, Long) => Long] = List(_ + _, _ * _, concat(_, _))
  lines.map(calc(_, operators)).sum
}
