package stef9998.aoc2024
package day17

val dayNumber: Int = 17
val results = Map(
  "part1" -> Map("sample" -> "4,6,3,5,6,3,5,2,1,0", "input" -> "1,5,0,3,7,3,0,3,1"),
  "part2" -> Map("sample" -> 117440, "input" -> 0)
)

@main
def main(): Unit =

  val sampleLines = ReadInput.readIn(dayNumber, "sample")
  val lines = ReadInput.readIn(dayNumber, "input")
  val sampleLinesPart2 = ReadInput.readIn(dayNumber, "sample_p2")


  def runP1(lines: List[String], partFunction: (Computer) => String) = {
    val sampleData = Parser().parseInput(lines)
    val computer = Computer(sampleData._1("A"), sampleData._1("B"), sampleData._1("C"), sampleData._2)
    partFunction(computer)
  }

  def runP2(lines: List[String], partFunction: ((Map[String, Int], List[Int])) => Long) = {
    val sampleData = Parser().parseInput(lines)
    partFunction(sampleData)
  }

//  val sample1: String = runP1(sampleLines, part1)
//  assert(sample1 == results("part1")("sample"))
//  val result1: String = runP1(lines, part1)
//  println(result1)
//  assert(result1 == results("part1")("input"))

  val sample2 = runP2(sampleLinesPart2, part2)
  println(sample2)
  assert(sample2 == results("part2")("sample").asInstanceOf[Int].toLong)
  val result2 = runP2(lines, part2)
  println(result2)
  assert(result2 == results("part2")("input").asInstanceOf[Int].toLong)


def runTillHaltGetOutput(computer: Computer): String = {
  while (computer.step()) {
  }
  computer.getOutput().mkString(",")
}

def part2(config: (Map[String, Int], List[Int])): Long = {
  val program = config._2
  val programOutput = program.mkString(",")
  var i = 0L
  while (true) {
    if (i % 100_000_000 == 0) {
      println(s"Trying with A = ${"%,d".format(i/1000000)} million")
    }
    val computer: Computer = TestableComputer(i, config._1("B"), config._1("C"), program)
    if (runTillHaltGetOutput(computer) == programOutput) {
      return i
    }
    i += 1
  }
  throw new IllegalArgumentException("No valid value found for part 2")
}
