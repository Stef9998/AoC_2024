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

  val sample1: String = runP1(sampleLines, part1)
  assert(sample1 == results("part1")("sample"))
  val result1: String = runP1(lines, part1)
  println(result1)
  assert(result1 == results("part1")("input"))

//  val sample2 = runP2(sampleLinesPart2, part2)
//  println(sample2)
//  assert(sample2 == results("part2")("sample").asInstanceOf[Int].toLong)
//  val result2 = runP2(lines, part2)
//  println(result2)
//  assert(result2 == results("part2")("input").asInstanceOf[Int].toLong)


def part1(computer: Computer): String = {
  while (computer.step()) {
  }
  computer.getOutput().mkString(",")
}
