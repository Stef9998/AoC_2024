package stef9998.aoc2024
package day17

class Parser {
  def parseInput(lines: List[String]): (Map[String, Int], List[Int]) = {
    val registerPattern = """Register (\w): (\d+)""".r
    val programPattern = """Program: (.*)""".r

    val registers = lines.collect {
      case registerPattern(name, value) => name -> value.toInt
    }.toMap

    val program = lines.collectFirst {
      case programPattern(nums) => nums.split(",").map(_.trim.toInt).toList
    }.getOrElse(Nil)

    (registers, program)
  }
}
