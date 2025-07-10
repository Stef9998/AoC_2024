package stef9998.aoc2024
package day3

@main
def main(): Unit =
  val sampleLines = ReadInput.readIn(3, "sample")
  val lines = ReadInput.readIn(3, "input")
  val sampleParsed = parse(sampleLines)
  val linesParsed = parse(lines)

  val sample1: Int = part1(sampleParsed)
  assert(sample1 == 161)
  val result1: Int = part1(linesParsed)
  assert(result1 == 179571322)
  println(result1)

  val p2sampleLines = ReadInput.readIn(3, "sample_p2")
  val sampleParsed2 = parse2(p2sampleLines)
  val linesParsed2 = parse2(lines)

  val sample2: Int = part2(sampleParsed2)
  assert(sample2 == 48)
  val result2: Int = part2(linesParsed2)
  assert(result2 == 103811193)
  println(result2)


def part1(parsedLines: List[String]) = {
  multInput(parsedLines).sum
}

def part2(parsedLines: List[String]): Int = {
  var active = true
  parsedLines.foldLeft(0) { (sum, instr) =>
    instr match {
      case "do()"     => active = true; sum
      case "don't()"  => active = false; sum
      case s if s.startsWith("mul") && active =>
        parseMult(s).getOrElse(0) + sum
      case _ => sum
    }
  }
}

def parseMult(s: String): Option[Int] = {
  val pattern = raw"mul\((\d+),(\d+)\)".r
  s match {
    case pattern(a, b) => Some(a.toInt * b.toInt)
    case _ => None
  }
}

def multInput(multiplications: List[String]): List[Int] = {
  multiplications.map(parseMult(_).getOrElse(0))
}

def parse(lines: List[String]): List[String] = {
  FileParser.parseLines(lines, instructionParser).flatten
}

def instructionParser(line: String): List[String] = {
  val pattern = raw"mul\(\d+,\d+\)".r
  pattern.findAllIn(line).toList
}

def parse2(lines: List[String]): List[String] = {
  FileParser.parseLines(lines, instructionParser2).flatten
}

def instructionParser2(line: String): List[String] = {
  val pattern = raw"mul\(\d+,\d+\)|do\(\)|don't\(\)".r
  pattern.findAllIn(line).toList
}