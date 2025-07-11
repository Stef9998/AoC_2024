package stef9998.aoc2024
package day4

@main
def main(): Unit =
  val sampleLines = ReadInput.readIn(4, "sample")
  val lines = ReadInput.readIn(4, "input")

  val sample1: Int = part1(sampleLines)
  assert(sample1 == 18)
  val result1: Int = part1(lines)
  assert(result1 == 2654)
  println(result1)

  val sample2: Int = part2(sampleLines)
  assert(sample2 == 9)
  val result2: Int = part2(lines)
  assert(result2 == 1990)
  println(result2)


def part1(lines: List[String]): Int = {
  val horizontalXmas = lines.map(horizontalXmasFound)
  val zip4 = lines.sliding(4).toList.map { case List(a, b, c, d) => (a, b, c, d) }
  val verticalXmas = zip4.map(VerticalCalc)
  val crossXmas = zip4.map(crossCalc)
  horizontalXmas.sum + verticalXmas.sum + crossXmas.sum
}

def horizontalXmasFound(line: String): Int = {
  line.sliding(4).count(s => s == "XMAS" || s == "SAMX")
}

def VerticalCalc(line: (String, String, String, String)): Int = {
  val (a, b, c, d) = line
  val slidings: List[String] =
    a.zip(b).zip(c).zip(d).map { case (((a1, b1), c1), d1) => s"$a1$b1$c1$d1" }.toList
  slidings.map(s => s == "XMAS" || s == "SAMX").count(identity)
}

def crossCalc(line: (String, String, String, String)): Int = {
  val (a, b, c, d) = line
  val slidings: List[(String, String, String, String)] =
    a.sliding(4).toList
      .zip(b.sliding(4).toList)
      .zip(c.sliding(4).toList)
      .zip(d.sliding(4).toList)
      .map { case (((a1, b1), c1), d1) => (a1, b1, c1, d1) }
  slidings.map(cross).sum
}

def cross(fourXfour: (String, String, String, String)): Int = {
  val (a, b, c, d) = fourXfour
  val xmasStrings = List(
    "" + a(0) + b(1) + c(2) + d(3), ("" + a(0) + b(1) + c(2) + d(3)).reverse,
    "" + a(3) + b(2) + c(1) + d(0), ("" + a(3) + b(2) + c(1) + d(0)).reverse,
  )
  xmasStrings.count(_ == "XMAS")
}

def part2(lines: List[String]): Int = {
  val zip3 = lines.sliding(3).toList.map { case List(a, b, c) => (a, b, c) }
  val zip3x3 = zip3.flatMap(get3x3)
  zip3x3.map(masCross).count(identity)
}

def masCross(pack3x3: (String, String, String)): Boolean = {
  val (a, b, c) = pack3x3
  if (b(1) != 'A') return false

  val s1 = s"${a(0)}${c(2)}"
  val s2 = s"${a(2)}${c(0)}"
  (s1 == "MS" || s1 == "SM") && (s2 == "MS" || s2 == "SM")

}

def get3x3(lines: (String, String, String)): List[(String, String, String)] = {
  val (a, b, c) = lines
  val slidings: List[(String, String, String)] =
    a.sliding(3).toList
      .zip(b.sliding(3).toList)
      .zip(c.sliding(3).toList)
      .map { case ((a1, b1), c1) => (a1, b1, c1) }
  slidings
}
