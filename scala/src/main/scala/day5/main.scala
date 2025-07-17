package stef9998.aoc2024
package day5

import scala.annotation.tailrec

@main
def main(): Unit =
  val sampleLines = ReadInput.readIn(5, "sample")
  val lines = ReadInput.readIn(5, "input")

  def run(lines: List[String], partFunction: (List[(Int, Int)], List[List[Int]]) => Int) = {
    val sampleLinesSplit = FileParser.splitAtEmptyLine(lines)
    val sampleLinesParsed = Tuple2(
      FileParser.parseLines(sampleLinesSplit._1, FileParser.twoIntParser), FileParser.parseLines(sampleLinesSplit._2, FileParser.parseInts)
    )
    partFunction.tupled(sampleLinesParsed)
  }

  val sample1: Int = run(sampleLines, part1)
  assert(sample1 == 143)
  val result1: Int = run(lines, part1)
  println(result1)
  assert(result1 == 6505)

  val sample2: Int = run(sampleLines, part2)
  println(sample2)
  assert(sample2 == 123)
  val result2: Int = run(lines, part2)
  println(result2)
  assert(result2 == 6897)


def part1(constrains: List[(Int, Int)], lines: List[List[Int]]): Int = {
  lines.map(p1calc(_, constrains)).sum
}

def correctOrder(line: List[Int], constrains: List[(Int, Int)]) = {
  val constrainSet: Set[(Int, Int)] = constrains.toSet

  @tailrec
  def helper(element: Int, remaining: List[Int]): Boolean = {
    remaining match {
      case List() => true
      case list =>
        val wrongOrder = list.exists { nextElement => constrainSet.contains((nextElement, element)) }
        if wrongOrder then false else helper(list.head, list.tail)
    }
  }

  helper(line.head, line.tail)
}

def p1calc(line: List[Int], constrains: List[(Int, Int)]): Int = {
  if correctOrder(line, constrains) then line((line.length - 1) / 2) else 0
}


def part2(constrains: List[(Int, Int)], lines: List[List[Int]]): Int = {
  val customOrdering = partialOrderingFromConstraints(constrains)
  val linesToCorrect = lines.filter(line => !correctOrder(line, constrains))

  linesToCorrect.map(p2calc(_, customOrdering)).sum
}

def p2calc(line: List[Int], ordering: Ordering[Int]): Int = {
  val sortedLine = line.sorted(ordering)
  sortedLine((sortedLine.length - 1) / 2)
}

/**
 * Creates a partial ordering from a list of constraints.
 *
 * @param constraints List of pairs representing constraints (a, b) where a comes before b.
 * @tparam T The type of the elements in the constraints.
 * @return An Ordering[T] that defines the partial order based on the constraints.
 */
def partialOrderingFromConstraints[T](constraints: List[(T, T)]): Ordering[T] = {

  val constraintMap: Map[T, List[T]] = constraints.groupMap(_._1)(_._2)

  (a: T, b: T) =>
    if (constraintMap.getOrElse(a, List()).contains(b)) -1
    else if (constraintMap.getOrElse(b, List()).contains(a)) 1
    else 0
}
