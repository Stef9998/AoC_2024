package stef9998.aoc2024

// Is used for parsing the input file for Advent of Code 2024.
// Only used for parsing and extracting integers from the input file.
// Not used for creating the data structures.


object FileParser {

  def splitBy(delim: String): String => List[String] = _.split(delim).toList

  val parseInts: String => List[Int] = { line =>
    "\\d+".r.findAllIn(line).map(_.toInt).toList
  }

  val twoIntParser: String => (Int, Int) = line => parseInts(line) match {
    case List(a, b) => (a, b)
    case _ => throw new IllegalArgumentException("Expected two integers")
  }

  /**
   * Parses a list of lines using a provided parser function.
   *
   * @param lines  The input lines as a List[String].
   * @param parser A function that takes a line as String and returns the parsed value.
   * @tparam T The type of the parsed value.
   * @return A list of parsed values of type T.
   */
  def parseLines[T](lines: List[String], parser: String => T): List[T] = {
    lines.map(parser)
  }
  
  /**
   * Splits a list of lines at the first empty line.
   * Returns a tuple: (lines before the empty line, lines after the empty line).
   */
  def splitAtEmptyLine(lines: List[String]): (List[String], List[String]) = {
    val (before, afterWithEmpty) = lines.span(_.nonEmpty)
    val after = afterWithEmpty.dropWhile(_.isEmpty)
    (before, after)
  }

  /**
   * Splits a list of lines at every empty line.
   * Returns a list of list of strings, where each inner list is a block of non-empty lines.
   */
  def splitAtAllEmptyLines(lines: List[String]): List[List[String]] = {
    lines.foldLeft(List(List.empty[String])) { (acc, line) =>
      if (line.isEmpty) acc :+ List.empty[String]
      else acc.init :+ (acc.last :+ line)
    }.filter(_.nonEmpty)
  }

}
