package stef9998.aoc2024

import scala.io.Source
import scala.util.Try


object ReadInput {
  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
      println("Usage: ReadInput <dayNumber> [<filename>]")
      sys.exit(1)
    }

    val dayNumber = args(0).toIntOption.getOrElse {
      println("Invalid day number. Please provide a valid integer.")
      sys.exit(1)
    }
    val filename = args(1)
    val lines: List[String] = readIn(dayNumber, filename)
    lines.foreach(println)
  }

  /**
   * Attempts to read the input file for a given Advent of Code day and filename, returning a Try-wrapped list of lines.
   *
   * @param dayNumber The day number (1-24).
   * @param filename  The input file name (default: "input").
   * @return Try[List[String]] containing the file lines or a failure.
   */
  def readInFunc(dayNumber: Int, filename: String = "input"): Try[List[String]] = Try {
    readIn(dayNumber, filename)
  }

  /**
   * Reads the input file for a given Advent of Code day and filename.
   *
   * @param dayNumber The day number (1-24).
   * @param filename  The input file name (default: "input").
   * @return List[String] containing the file lines.
   * @throws IllegalArgumentException if dayNumber is not between 1 and 24.
   */
  def readIn(dayNumber: Int, filename: String = "input"): List[String] = {
    if (dayNumber <= 0 || dayNumber > 24) throw new IllegalArgumentException("Day number must be between 1 and 24")
    val filepath = s"C:\\Users\\sreichel\\git\\AoC_2024\\input\\day$dayNumber\\$filename.txt"
    readLines(filepath)
  }

  /**
   * Reads all lines from a file.
   * This is a private helper method that reads all lines from the specified file.
   * It is used internally by the readIn method to fetch the content of the input file.
   * It is not functional but throws Exceptions if there is an error reading the file.
   *
   * @param filename The path to the file.
   * @return List[String] containing all lines in the file.
   * @throws java.io.IOException if the file cannot be read.
   */
  private def readLines(filename: String): List[String] = {
    val source = Source.fromFile(filename)
    try source.getLines().toList finally source.close()
  }

}
