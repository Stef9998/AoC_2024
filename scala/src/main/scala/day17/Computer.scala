package stef9998.aoc2024
package day17

import scala.collection.mutable.ArrayBuffer

class Computer(registerA: Long, registerB: Int, registerC: Int, program: List[Int]) {
  protected var regA: Long = registerA
  protected var regB: Long = registerB
  protected var regC: Long = registerC
  protected var instructionPointer: Int = 0
  protected val output: ArrayBuffer[Int] = ArrayBuffer.empty[Int]

  def step(): Boolean = {
    if (instructionPointer < 0 || instructionPointer >= program.length) {
      return false
    }
    val opCode = program(instructionPointer)
    val operant = program(instructionPointer + 1)
    calcStep(opCode, operant)
    true
  }

  def calcStep(opCode: Int, operant: Int): Unit = {
    lazy val comboValue = operant match {
      case 0 | 1 | 2 | 3 => operant
      case 4 => regA.toInt
      case 5 => regB.toInt
      case 6 => regC.toInt
      case 7 => throw new IllegalArgumentException(s"Operant Value 7 as combo value is reserved and cannot be used")
      case _ => throw new IllegalArgumentException(s"Operant Value must be between 0 and 7, but was $operant")
    }
    opCode match {
      case 0 => // ADV
        regA = regA >> comboValue
      case 1 => // BXL
        regB = regB ^ operant
      case 2 => // BST
        regB = comboValue % 8
      case 3 => // JNZ
        regA match {
          case 0 => // do nothing
          case _ => instructionPointer = operant - 2
        }
      case 4 => // BXC
        regB = regB ^ regC
      case 5 => // OUT
        output += comboValue % 8
      case 6 => // BDV
        regB = regA >> comboValue
      case 7 => // CDV
        regC = regA >> comboValue
      case _ => throw new IllegalArgumentException(s"op-code must be between 0 and 7, but was: $opCode")
    }
    instructionPointer += 2
  }

  def getState(): String = {
    s"""RegisterA: ${regA}
       |RegisterB: ${regB}
       |RegisterC: ${regC}
       |InstructionPointer: ${instructionPointer}
       |Output: ${output.mkString(",")}
       |""".stripMargin
  }

  def printoutState(): Unit = {
    println(getState())
  }

  def getOutput(): List[Int] = {
    output.toList
  }

}
