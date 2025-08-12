package stef9998.aoc2024
package day17

class TestableComputer(registerA: Long, registerB: Int, registerC: Int, program: List[Int])
  extends Computer(registerA, registerB, registerC, program) {

  private var outputIndex: Int = 0

  override def step(): Boolean = {
    if (instructionPointer < 0 || instructionPointer >= program.length) {
      return false
    }
    val opCode = program(instructionPointer)
    val operant = program(instructionPointer + 1)
    val instructionOutput = calcStepWithOutput(opCode, operant) // This will return Some(value) if the OUT instruction was executed, otherwise None

    instructionOutput match {
      case Some(value) =>
        if (outputIndex >= program.length) {
          return false
        }
        val newOutput = instructionOutput.get
        if (newOutput != program(outputIndex)) {
          return false
        }
        outputIndex += 1
        true
      case None => true
    }
  }

  def calcStepWithOutput(opCode: Int, operant: Int): Option[Int] = {
    lazy val comboValue = calcComboValue(operant)
    runStep(opCode, operant, comboValue)
    instructionPointer += 2

    if (opCode == 5) Some(comboValue % 8)
    else None
  }


}