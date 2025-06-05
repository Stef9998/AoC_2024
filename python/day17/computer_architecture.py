import dataclasses
from functools import lru_cache, cache
from dataclasses import dataclass
from enum import Enum


number_of_instructions = 8

class DataIO(Enum):
    Operand = 0
    Register = 1
    Output = 2


class Register(Enum):
    A = 0
    B = 1
    C = 2
    IP = 3


@dataclass(frozen=True)
class ComputerState:
    A: int
    B: int
    C: int
    IP: int

    key_mapping = {
        Register.A: 'A',
        Register.B: 'B',
        Register.C: 'C',
        Register.IP: 'IP',
    }

    def get_register_value(self, register: Register) -> int:
        key = self.key_mapping[register]
        return getattr(self, key)

    def get_with_modified_register(self, register: Register, value: int) -> "ComputerState":
        key = self.key_mapping[register]
        return dataclasses.replace(self, **{key: value})


class OperandType(Enum):
    NOT_USED = 0
    LITERAL = 1
    COMBO = 2

    @lru_cache(3)
    def get_operand_value_location(self):
        @lru_cache(number_of_instructions)
        def get_literal(operand: int) -> DataIO:
            assert 0 <= operand < 8
            return DataIO.Operand

        @lru_cache(number_of_instructions)
        def get_combo_input_location(operand: int) -> None | Register | DataIO:
            assert 0 <= operand < 8
            if operand <= 3:
                return DataIO.Operand
            if operand == 4:
                return Register.A
            if operand == 5:
                return Register.B
            if operand == 6:
                return Register.C
            if operand == 7:
                raise ValueError("Combo Operand should never have the value 7 in a valid program")

        if self is OperandType.LITERAL:
            return get_literal
        if self is OperandType.COMBO:
            return get_combo_input_location
        if self is OperandType.NOT_USED:
            return get_literal


class Instruction(Enum):
    ADV = (0, OperandType.COMBO)
    BXL = (1, OperandType.LITERAL)
    BST = (2, OperandType.COMBO)
    JNZ = (3, OperandType.LITERAL)
    BXC = (4, OperandType.NOT_USED)
    OUT = (5, OperandType.COMBO)
    BDV = (6, OperandType.COMBO)
    CDV = (7, OperandType.COMBO)

    def __init__(self, op_code: int, operand_type: OperandType):
        self.op_code = op_code
        self.operand_type = operand_type
        assert len(type(self)) != number_of_instructions


    @staticmethod
    @lru_cache(number_of_instructions)
    def get_by_op_code(op_code: int):
        for instruction in Instruction:
            if instruction.op_code == op_code:
                return instruction
        raise ValueError(f"No Instruction found with op_code: {op_code}")


class InstructionFunctions:
    @staticmethod
    @lru_cache()
    def division_by_power_of_two(numerator: int, denominator: int) -> int:
        return numerator >> denominator

    @staticmethod
    @lru_cache()
    def bitwise_xor(first: int, second: int) -> int:
        return first ^ second

    @staticmethod
    @lru_cache()
    def modulo(dividend: int, divider: int) -> int:
        return dividend % divider

    @staticmethod
    @lru_cache()
    def modulo8(dividend: int, not_used: int = 8) -> int:
        return InstructionFunctions.modulo(dividend, 8)

    @staticmethod
    @lru_cache()
    def return_parameter(parameter: int, not_used: int = 0) -> int:
        return parameter

    instruction_function_mapper = {
        Instruction.ADV: division_by_power_of_two,
        Instruction.BXL: bitwise_xor,
        Instruction.BST: modulo8,
        Instruction.JNZ: return_parameter,
        Instruction.BXC: bitwise_xor,
        Instruction.OUT: modulo8,
        Instruction.BDV: division_by_power_of_two,
        Instruction.CDV: division_by_power_of_two,
    }

    @classmethod
    @lru_cache(number_of_instructions)
    def get_instruction_function(cls, instruction: Instruction) -> callable:
        return cls.instruction_function_mapper[instruction]

    instruction_output_mapper = {
        Instruction.ADV: Register.A,
        Instruction.BXL: Register.B,
        Instruction.BST: Register.B,
        Instruction.JNZ: Register.IP,
        Instruction.BXC: Register.B,
        Instruction.OUT: DataIO.Output,
        Instruction.BDV: Register.B,
        Instruction.CDV: Register.C,
    }

    @classmethod
    @lru_cache(number_of_instructions)
    def get_instruction_output(cls, instruction: Instruction):  # TODO rename goal name
        return cls.instruction_output_mapper[instruction]

    instruction_input_mapper = {
        Instruction.ADV: (Register.A, DataIO.Operand),
        Instruction.BXL: (Register.B, DataIO.Operand),
        Instruction.BST: (DataIO.Operand, 8),
        Instruction.JNZ: (DataIO.Operand, 0),
        Instruction.BXC: (Register.B, Register.C),
        Instruction.OUT: (DataIO.Operand, 8),
        Instruction.BDV: (Register.A, DataIO.Operand),
        Instruction.CDV: (Register.A, DataIO.Operand),
    }

    @classmethod
    @lru_cache(number_of_instructions)
    def get_instruction_input(cls, instruction: Instruction):
        return cls.instruction_input_mapper[instruction]


if __name__ == '__main__':
    print("Computer architecture file run.")
