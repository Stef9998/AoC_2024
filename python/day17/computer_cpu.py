from typing import Optional

from day17.computer_architecture import ComputerState, Register, DataIO, Instruction, InstructionFunctions


def get_next_state(state: ComputerState, operation: tuple[int, int]) -> tuple[ComputerState, Optional[int]]:
    op_code, raw_operand = operation

    # Architecture Values (Which Input/Output location, ...)
    instruction = Instruction.get_by_op_code(op_code)
    op_type = instruction.operand_type
    operation_value_location_calculator = op_type.get_operant_value_location()
    input_locations = InstructionFunctions.get_instruction_input(instruction)
    operation_function = InstructionFunctions.get_instruction_function(instruction)
    output_location = InstructionFunctions.get_instruction_output(instruction)

    operand_value_location = operation_value_location_calculator(raw_operand)

    # Data-Flow of the concrete data
    operand_value = get_operand_value(operand_value_location, raw_operand, state)
    first_value, second_value = get_instruction_input_values(input_locations, operand_value, state)
    operation_return_value = operation_function(first_value, second_value)

    next_state = get_new_state(operation_return_value, output_location, state)
    next_output = get_new_output(operation_return_value, output_location)
    return next_state, next_output


def get_operand_value(input_location, operand, state):
    if input_location is DataIO.Operand:
        return operand
    elif input_location in Register:
        return state.get_register_value(input_location)
    else:
        raise ValueError("Wrong input location. Needs to be Register or Operand!")


def get_instruction_input_values(input_locations, operand_value, state) -> tuple[int, int]:
    return (operand_value if input_locations[0] == DataIO.Operand else
            state.get_register_value(input_locations[0]) if isinstance(input_locations[0], Register) else 0,
            operand_value if input_locations[1] == DataIO.Operand else
            state.get_register_value(input_locations[1]) if isinstance(input_locations[1], Register) else 0)


def get_new_state(output_value: int, output_location: Register | DataIO, state: ComputerState) -> ComputerState:
    if output_location is DataIO.Output:
        return state.get_with_modified_register(Register.IP, state.IP + 2)
    if output_location not in Register:
        raise ValueError("Wrong output location. Needs to be Register or Output!")

    if output_location is Register.IP:
        if state.get_register_value(Register.A) != 0:
            return state.get_with_modified_register(Register.IP, output_value)
        else:
            return state.get_with_modified_register(Register.IP, state.IP + 2)

    return state.get_with_modified_register(output_location, output_value).get_with_modified_register(Register.IP, state.IP + 2)


def get_new_output(output_value: int, output_location: Register | DataIO) -> Optional[int]:
    return output_value if output_location is DataIO.Output else None


def get_next_instruction(operation_codes: list[int], instruction_pointer: int) -> tuple[int, int]:
    return operation_codes[instruction_pointer], operation_codes[instruction_pointer + 1]
