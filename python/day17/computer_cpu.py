from collections.abc import Callable
from functools import lru_cache, cache
from typing import Optional

from day17.computer_architecture import ComputerState, Register, DataIO, Instruction, InstructionFunctions


# TODO cache:
#  - split in the beginning state up, and then use a, b, c, ip only which needed.
#  - that way I might be able to cache more


# @lru_cache(None)
def get_next_state(state: ComputerState, operation: tuple[int, int]) -> tuple[ComputerState, Optional[int]]:
    # print(get_next_state.cache_info())
    op_code, raw_operand = operation

    # Architecture Values (Which Input/Output location, ...)
    input_locations, operation_function, operation_value_location_calculator, output_location = build_pipeline(op_code)

    operand_value_location = operation_value_location_calculator(raw_operand)

    # Data-Flow of the concrete data
    operand_value = get_operand_value(operand_value_location, raw_operand, state)
    first_value, second_value = get_instruction_input_values(input_locations, operand_value, state)
    operation_return_value = operation_function(first_value, second_value)

    next_state = get_new_state(operation_return_value, output_location, state)
    next_output = get_new_output(operation_return_value, output_location)
    return next_state, next_output

@cache
def build_pipeline(op_code):
    instruction = Instruction.get_by_op_code(op_code)
    op_type = instruction.operand_type
    operation_value_location_calculator = op_type.get_operand_value_location()
    input_locations = InstructionFunctions.get_instruction_input(instruction)
    operation_function = InstructionFunctions.get_instruction_function(instruction)
    output_location = InstructionFunctions.get_instruction_output(instruction)
    return input_locations, operation_function, operation_value_location_calculator, output_location


# @lru_cache(None)
def get_operand_value(input_location, operand, state):
    if input_location is DataIO.Operand:
        return operand
    elif input_location in Register:
        return state.get_register_value(input_location)
    else:
        raise ValueError("Wrong input location. Needs to be Register or Operand!")


# # @lru_cache(None)
# def get_instruction_input_values(input_locations, operand_value, state) -> tuple[int, int]:
#     return (operand_value if input_locations[0] == DataIO.Operand else
#             state.get_register_value(input_locations[0]),
#             operand_value if input_locations[1] == DataIO.Operand else
#             state.get_register_value(input_locations[1]) if isinstance(input_locations[1], Register) else 0)


# @lru_cache(None)
def get_instruction_input_values(input_locations, operand_value, state) -> tuple[int, int]:
    return (operand_value, 0) if input_locations[0] == DataIO.Operand else\
        (state.get_register_value(input_locations[0]),
    (operand_value if input_locations[1] == DataIO.Operand else state.get_register_value(input_locations[1]) ))


def get_instruction_input_values_(input_locations, operand_value, state) -> tuple[int, int]:
    return (get_instruction_input_values_pipe(input_locations[0])(operand_value, state),
            get_instruction_input_values_pipe(input_locations[1])(operand_value, state))

# @lru_cache(8)
@cache
def get_instruction_input_values_pipe(input_location):
    if input_location == DataIO.Operand:
        return lambda operand_value, state: operand_value
    elif input_location is Register.A or input_location is Register.B or input_location is Register.C:
        return lambda operand_value, state: state.get_register_value(input_location)
    else:
        return lambda operand_value, state: 0
    # return lambda operand_value, state: operand_value if input_location == DataIO.Operand else \
    #     state.get_register_value(input_location) if isinstance(input_location, Register) else 0


def get_new_state(output_value: int, output_location: Register | DataIO, state: ComputerState) -> ComputerState:

    new_ip = get_new_ip(output_value, output_location, state.IP, state.A != 0)

    pipe_abc_function = get_pipe_abc_function(output_location)
    return ComputerState(*pipe_abc_function(output_value, state.A, state.B, state.C), new_ip)


# @lru_cache(6)
@cache
def get_pipe_abc_function(output_location: Register | DataIO ) -> Callable[[int, int, int, int], tuple[int, int, int]]:
    if output_location is Register.A:
        return lambda output_value, state_a, state_b, state_c: (output_value, state_b, state_c)
    elif output_location is Register.B:
        return lambda output_value, state_a, state_b, state_c: (state_a, output_value, state_c)
    elif output_location is Register.C:
        return lambda output_value, state_a, state_b, state_c: (state_a, state_b, output_value)
    else:
        return lambda output_value, state_a, state_b, state_c: (state_a, state_b, state_c)

# @cache
def get_new_ip(output_value: int, output_location: Register | DataIO, state_ip, state_a_not_zero):
    if output_location is Register.IP and state_a_not_zero:
        return output_value
    return state_ip + 2

def get_new_output(output_value: int, output_location: Register | DataIO) -> Optional[int]:
    return output_value if output_location is DataIO.Output else None


@cache
def get_next_instruction(operation_codes: tuple[int, ...], instruction_pointer: int) -> tuple[int, int]:
    return operation_codes[instruction_pointer], operation_codes[instruction_pointer + 1]
