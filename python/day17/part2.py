from functools import lru_cache
from typing import Optional

from day17.computer_architecture import ComputerState, Register
from day17.computer_cpu import get_next_state, get_next_instruction
from day17.utils import parse_input, does_program_halt
from file_handling import get_specific_file_as_lines, get_file_as_lines


def main(lines: list[str]) -> int:
    initial_state, program_codes = parse_input(lines)
    operation_codes = tuple(program_codes)

    register_a = 0
    while True:
        if register_a == 320_000:
            break
        if register_a % 100_000 == 0:
            print(f"Register A init: {register_a}")
        modified_state = initial_state.get_with_modified_register(Register.A, register_a)
        if solve_recursively(modified_state, operation_codes):
            break
        register_a += 1
    return register_a

def solve_recursively(initial_state: ComputerState, operation_codes: tuple[int, ...]) -> bool:

    def recursive_function(state: ComputerState, op_index_to_output: int) -> bool:
        if halt_program(operation_codes, op_index_to_output, state.IP) is not None:
            return halt_program(operation_codes, op_index_to_output, state.IP)
        new_state, output = get_next_state(state, get_next_instruction(operation_codes, state.IP))
        if output is None:
            return recursive_function(new_state, op_index_to_output)
        if output != operation_codes[op_index_to_output]:
            return False
        return recursive_function(new_state, op_index_to_output + 1)

    return recursive_function(initial_state, 0)


@lru_cache()
def halt_program(operation_codes, op_index_to_output, state_ip) -> Optional[bool]:
    # print(halt_program.cache_info())
    if does_program_halt(state_ip, operation_codes):
        if state_ip == op_index_to_output:
            return True
        else:
            return False
    return None


if __name__ == '__main__':

    import time

    result = main(get_specific_file_as_lines('sample_input_p2.txt'))
    print(f"Sample one result:\n{result}")
    assert result == 117440

    start_time = time.time()
    result = main(get_file_as_lines())
    print(f"Part one result:\n{result}")
    print(f"Time taken: {time.time() - start_time:.2f} seconds")
