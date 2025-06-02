from day17.computer_architecture import ComputerState, Register
from day17.computer_cpu import get_next_state, get_next_instruction
from day17.utils import parse_input, does_program_halt
from file_handling import get_specific_file_as_lines, get_file_as_lines


def main(lines: list[str]) -> str:

    initial_state, program_codes = parse_input(lines)
    operation_codes = list(program_codes)

    register_a = 0
    while True:
        if register_a % 1000 == 0:
            print(f"Register A init: {register_a}")
        modified_state = initial_state.get_with_modified_register(Register.A, register_a)
        if solve_recursively(modified_state, operation_codes):
            break
        register_a += 1
    return register_a

def solve_recursively(initial_state: ComputerState, operation_codes: list[int]) -> bool:

    def recursive_function(state: ComputerState, op_index_to_output: int) -> bool:
        if does_program_halt(state.IP, operation_codes):
            if state.IP == op_index_to_output:
                return True
            else:
                return False
        new_state, output = get_next_state(state, get_next_instruction(operation_codes, state.IP))
        if output is None:
            return recursive_function(new_state, op_index_to_output)
        if output != operation_codes[op_index_to_output]:
            return False
        return recursive_function(new_state, op_index_to_output + 1)

    return recursive_function(initial_state, 0)


if __name__ == '__main__':

    import time

    result = main(get_specific_file_as_lines('sample_input_p2.txt'))
    print(f"Sample one result:\n{result}")
    assert result == 117440

    start_time = time.time()
    result = main(get_file_as_lines())
    print(f"Part one result:\n{result}")
    print(f"Time taken: {time.time() - start_time:.2f} seconds")
