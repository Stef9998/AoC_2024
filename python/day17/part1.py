from day17.computer_architecture import ComputerState
from day17.computer_cpu import get_next_state, get_next_instruction
from day17.utils import parse_input, CpuMachine, does_program_halt
from file_handling import get_specific_file_as_lines, get_file_as_lines


def main(lines: list[str]) -> str:

    initial_state, program_codes = parse_input(lines)
    operation_codes = list(program_codes)

    # output = solve_with_while(CpuMachine(initial_state, operation_codes))
    output = solve_recursively(initial_state, operation_codes)
    return ','.join(str(num) for num in output)

def solve_recursively(initial_state: ComputerState, operation_codes: list[int]) -> list[int]:

    def recursive_function(state: ComputerState) -> list[int]:
        if does_program_halt(state.IP, operation_codes):
            return []
        new_state, output = get_next_state(state, get_next_instruction(operation_codes, state.IP))
        found_output = [output] if output is not None else []
        return found_output + recursive_function(new_state)

    return recursive_function(initial_state)

def solve_with_while(cpu) -> list[int]:
    output: list[int] = []
    while not cpu.does_program_halt():
        instruction_output = cpu.calculate_next_step()
        if instruction_output is not None:
            output.append(instruction_output)
    return output


if __name__ == '__main__':

    import time

    result = main(get_specific_file_as_lines('sample_input.txt'))
    print(f"Sample one result:\n{result}")
    assert result == "4,6,3,5,6,3,5,2,1,0"

    start_time = time.time()
    result = main(get_file_as_lines())
    print(f"Part one result:\n{result}")
    assert result == "1,5,0,3,7,3,0,3,1"
    print(f"Time taken: {time.time() - start_time:.2f} seconds")
