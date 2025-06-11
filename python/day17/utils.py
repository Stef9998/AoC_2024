from typing import Optional

from day17.computer_architecture import ComputerState
from day17.computer_cpu import get_next_state, get_next_instruction


def parse_input(lines: list[str]):
    register_values: tuple[int,int,int] = tuple(int(line.split(': ')[1]) if "Register" in line else -1 for line in lines[0:3])
    initial_state = ComputerState(*register_values, 0)
    op_codes = map(int, lines[4].split(": ")[1].split(","))
    return initial_state, op_codes


class CpuMachine:
    state: ComputerState
    operation_codes: list[int]

    def __init__(self, state: ComputerState, operation_codes: list[int]):
        self.state = state
        self.operation_codes = operation_codes

    def calculate_next_step(self) -> Optional[int]:
        if self.state.IP + 1 >= len(self.operation_codes):
            raise ValueError("Instruction Pointer can't be outside of OP-Codes Array")
        self.state, output = get_next_state(self.state, get_next_instruction(self.operation_codes, self.state.IP))
        return output

    def does_program_halt(self) -> bool:
        return does_program_halt(self.state.IP, self.operation_codes)


def does_program_halt(instruction_pointer, operation_codes) -> bool:
    if instruction_pointer + 1 >= len(operation_codes):
        return True
    return False


if __name__ == '__main__':
    import time

    start_time = time.time()
    # result = function_(input_as_lines())
    # print(f"Part one result:\n{result}")
    print(f"Time taken: {time.time() - start_time:.3f} seconds")

