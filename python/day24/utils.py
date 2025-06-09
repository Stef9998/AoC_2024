import dataclasses
from collections.abc import Callable

from file_handling import get_file_as_lines, get_specific_file_as_lines

gate_function = {
    'AND': lambda a, b: a & b,
    'OR': lambda a, b: a | b,
    'XOR': lambda a, b: a ^ b,
}


@dataclasses.dataclass(frozen=True)
class Gate:
    gate_name: str
    gate_input_1: str
    gate_input_2: str
    gate_output: str
    gate_function: Callable = dataclasses.field(init=False)

    def __post_init__(self):
        object.__setattr__(self, 'gate_function', gate_function[self.gate_name])


def parse(lines: list[str]):
    split_index = lines.index("")
    inputs_def, gates_def = lines[:split_index], lines[split_index + 1:]

    inputs = {line.split(": ")[0]: int(line.split(": ")[1]) for line in inputs_def}
    wires, outputs, gates, gate_output_dict = parse_gates(gates_def)

    print()


def parse_gates(gates_lines: list[str]):
    wires = dict()
    outputs = dict()
    gates = set()
    gate_output_dict = dict()
    for line in gates_lines:
        gate_and_input, output = line.split(" -> ")
        input1, gate_name, input2 = gate_and_input.split(" ")
        for wire in [input1, input2, output]:
            if wire[0] == 'z':
                outputs[wire] = None
            else:
                wires[wire] = None
        gate = Gate(gate_name, input1, input2, output)
        gates.add(gate)
        gate_output_dict[output] = gate
    return wires, outputs, gates, gate_output_dict


if __name__ == '__main__':
    # result = parse(get_file_as_lines())
    result = parse(get_specific_file_as_lines('sample_input.txt'))
    print(f"Part one result:\n{result}")
