from day24.utils import parse
from file_handling import get_file_as_lines, get_specific_file_as_lines


def main(lines: list[str]) -> int:
    inputs, wires, output_names, gates, gate_output_dict = parse(lines)
    calc_gates = gate_calculator(inputs, wires, gate_output_dict)

    outputs = {output: calc_gates(output) for output in output_names}

    return int(''.join(str(outputs[key]) for key in sorted(outputs, reverse=True)), 2)


def gate_calculator(inputs: dict, wires: dict, gate_output_dict: dict):
    def calc_gate_output(output):
        return calc_gate_func(output)

    def calc_gate_func(wire):
        gate = gate_output_dict[wire]
        input1 = calc_gate_input(gate.input_1)
        input2 = calc_gate_input(gate.input_2)
        return gate.function(input1, input2)

    def calc_gate_input(_input):
        if _input in inputs:
            assert inputs[_input] is not None, f"inputs dictionary value for {_input} should never be None"
            return inputs[_input]
        assert _input in wires, f"Input key {_input} not found in wires or inputs"

        return calc_gate_func(_input) if wires[_input] is None else wires[_input]

    return calc_gate_output


if __name__ == '__main__':
    import time

    result = main(get_specific_file_as_lines('sample_input.txt'))
    print(f"Sample one result:\n{result}")
    assert result == 2024

    start_time = time.time()
    result = main(get_file_as_lines())
    print(f"Part one result:\n{result}")
    assert result == 65740327379952
    print(f"Time taken: {time.time() - start_time:.3f} seconds")
