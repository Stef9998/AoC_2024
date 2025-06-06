import day21.utils as utils
from day21.keypad import get_shortest_path, PadType
from file_handling import get_specific_file_as_lines, get_file_as_lines


def main(lines: list[str]) -> int:
    _inputs = lines
    # return sum(calc_result_one_code(_keys_to_press) for _keys_to_press in _inputs)
    return_sum = 0
    for _keys_to_press in _inputs:
        one_res = calc_result_one_code(_keys_to_press)
        if _keys_to_press == "029A":
            assert one_res == 68*29
        elif _keys_to_press == "980A":
            assert one_res == 60*980
        elif _keys_to_press == "179A":
            assert one_res == 68*179
        elif _keys_to_press == "456A":
            assert one_res == 64*456
        elif _keys_to_press == "379A":
            assert one_res == 64*379
        return_sum += one_res
    return return_sum



def calc_result_one_code(numpad_keys: str) -> int:
    number = utils.get_number_from_numpad(numpad_keys)
    pressed_keys = get_d3_keys(numpad_keys)
    keypress_length = len(pressed_keys)
    calculation = number * keypress_length
    print(f"Keys: {numpad_keys}\tKeypresses: {pressed_keys}")
    print(f"Number: {number}\tLength: {keypress_length}\tCalculation: {calculation}")
    # exit(0)
    return calculation


def get_d3_keys(keys: str) -> str:
    movements = movements_from_keys(keys)
    # print(movements)
    d1_keys = keys_to_press_(movements, PadType.NumPad)
    print(d1_keys)
    d1_movements = movements_from_keys(d1_keys)
    # print(d1_movements)
    d2_keys = keys_to_press_(d1_movements, PadType.DirectionPad)
    print(d2_keys)
    d2_movements = movements_from_keys(d2_keys)
    # print(d2_movements)
    d3_keys = keys_to_press_(d2_movements, PadType.DirectionPad)
    print(d3_keys)
    return d3_keys


def movements_from_keys(d1_keys):
    return list(zip('A' + d1_keys, d1_keys))


def keys_to_press_(movements, pad_type):
    import itertools
    return ''.join(itertools.chain.from_iterable(
        sorted((element.value for element in _list), key=custom_sorter) + ['A'] for _list in
        (get_shortest_path(pad_type, start, end) for start, end in movements)
    ))

def custom_sorter(val):
    order = {'>': 0, 'v': 1, '^': 2, '<': 3}
    return order.get(val, 99)


if __name__ == '__main__':
    import time

    result = main(get_specific_file_as_lines('sample_input.txt'))
    print(f"Sample result:\n{result}")
    assert result == 126384

    # start_time = time.time()
    # result = main(get_file_as_lines())
    # print(f"Part one result:\n{result}")
    # assert result > 103026
    # print(f"Time taken: {time.time() - start_time:.2f} seconds")
