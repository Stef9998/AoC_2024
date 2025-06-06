from functools import lru_cache

import day21.utils as utils
from day21.keypad import get_all_shortest_path, PadType
from file_handling import get_specific_file_as_lines, get_file_as_lines


def main(lines: list[str]) -> int:
    _inputs = lines
    return sum(calc_result_one_code(_keys_to_press) for _keys_to_press in _inputs)


def calc_result_one_code(numpad_keys: str) -> int:
    number = utils.get_number_from_numpad(numpad_keys)
    pressed_keys = get_d3_keys(numpad_keys)
    keypress_length = len(pressed_keys)
    calculation = number * keypress_length
    # print(f"Keys: {numpad_keys}\tKeypresses: {pressed_keys}")
    # print(f"Number: {number}\tLength: {keypress_length}\tCalculation: {calculation}")
    # print()
    return calculation


def get_d3_keys(keys: str) -> str:
    movements = movements_from_keys(keys)
    return ''.join(d1_calc(move) for move in movements)


def d1_calc(move: tuple[str, str]):
    possible_paths = get_all_shortest_path(PadType.NumPad, move[0], move[1])
    possible_keys_to_press = [''.join((step.value for step in possible_path)) + 'A' for possible_path in possible_paths]
    possible_movements = []
    for possible_key_to_press in possible_keys_to_press:
        movements = movements_from_keys(possible_key_to_press)
        foo = ''.join(d2_calc(move) for move in movements)
        possible_movements.append(foo)
    return min(possible_movements, key=len)


@lru_cache()
def d2_calc(move: tuple[str, str]):
    possible_paths = get_all_shortest_path(PadType.DirectionPad, move[0], move[1])
    possible_keys_to_press = [''.join((step.value for step in possible_path)) + 'A' for possible_path in possible_paths]
    possible_movements = []
    for possible_key_to_press in possible_keys_to_press:
        movements = movements_from_keys(possible_key_to_press)
        foo = ''.join(d3_calc(move) for move in movements)
        possible_movements.append(foo)
    return min(possible_movements, key=len)


@lru_cache()
def d3_calc(move: tuple[str, str]):
    possible_paths = get_all_shortest_path(PadType.DirectionPad, move[0], move[1])
    possible_keys_to_press = [''.join((step.value for step in possible_path)) + 'A' for possible_path in possible_paths]
    return possible_keys_to_press[0]


def movements_from_keys(d1_keys: str):
    return list(zip('A' + d1_keys, d1_keys))


if __name__ == '__main__':
    import time

    result = main(get_specific_file_as_lines('sample_input.txt'))
    print(f"Sample result:\n{result}")
    assert result == 126384

    start_time = time.time()
    result = main(get_file_as_lines())
    print(f"Part one result:\n{result}")
    assert result > 103026
    assert result < 126384
    print(f"Time taken: {time.time() - start_time:.2f} seconds")
