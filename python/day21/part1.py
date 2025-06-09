from functools import lru_cache

from day21.keypad import PadType
from day21.utils import get_number_from_numpad, get_possible_keys, movements_from_keys
from file_handling import get_specific_file_as_lines, get_file_as_lines


def main(lines: list[str]) -> int:
    _inputs = lines
    return sum(calc_result_one_code(_keys_to_press) for _keys_to_press in _inputs)


def calc_result_one_code(numpad_keys: str) -> int:
    number = get_number_from_numpad(numpad_keys)
    pressed_keys = get_d3_keys(numpad_keys)
    keypress_length = len(pressed_keys)
    return number * keypress_length


def get_d3_keys(keys: str) -> str:
    movements = movements_from_keys(keys)
    return ''.join(d1_calc(move) for move in movements)


def d1_calc(move: tuple[str, str]):
    possible_keys_to_press = get_possible_keys(move, PadType.NumPad)
    possible_movements = [''.join(d2_calc(move) for move in (movements_from_keys(possible_key_to_press)))
                          for possible_key_to_press in possible_keys_to_press]
    return min(possible_movements, key=len)


@lru_cache()
def d2_calc(move: tuple[str, str]):
    possible_keys_to_press = get_possible_keys(move, PadType.DirectionPad)
    possible_movements = [''.join(d3_calc(move) for move in (movements_from_keys(possible_key_to_press)))
                          for possible_key_to_press in possible_keys_to_press]
    return min(possible_movements, key=len)


@lru_cache()
def d3_calc(move: tuple[str, str]):
    possible_keys_to_press = get_possible_keys(move, PadType.DirectionPad)
    return possible_keys_to_press[0]


if __name__ == '__main__':
    import time

    result = main(get_specific_file_as_lines('sample_input.txt'))
    print(f"Sample result:\n{result}")
    assert result == 126384

    start_time = time.time()
    result = main(get_file_as_lines())
    print(f"Part one result:\n{result}")
    assert result == 105458
    print(f"Time taken: {time.time() - start_time:.2f} seconds")
