from functools import lru_cache

from day21.keypad import PadType
from day21.utils import get_possible_keys, movements_from_keys, get_number_from_numpad
from file_handling import get_file_as_lines


def main(lines: list[str]) -> int:
    _inputs = lines
    return sum(calc_result_one_code(_keys_to_press) for _keys_to_press in _inputs)


def calc_result_one_code(numpad_keys: str) -> int:
    number = get_number_from_numpad(numpad_keys)
    movements = movements_from_keys(numpad_keys)
    keypress_length = sum(num_pad_calc(move) for move in movements)
    return number * keypress_length


def num_pad_calc(move: tuple[str, str]):
    possible_keys_to_press = get_possible_keys(move, PadType.NumPad)
    depth_to_go = 25
    return min(
        [sum(dir_pad_calc(move, depth_to_go) for move in movements_from_keys(possible_key_to_press))
         for possible_key_to_press in possible_keys_to_press]
    )


@lru_cache(maxsize=1024)
def dir_pad_calc(move: tuple[str, str], depth_to_go: int) -> int:
    possible_keys_to_press = get_possible_keys(move, PadType.DirectionPad)
    if depth_to_go == 1:
        return len(possible_keys_to_press[0])
    return min(
        [sum(dir_pad_calc(move, depth_to_go - 1) for move in movements_from_keys(possible_key_to_press))
         for possible_key_to_press in possible_keys_to_press]
    )


if __name__ == '__main__':
    import time

    start_time = time.time()
    result = main(get_file_as_lines())
    print(f"Part one result:\n{result}")
    assert result == 129551515895690
    print(f"Time taken: {time.time() - start_time:.2f} seconds")
