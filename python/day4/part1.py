from collections import deque

from enum import Enum

class Direction(Enum):
    VERTICAL = 0
    DOWN_RIGHT = 1
    DOWN_LEFT = -1


def part_one(lines: list[str]) -> int:

    line_length = len(lines[0])
    match_words = match_word_all_directions(line_length)

    match_word_in_line = lambda line: sum(match_words(char, char_position) for char_position, char in enumerate(line))
    return sum(match_word_in_line(line) for line in lines)


def match_word_all_directions(line_length):
    horizontal_matcher = next_char_horizontal_clojure()
    vertical_matcher = next_char_vertical_clojure(line_length, Direction.VERTICAL)
    down_right_matcher = next_char_vertical_clojure(line_length, Direction.DOWN_RIGHT)
    down_left_matcher = next_char_vertical_clojure(line_length, Direction.DOWN_LEFT)

    def process_char(char: str, char_position: int) -> int:
        return (horizontal_matcher(char, char_position)
                + vertical_matcher(char, char_position)
                + down_right_matcher(char, char_position)
                + down_left_matcher(char, char_position))

    return process_char


def next_char_vertical_clojure(line_length: int, direction: Direction):
    is_reversed: deque[bool] = deque([False] * line_length)
    last_char_index: deque[int] = deque([0] * line_length)

    def process_char(char: str, char_position: int) -> int:
        nonlocal is_reversed, last_char_index
        matched = 0
        if char_position == 0:
            is_reversed.rotate(direction.value)
            last_char_index.rotate(direction.value)
            if direction.value == 1:
                last_char_index[0] = 0
            elif direction.value == -1:
                last_char_index[-1] = 0

        if found_next_char(char, is_reversed[char_position], last_char_index[char_position]):
            if is_reversed[char_position]:
                last_char_index[char_position] -= 1
                if last_char_index[char_position] == 1:
                    matched += 1
            else:
                last_char_index[char_position] += 1
                if last_char_index[char_position] == 4:
                    matched += 1
        else:
            last_char_index[char_position] = 0

        if char == 'X':
            is_reversed[char_position] = False
            last_char_index[char_position] = 1
        elif char == 'S':
            is_reversed[char_position] = True
            last_char_index[char_position] = 4

        return matched

    return process_char


def next_char_horizontal_clojure():
    is_reversed = False
    last_char_index = 0

    def process_char(char: str, char_position: int) -> int:
        nonlocal is_reversed, last_char_index
        matched = 0

        if char_position == 0:
            last_char_index = 0

        if found_next_char(char, is_reversed, last_char_index):
            if is_reversed:
                last_char_index -= 1
                if last_char_index == 1:
                    matched += 1
            else:
                last_char_index += 1
                if last_char_index == 4:
                    matched += 1
        else:
            last_char_index = 0

        if char == 'X':
            is_reversed = False
            last_char_index = 1
        elif char == 'S':
            is_reversed = True
            last_char_index = 4

        return matched

    return process_char


def found_next_char(char: str, is_reversed: bool, before_char_index: int) -> bool:
    if char == 'X':
        if is_reversed and before_char_index == 2:
            return True
    elif char == 'M':
        if not is_reversed and before_char_index == 1:
            return True
        elif is_reversed and before_char_index == 3:
            return True
    elif char == 'A':
        if not is_reversed and before_char_index == 2:
            return True
        elif is_reversed and before_char_index == 4:
            return True
    elif char == 'S':
        if not is_reversed and before_char_index == 3:
            return True
    else:
        raise ValueError(f"Unexpected character: {char}")
    return False
