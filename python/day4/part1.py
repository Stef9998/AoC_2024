from collections import deque

from numpy.f2py.auxfuncs import throw_error

from python.file_handling import get_file_as_lines

from enum import Enum

class Direction(Enum):
    VERTICAL = 0
    DOWN_RIGHT = 1
    DOWN_LEFT = -1


def part_one(lines: list[str]) -> int:
    # Thinking about being able to only use one pass through the data (left->right, then down)
    #  every time we find a X we can test for direction: right, right-down, down, down-left
    #  every time we find a S we can test the same direction, but the word is now reversed

    match = 0

    line_length = len(lines[0])

    match_word_vertical = next_char_vertical_clojure(line_length, Direction.VERTICAL)
    match_word_down_right = next_char_vertical_clojure(line_length, Direction.DOWN_RIGHT)
    match_word_down_left = next_char_vertical_clojure(line_length, Direction.DOWN_LEFT)
    for line in lines:
        char_position = 0
        match_word_horizontal = next_char_horizontal_clojure()
        for char in line:
            match += match_word_horizontal(char)
            match += match_word_vertical(char, char_position)
            match += match_word_down_right(char, char_position)
            match += match_word_down_left(char, char_position)

            char_position += 1
        #     print(char, end="")
        # print()
    # print()
    return match


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

    def process_char(char: str) -> int:
        nonlocal is_reversed, last_char_index
        matched = 0

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
        throw_error(f"Unknown character {char}")
    return False


if __name__ == '__main__':
    part_one_result = part_one(get_file_as_lines())
    print(f"Part one result:\n{part_one_result}")