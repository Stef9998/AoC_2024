import sys
from enum import Enum
from functools import lru_cache


class Direction(Enum):
    UP = '^'
    RIGHT = '>'
    DOWN = 'v'
    LEFT = '<'

    def __str__(self) -> str:
        return f"{self.name.capitalize()} {self.value}"

    def __repr__(self) -> str:
        return f"{self.name.capitalize()} {self.value}"


"""
num pad:
+---+---+---+
| 7 | 8 | 9 |
+---+---+---+
| 4 | 5 | 6 |
+---+---+---+
| 1 | 2 | 3 |
+---+---+---+
    | 0 | A |
    +---+---+
"""

numpad_next_step = {
    '1': {'2': Direction.RIGHT, '4': Direction.UP},
    '2': {'1': Direction.LEFT, '3': Direction.RIGHT, '5': Direction.UP, '0': Direction.DOWN},
    '3': {'2': Direction.LEFT, '6': Direction.UP, 'A': Direction.DOWN},
    '4': {'1': Direction.DOWN, '5': Direction.RIGHT, '7': Direction.UP},
    '5': {'2': Direction.DOWN, '4': Direction.LEFT, '6': Direction.RIGHT, '8': Direction.UP},
    '6': {'3': Direction.DOWN, '5': Direction.LEFT, '9': Direction.UP},
    '7': {'4': Direction.DOWN, '8': Direction.RIGHT},
    '8': {'5': Direction.DOWN, '7': Direction.LEFT, '9': Direction.RIGHT},
    '9': {'6': Direction.DOWN, '8': Direction.LEFT},
    '0': {'2': Direction.UP, 'A': Direction.RIGHT},
    'A': {'0': Direction.LEFT, '3': Direction.UP},
}

"""
direction pad:
    +---+---+
    | ^ | A |
+---+---+---+
| < | v | > |
+---+---+---+
"""
direction_next_step = {
    '^': {'v': Direction.DOWN, 'A': Direction.RIGHT},
    'v': {'^': Direction.UP, '<': Direction.LEFT, '>': Direction.RIGHT},
    '<': {'v': Direction.RIGHT},
    '>': {'v': Direction.LEFT, 'A': Direction.UP},
    'A': {'^': Direction.LEFT, '>': Direction.DOWN},
}

direction_char_converter = {
    'v': Direction.DOWN,
    '^': Direction.UP,
    '<': Direction.LEFT,
    '>': Direction.RIGHT
}

def convert_dir(char: str):
    return direction_char_converter[char]

class PadType(Enum):
    NumPad = numpad_next_step
    DirectionPad = direction_next_step

NEXT_STEP = {
    "num_pad": numpad_next_step,
    "direction_pad": direction_next_step
}

@lru_cache()
def get_all_shortest_path(pad_type: PadType, start, end):
    def rec(current, visited: set):
        if current == end:
            return [[]]
        new_visited = visited | {current}
        options = pad_type.value[current]
        next_keys = set(options.keys()).difference(visited)
        if len(next_keys) == 0:
            return []

        shortest_paths = []
        shortest_length = sys.maxsize
        for key in next_keys:
            direction = options[key]
            res = rec(key, new_visited)
            for path in res:
                if len(path) + 1 < shortest_length:
                    shortest_length = len(path) + 1
                    shortest_paths = [[direction] + path]
                elif len(path) + 1 == shortest_length:
                    shortest_paths.append([direction] + path)
        return shortest_paths

    result = rec(start, set())
    assert result
    return result


import time

if __name__ == "__main__":
    keys = numpad_next_step.keys()
    iterations = 10_000

    t0 = time.time()
    for _ in range(iterations):
        for start_key in keys:
            for end_key in keys:
                foo = get_shortest_path(PadType.NumPad, start_key, end_key)
                foo2 = get_all_shortest_path(PadType.NumPad, start_key, end_key)
                print("breakpoint")
    t1 = time.time()
    print(f"Benchmark: {iterations} runs in {t1 - t0:.2f} seconds")