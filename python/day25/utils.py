from collections.abc import Generator
from enum import Enum

from file_handling import get_specific_file_as_lines

key_lock_data_line_length = 7
num_of_pins = 5


class KeyLock(Enum):
    Lock = 0
    Key = 1


def parse(lines: list[str]):
    key_lock_generator = key_lock_getter(lines)
    key_locks = map(get_key_lock_data, key_lock_generator)
    locks = set()
    keys = set()
    for key_lock in key_locks:
        if key_lock[0] == KeyLock.Lock:
            locks.add(key_lock[1])
        elif key_lock[0] == KeyLock.Key:
            keys.add(key_lock[1])
        else:
            raise RuntimeError
    return locks, keys

def get_key_lock_data(key_lock_data: list[str]):
    assert len(key_lock_data) == key_lock_data_line_length
    return lock_or_key(key_lock_data), pin_heights(key_lock_data)


def pin_heights(key_lock_data: list[str]) -> tuple:
    """

    :param key_lock_data:
    :return:

    >>> pin_heights([
    ... "#####",
    ... ".####",
    ... ".####",
    ... ".####",
    ... ".#.#.",
    ... ".#...",
    ... "....."
    ... ])
    (0, 5, 3, 4, 3)
    >>> pin_heights([
    ... ".....",
    ... "#....",
    ... "#....",
    ... "#...#",
    ... "#.#.#",
    ... "#.###",
    ... "#####"
    ... ])
    (5, 0, 2, 1, 3)
    """

    assert len(key_lock_data) == key_lock_data_line_length
    return tuple(
        sum(1 for line in key_lock_data[1:-1] if line[pin_index] == '#')
        for pin_index in range(num_of_pins)
    )

def lock_or_key(key_lock_data: list[str]):
    """

    :param key_lock_data:
    :return:
    >>> lock_or_key([
    ... "#####",
    ... ".####",
    ... ".####",
    ... ".####",
    ... ".#.#.",
    ... ".#...",
    ... "....."
    ... ])
    <KeyLock.Lock: 0>
    >>> lock_or_key([
    ... ".....",
    ... "#....",
    ... "#....",
    ... "#...#",
    ... "#.#.#",
    ... "#.###",
    ... "#####"
    ... ])
    <KeyLock.Key: 1>
    """
    assert len(key_lock_data) == key_lock_data_line_length
    if key_lock_data[0] == "#####" and key_lock_data[-1] == ".....":
        return KeyLock.Lock
    if key_lock_data[0] == "....." and key_lock_data[-1] == "#####":
        return KeyLock.Key
    raise ValueError

def key_lock_getter(lines: list[str]) -> Generator[list[str], None, None]:
    key_lock_lines = []
    for line in lines:
        if line.strip() == "":
            assert len(key_lock_lines) == key_lock_data_line_length
            yield key_lock_lines
            key_lock_lines = []
        else:
            key_lock_lines.append(line)
    yield key_lock_lines
    return


if __name__ == '__main__':
    parse(get_specific_file_as_lines('sample_input.txt'))