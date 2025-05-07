from collections import deque
from typing import Iterable

from file_handling import get_file_as_lines


def lines_to_data(lines: list[str]) -> Iterable[tuple[int, Iterable[int]]]:
    number_getter = lambda result, operants: (int(result), split_numbers(operants))
    splitted_lines = split_lines(lines)
    return map(lambda line: number_getter(*line), splitted_lines)


def split_lines(lines: list[str]) -> Iterable[tuple[str, str]]:
    return map(lambda line: tuple(str.split(line, ': ', maxsplit=1)), lines)


def split_numbers(line: str) -> Iterable[int]:
    return map(int, line.split(' '))


if __name__ == "__main__":
    lines_ = get_file_as_lines()
    data = lines_to_data(lines_)
    print(list(data))