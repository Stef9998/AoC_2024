from collections import deque
from typing import Iterable

from file_handling import input_as_lines
from list_handling import number_parser


def lines_to_data(lines: list[str]) -> Iterable[tuple[int, Iterable[int]]]:
    number_getter = lambda result, operants: (int(result), number_parser(operants))
    splitted_lines = split_lines(lines)
    return map(lambda line: number_getter(*line), splitted_lines)


def split_lines(lines: list[str]) -> Iterable[tuple[str, str]]:
    return map(lambda line: tuple(str.split(line, ': ', maxsplit=1)), lines)


if __name__ == "__main__":
    lines_ = input_as_lines()
    data = lines_to_data(lines_)
    print(list(data))
