from typing import Iterable

from python.file_handling import get_file_as_lines


def get_lines_as_list() -> Iterable[list[int]]:
    lines = get_file_as_lines()
    return map(lambda line: parse_numbers(line, default_parser), lines)


def default_parser(line: str) -> list[str]:
    return line.split()


def parse_numbers(line: str, parser: callable) -> list[int]:
    digit = filter(lambda x: x.isdigit(), parser(line))
    return list(map(lambda number: int(number), digit))
