from typing import Iterable

from python.file_handling import get_file_as_lines


def get_lines_as_list_p1() -> Iterable[list[(int, int)]]:
    return get_lines_as_list(default_parser)


def get_lines_as_list(parser: callable) -> Iterable[list[(int, int)]]:
    lines = get_file_as_lines()
    return map(parser, lines)


def default_parser(line: str):
    import re
    return map(lambda tup: (map(int, tup[1]),map(int,tup[2])), map(re.findall(r'mul\((\d+),(\d+)\)', line)))

