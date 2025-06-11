from typing import Iterable

from list_handling import parse_lines

import re


def get_lines_as_list_p1(lines) -> Iterable[list[(int, int)]]:
    return parse_lines(lines, default_parser)


def get_lines_as_list_p2(lines) -> Iterable[list[(int, int)]]:
    return parse_lines(remove_disabled([''.join(lines)]), default_parser)


def default_parser(line: str):
    reg = re.findall(r'mul\((\d+),(\d+)\)', line)
    return list(map(lambda tup: (int(tup[0]), int(tup[1])), reg))


def remove_disabled(lines: list[str]) -> list[str]:
    return_lines = []
    for line in lines:
        return_lines.append(remove_between_keywords(line, "don't()", "do()"))
    return return_lines


def remove_between_keywords(text: str, start_keyword: str, end_keyword: str) -> str:
    pattern = re.escape(start_keyword) + r'.*?' + re.escape(end_keyword)
    return re.sub(pattern, '', text)
