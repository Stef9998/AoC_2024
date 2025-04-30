from typing import Iterable

from file_handling import get_file_as_lines


def get_lines_as_list_p1() -> Iterable[list[(int, int)]]:
    return map(default_parser, get_file_as_lines())


def get_lines_as_list_p2() -> Iterable[list[(int, int)]]:
    return map(default_parser, remove_disabled([''.join(get_file_as_lines())]))


def default_parser(line: str):
    import re
    reg = re.findall(r'mul\((\d+),(\d+)\)', line)
    return list(map(lambda tup: (int(tup[0]),int(tup[1])), reg))

import re

def remove_disabled(lines: list[str]) -> list[str]:
    return_lines = []
    for line in lines:
        return_lines.append(remove_between_keywords(line, "don't()", "do()"))
    return return_lines

def remove_between_keywords(text: str, start_keyword: str, end_keyword: str) -> str:
    pattern = re.escape(start_keyword) + r'.*?' + re.escape(end_keyword)
    return re.sub(pattern, '', text)
