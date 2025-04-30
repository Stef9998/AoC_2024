import re

from file_handling import get_file_as_lines

def get_input_as_lists():
    data = get_file_as_lines()
    list1, list2 = get_numbers_as_lists(data)
    return list1, list2


def get_numbers_as_lists(data: list[str]) -> map:
    return parse_column_values_of_lines_to_lists(data, parse_numbers)


def parse_column_values_of_lines_to_lists(data: list, parser: callable):
    return map(list, zip(*map(parser, data)))


def parse_numbers(line: str) -> tuple[int, int] | None:
    match = re.search(r'(\d+)\s+(\d+)', line)
    if match:
        return tuple(map(int, match.groups()))
    return None