from typing import Iterable

from python.day3.utils import get_lines_as_list_p1, default_parser
from utils import get_lines_as_list


def part_one(lines: Iterable[list[(int, int)]]):
    result = 0
    for line in lines:
        result += sum(map(lambda pair: pair[0] * pair[1], line))
    return result


def part_two(lists):
    # Placeholder for part two logic
    pass


if __name__ == '__main__':
    part_one_result = part_one(get_lines_as_list(default_parser))
    print(f"Part one result:\n{part_one_result}")
    # part_two_result = part_two(get_lines_as_list())
    # print(f"Part two result:\n{part_two_result}")
