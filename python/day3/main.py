from typing import Iterable

from file_handling import get_file_as_lines

from day3.utils import get_lines_as_list_p1
from day3.utils import get_lines_as_list_p2


def sum_for_result(lines: Iterable[list[(int, int)]]):
    return sum(sum(pair[0] * pair[1] for pair in line) for line in lines)


def part_one(lines):
    data_lists = get_lines_as_list_p1(lines)
    return sum_for_result(data_lists)


def part_two(lines):
    data_lists = get_lines_as_list_p2(lines)
    return sum_for_result(data_lists)


if __name__ == '__main__':
    part_one_result = part_one(get_file_as_lines())
    print(f"Part one result:\n{part_one_result}")
    assert part_one_result == 179571322
    part_two_result = part_two(get_file_as_lines())
    print(f"Part two result:\n{part_two_result}")
    assert part_two_result == 103811193
