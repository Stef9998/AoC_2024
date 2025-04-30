from typing import Iterable

from python.day3.utils import get_lines_as_list_p1
from python.day3.utils import get_lines_as_list_p2


def part_one(lines: Iterable[list[(int, int)]]):
    return sum(sum(pair[0] * pair[1] for pair in line) for line in lines)


def part_two(lists):
    # Placeholder for part two logic
    pass


if __name__ == '__main__':
    part_one_result = part_one(get_lines_as_list_p1())
    print(f"Part one result:\n{part_one_result}")
    part_two_result = part_one(get_lines_as_list_p2())
    print(f"Part two result:\n{part_two_result}")
