from typing import Iterable

from utils import get_lines_as_list


def part_one_it(lists: Iterable[list[int]]) -> int:
    num_of_safe_reports = 0
    for numbers in lists:
        if is_report_safe(numbers):
            num_of_safe_reports += 1
    return num_of_safe_reports


def part_one(lists: Iterable[list[int]]) -> int:
    return sum(map(is_report_safe, lists))


def is_report_safe(numbers: list[int]) -> bool:
    return ( compare_adjacent_numbers(numbers, lambda x, y: y-3 <= x <= y-1) or
             compare_adjacent_numbers(numbers, lambda x, y: y+1 <= x <= y+3))


def compare_adjacent_numbers(numbers, comparator):
    return all(map(comparator, numbers, numbers[1:]))


if __name__ == '__main__':
    lines_as_list = get_lines_as_list()
    # print(list(lines_as_list))
    part_one_result = part_one(lines_as_list)
    print(f"Part one result:\n{part_one_result}")
