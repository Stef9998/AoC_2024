from typing import Iterable

from file_handling import get_file_as_lines
from list_handling import get_lines_of_numbers


def part_two(lists: Iterable[list[int]]) -> int:
    return sum(map(lambda numbers: is_report_safe(numbers, compare_adjacent_numbers_allow_one_wrong), lists))


def is_report_safe(numbers: list[int], compare_adjacent_numbers_func) -> bool:
    return (compare_adjacent_numbers_func(numbers, lambda x, y: y - 3 <= x <= y - 1) or
            compare_adjacent_numbers_func(numbers, lambda x, y: y + 1 <= x <= y + 3))


def compare_adjacent_numbers_allow_one_wrong(numbers, comparator):
    numbers_excluding_index = lambda i: numbers[:i] + numbers[i + 1:]
    return any(
        all(map(comparator, numbers_excluding_index(i), numbers_excluding_index(i)[1:]))
        for i in range(len(numbers))
    )


def part_one(lists: Iterable[list[int]]) -> int:
    return sum(map(lambda numbers: is_report_safe(numbers, compare_adjacent_numbers), lists))


def compare_adjacent_numbers(numbers, comparator):
    return all(map(comparator, numbers, numbers[1:]))


if __name__ == '__main__':
    part_one_result = part_one(get_lines_of_numbers(get_file_as_lines()))
    print(f"Part one result:\n{part_one_result}")
    assert part_one_result == 490
    part_two_result = part_two(get_lines_of_numbers(get_file_as_lines()))
    print(f"Part two result:\n{part_two_result}")
    assert part_two_result == 536
