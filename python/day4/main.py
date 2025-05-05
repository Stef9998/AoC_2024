from file_handling import get_file_as_lines

from part1 import part_one
from part2 import part_two


if __name__ == '__main__':
    part_one_result = part_one(get_file_as_lines())
    assert part_one_result == 2654, f"Expected 2654, but got {part_one_result}"
    print(f"Part one result:\n{part_one_result}")
    part_two_result = part_two(get_file_as_lines())
    print(f"Part two result:\n{part_two_result}")
