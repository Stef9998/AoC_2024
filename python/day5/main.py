from file_handling import  input_as_lines

from part1 import main as part_one
from part2 import main as part_two


if __name__ == '__main__':
    part_one_result = part_one(input_as_lines())
    print(f"Part one result:\n{part_one_result}")
    part_two_result = part_two(input_as_lines())
    print(f"Part two result:\n{part_two_result}")
