from day8.part1_2 import part_one, part_two
from file_handling import input_as_lines

if __name__ == '__main__':
    part_one_result = part_one(input_as_lines())
    print(f"Part one result:\n{part_one_result}")
    assert part_one_result == 318
    part_two_result = part_two(input_as_lines())
    print(f"Part one result:\n{part_two_result}")
    assert part_two_result == 1126
