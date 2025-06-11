from day7.part1_2 import part_one, part_two
from file_handling import input_as_lines

part_one_target = 850435817339
part_two_target = 104824810233437

if __name__ == '__main__':
    part_one_result = part_one(input_as_lines())
    print(f"Part one result:\n{part_one_result}")
    assert part_one_result == part_one_target, f'Expected {part_one_target}, but got {part_one_result}'
    part_two_result = part_two(input_as_lines())
    print(f"Part two result:\n{part_two_result}")
    assert part_two_result == part_two_target, f'Expected {part_two_target}, but got {part_two_result}'
