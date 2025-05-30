from file_handling import get_file_as_lines, get_specific_file_as_lines

from part1 import main as part_one
from part2 import main as part_two


if __name__ == '__main__':
    part_one_result = part_one(get_file_as_lines())
    print(f"Part one result:\n{part_one_result}")
    assert part_one_result == 1381056
    part_two_result = part_two(get_file_as_lines())
    # part_two_result = part_two(get_specific_file_as_lines("sample_input.txt"))
    print(f"Part two result:\n{part_two_result}")
    assert part_two_result == 834828
