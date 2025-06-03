from file_handling import get_file_as_lines, get_specific_file_as_lines


def main(lines: list[str]) -> int:

    # TODO recursion:
    #  - testing for next char and then get the list of possible options from dict
    #    - if options are empty return with false (should only be in example data)
    #    - for every option run the recursion
    #  - recursion-step (towel_option, index of current test in pattern, pattern (through closure), ):
    #    - test if rest of length of pattern is > length from towel_option, return if
    #    - test if towel option is equal to the next slice of pattern, recurse, else return false

    return 0 #TODO


if __name__ == '__main__':
    result = main(get_file_as_lines())
    # result = main(get_specific_file_as_lines('sample_input.txt'))
    print(f"Part one result:\n{result}")
    # assert result ==