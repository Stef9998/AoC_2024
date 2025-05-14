from python.file_handling import get_file_as_lines

def main(lines: list[str]) -> int:

    # recursive idea:
    #  from 0 start, and then for each surrounding, that is excactly one more go inside and do recursive function
    #  if number is 9 return with 1
    #  if doesn't find next higher number return 0
    #  sum up all the ones that are called within and return



    return 0


if __name__ == '__main__':
    part_one_result = main(get_file_as_lines())
    # part_one_result = main(get_specific_file_as_lines('sample_input.txt'))
    print(f"Part one result:\n{part_one_result}")
    # assert part_one_result ==