from python.day13.utils import split_into_blocks, get_input_as_blocks
from python.file_handling import get_file_as_lines

button_a_cost = 3
button_b_cost = 1

def main(lines: list[str]) -> int:

    data_blocks = get_input_as_blocks(lines)

    for block in data_blocks:
        button_a = block[0]
        button_b = block[1]
        prize = block[2]
    return 0


if __name__ == '__main__':
    result = main(get_file_as_lines())
    # result = main(get_specific_file_as_lines('sample_input.txt'))
    print(f"Part one result:\n{result}")
    # assert result ==
