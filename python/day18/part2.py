import map_handling
from day18.utils import parse_lines, get_step, printout_step
from file_handling import input_as_lines, input_as_lines
from map_handling import Coordinate


def main(lines: list[str], bit_count: int = 1024, width=71, height=71) -> int:

    return 0


if __name__ == '__main__':
    import time

    # result = main(get_input_file_as_lines('sample.txt'), 12, 7, 7)
    # print(f"Sample one result:\n{result}")
    # assert result == 22

    start_time = time.time()
    result = main(input_as_lines(), 1024, 71, 71)
    print(f"Part two result:\n{result}")
    # assert result ==
    print(f"Time taken: {time.time() - start_time:.2f} seconds")
