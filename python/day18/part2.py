import map_handling
from day18.part1 import calc
from day18.utils import parse_lines, get_step, printout_step
from file_handling import input_as_lines, input_as_lines
from map_handling import Coordinate


def main(lines: list[str], bit_count: int = 1024, width=71, height=71) -> str:

    start_coordinate = Coordinate(0, 0)
    end_coordinate = Coordinate(width - 1, height - 1)
    map_handling.initialise_map(width, height)

    for i in range(bit_count, len(lines)):
        print(f"Number: {i}")
        blocked_coordinates = parse_lines(lines, i)
        result = calc(get_step(blocked_coordinates), start_coordinate, end_coordinate)
        if result is None:
            return lines[i-1]
    raise ValueError("Maze is with all blocked coordinates still solvable.")

if __name__ == '__main__':
    import time

    # result = main(input_as_lines('sample.txt'), 12, 7, 7)
    # print(f"Sample one result:\n{result}")
    # assert result == "6,1"

    start_time = time.time()
    result = main(input_as_lines(), 1024, 71, 71)
    print(f"Part two result:\n{result}")
    # assert result ==
    print(f"Time taken: {time.time() - start_time:.2f} seconds")
