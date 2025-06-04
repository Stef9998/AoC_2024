import map_handling
from day18.utils import parse_lines, get_step, printout_step
from file_handling import get_file_as_lines
from map_handling import Coordinate


def main(lines: list[str], bit_count: int, width, height) -> int:

    blocked_coordinates = parse_lines(lines, bit_count)
    start_coordinate = Coordinate(0,0)
    end_coordinate = Coordinate(width - 1, height - 1)
    map_handling.initialise_map(width, height)

    step = get_step(blocked_coordinates)
    previous_coordinates = {start_coordinate}
    current_coordinates = {start_coordinate}
    steps = 0
    while True:
        new_coordinates = step(current_coordinates, previous_coordinates)
        previous_coordinates = current_coordinates
        current_coordinates = new_coordinates
        steps += 1
        printout_step(blocked_coordinates, current_coordinates, previous_coordinates, steps)
        if end_coordinate in current_coordinates:
            break
    return steps


if __name__ == '__main__':
    import time

    # result = main(get_specific_file_as_lines('sample_input.txt'), 12, 7, 7)
    # print(f"Sample one result:\n{result}")
    # assert result == 22

    start_time = time.time()
    result = main(get_file_as_lines(), 1024, 71, 71)
    print(f"Part one result:\n{result}")
    assert result == 270
    print(f"Time taken: {time.time() - start_time:.2f} seconds")
