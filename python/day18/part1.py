import map_handling
from day18.utils import parse_lines, get_step, printout_step
from file_handling import input_as_lines
from map_handling import Coordinate


def main(lines: list[str], bit_count: int = 1024, width=71, height=71) -> int:

    blocked_coordinates = parse_lines(lines, bit_count)
    start_coordinate = Coordinate(0,0)
    end_coordinate = Coordinate(width - 1, height - 1)
    map_handling.initialise_map(width, height)

    result = calc(get_step(blocked_coordinates), start_coordinate, end_coordinate)
    if result:
        return result
    else:
        raise ValueError


def calc(step, start_coordinate, end_coordinate):
    previous_coordinates = {start_coordinate}
    current_coordinates = {start_coordinate}
    steps = 0
    while True:
        new_coordinates = step(current_coordinates, previous_coordinates)
        previous_coordinates = current_coordinates
        current_coordinates = new_coordinates
        steps += 1
        # printout_step(blocked_coordinates, current_coordinates, previous_coordinates, steps)
        if end_coordinate in current_coordinates:
            return steps
        if len(current_coordinates) == 0:
            return None


if __name__ == '__main__':
    import time

    # result = main(input_as_lines('sample.txt'), 12, 7, 7)
    # print(f"Sample one result:\n{result}")
    # assert result == 22

    start_time = time.time()
    result = main(input_as_lines(), 1024, 71, 71)
    print(f"Part one result:\n{result}")
    assert result == 270
    print(f"Time taken: {time.time() - start_time:.2f} seconds")
