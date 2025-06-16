import sys
from functools import lru_cache

import day20.utils as ut
from day20.utils import rec_solve
from file_handling import input_as_lines
from map_handling import Coordinate, out_of_bounds


def main(lines: list[str]) -> int:
    start_coord, end_coord, width, height = ut.parse(lines)

    sys.setrecursionlimit(100000)
    array = rec_solve(start_coord, end_coord, lines)
    sys.setrecursionlimit(1000)
    coord_path_to_end = {coordinate: i for i, coordinate in enumerate(array)}

    return calc(lines, width, height, array, coord_path_to_end)


def calc(map_data, width, height, array, coord_path_to_end):
    calcer = calc_sub1(map_data, width, height, coord_path_to_end)
    not_wall = lambda coordinate: ut.coordinate_type(map_data, coordinate) != '#'
    in_bounds = lambda coordinate: not out_of_bounds(coordinate, width, height)
    cheat_delta = cheat_coordinate_delta()
    cheats = 0
    for coordinate in array:
        new_coordinates = (delta + coordinate for delta in cheat_delta)
        movable_coordinates = filter(not_wall, filter(in_bounds, new_coordinates))
        cheats += sum(calcer(coordinate, coordinate_to_skip_to) for coordinate_to_skip_to in movable_coordinates)
    return cheats


def calc_sub1(coord_path_to_end):
    def calcer(coordinate, coordinate_to_skip_to):
        new_coordinate = coordinate - coordinate_to_skip_to
        distance = abs(new_coordinate.x) + abs(new_coordinate.y)
        cheat_saved_time = coord_path_to_end[coordinate] - coord_path_to_end[coordinate_to_skip_to] - distance
        if cheat_saved_time >= 100:
            return 1
        return 0

    return calcer


@lru_cache(maxsize=1)
def cheat_coordinate_delta():
    return [Coordinate(x, y) for x in range(-20, 21) for y in range(-(20 - abs(x)), 21 - abs(x))]


if __name__ == '__main__':
    import time

    result = main(input_as_lines('sample.txt'))
    print(f"Sample one result:\n{result}")
    assert result == 0

    start_time = time.time()
    result = main(input_as_lines())
    print(f"Part two result:\n{result}")
    assert result == 1012821
    print(f"Time taken: {time.time() - start_time:.3f} seconds")
