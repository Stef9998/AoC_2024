import sys

from day20.utils import rec_solve, coordinate_type
from file_handling import input_as_lines
import day20.utils as ut
from map_handling import Direction, Coordinate, out_of_bounds


def main(lines: list[str]) -> int:
    start_coord, end_coord, width, height = ut.parse(lines)

    sys.setrecursionlimit(100000)
    array = rec_solve(start_coord, end_coord, lines)
    sys.setrecursionlimit(1000)
    coord_path_to_end = {coordinate: i for i, coordinate in enumerate(array)}

    return calc(lines, width, height, array, coord_path_to_end)


def calc(map_data, width, height, array, coord_path_to_end):
    calcer = calc_sub1(map_data, width, height, coord_path_to_end)
    cheats = 0
    for coordinate in array:
        cheat_direction = adjacent_walls(map_data, coordinate)
        cheats += sum(calcer(coordinate, direction) for direction in cheat_direction)
    return cheats


def calc_sub1(map_data, width, height, coord_path_to_end):
    def calcer(coordinate, direction):
        coordinate_to_skip_to = coordinate + 2 * direction.value
        if out_of_bounds(coordinate_to_skip_to, width, height):
            return 0
        if coordinate_type(map_data, coordinate_to_skip_to) == '#':
            return 0
        cheat_saved_time = coord_path_to_end[coordinate] - coord_path_to_end[coordinate_to_skip_to] - 2
        if cheat_saved_time >= 100:
            return 1
        return 0

    return calcer


def adjacent_walls(map_data, coord):
    """
    >>> map_data = ['###############',
    ... '#...#...#.....#',
    ... '#.#.#.#.#.###.#',
    ... '#S#...#.#.#...#',
    ... '#######.#.#.###',
    ... '#######.#.#...#',
    ... '#######.#.###.#',
    ... '###..E#...#...#',
    ... '###.#######.###',
    ... '#...###...#...#',
    ... '#.#####.#.###.#',
    ... '#.#...#.#.#...#',
    ... '#.#.#.#.#.#.###',
    ... '#...#...#...###',
    ... '###############'
    ... ]

    >>> list(adjacent_walls(map_data, Coordinate(1,1)))
    [<Direction.UP: Coordinate(0, -1)>, <Direction.LEFT: Coordinate(-1, 0)>]
    >>> list(adjacent_walls(map_data, Coordinate(3,1)))
    [<Direction.UP: Coordinate(0, -1)>, <Direction.RIGHT: Coordinate(1, 0)>]
    """
    return filter(lambda _dir: coordinate_type(map_data, coord + _dir.value) == '#',
                  (direction for direction in Direction))


if __name__ == '__main__':
    import time

    result = main(input_as_lines('sample.txt'))
    print(f"Sample one result:\n{result}")
    assert result == 0

    start_time = time.time()
    result = main(input_as_lines())
    print(f"Part one result:\n{result}")
    assert result == 1409
    print(f"Time taken: {time.time() - start_time:.3f} seconds")
