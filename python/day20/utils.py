from map_handling import get_map_dimensions, Coordinate, Direction


def parse(lines: list[str]):
    height, width = get_map_dimensions(lines)
    end_coord, start_coord = get_start_end(lines)
    return start_coord, end_coord, width, height


def get_start_end(lines):
    start_coord, end_coord = None, None
    for y_coord, row in enumerate(lines):
        for x_coord, col in enumerate(row):
            if lines[y_coord][x_coord] == 'S':
                start_coord = Coordinate(x_coord, y_coord)
            elif lines[y_coord][x_coord] == 'E':
                end_coord = Coordinate(x_coord, y_coord)
    return end_coord, start_coord


def rec_solve(start_coord, end_coord, map_data):
    def recursion(coordinate, backwards_dir: Direction):
        if coordinate == end_coord:
            return [end_coord]
        next_dir = dir_not_back(map_data, coordinate, backwards_dir)
        new_dir = next(next_dir)
        return recursion(coordinate + new_dir.value, new_dir.opposite_direction()) + [coordinate]  # TODO

    _direction = dir_to_next(map_data, start_coord)
    _next_dir = next(_direction)
    return recursion(start_coord, _next_dir.opposite_direction())


def dir_not_back(map_data, coord, backwards_dir):
    """

    :param map_data:
    :param coord:
    :param backwards_dir:
    :return:

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

    >>> list(dir_not_back(map_data, Coordinate(1,1), Direction.DOWN))
    [<Direction.RIGHT: Coordinate(1, 0)>]
    >>> list(dir_not_back(map_data, Coordinate(3,1), Direction.LEFT))
    [<Direction.DOWN: Coordinate(0, 1)>]
    """
    movable_directions = dir_to_next(map_data, coord)
    return filter(lambda _dir: backwards_dir != _dir, movable_directions)


def dir_to_next(map_data, coord):
    """

    :param map_data:
    :param coord:
    :return:

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

    >>> list(dir_to_next(map_data, Coordinate(1,1)))
    [<Direction.RIGHT: Coordinate(1, 0)>, <Direction.DOWN: Coordinate(0, 1)>]
    >>> list(dir_to_next(map_data, Coordinate(3,1)))
    [<Direction.DOWN: Coordinate(0, 1)>, <Direction.LEFT: Coordinate(-1, 0)>]
    """
    return filter(lambda _dir: coordinate_type(map_data, coord + _dir.value) != '#',
                  (direction for direction in Direction))


def coordinate_type(map_data, coordinate):
    return map_data[coordinate.y][coordinate.x]
