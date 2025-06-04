import map_handling
from map_handling import Coordinate, get_surrounding_coordinates


def parse_lines(lines: list[str], count: int) -> set[Coordinate]:
    return set(map(line_parser, lines[:count]))


def line_parser(line: str) -> Coordinate:
    x, y = line.split(",")
    return Coordinate(int(x), int(y))


def get_step(blocked_coordinates: set[Coordinate]):
    def step(coordinates: set[Coordinate], last_coordinates: set[Coordinate]):
        from itertools import chain
        neighboring_coordinates = chain.from_iterable(map(get_surrounding_coordinates, coordinates))
        in_bounds = lambda _coordinate: not map_handling.get_map_instance().out_of_bounds_calculator(_coordinate)
        coordinates_not_out_of_bounds = filter(in_bounds, neighboring_coordinates)
        maybe_next_coordinates = set(coordinates_not_out_of_bounds).difference(blocked_coordinates)
        return maybe_next_coordinates.difference(coordinates | last_coordinates)

    return step


def printout_step(blocked_coordinates, current_coordinates, previous_coordinates, steps):
    print(f"Step: {steps}")
    print_map(previous_coordinates, current_coordinates, blocked_coordinates)
    print()


def print_map(previous_coordinates: set[Coordinate], current_coordinates: set[Coordinate], blocked_coordinates: set[Coordinate]):
    def get_char(coord):
        if coord in blocked_coordinates:
            return '#'
        if coord in current_coordinates:
            return 'O'
        if coord in previous_coordinates:
            return 'o'
        return '.'

    for y in range(map_handling.get_map_instance().height):
        row = (get_char(Coordinate(x, y)) for x in range(map_handling.get_map_instance().width))
        print(''.join(row))
