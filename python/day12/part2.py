from day12.utils import fill_one_plot
from file_handling import input_as_lines
from map_handling import Coordinate, out_of_bounds_calculator, get_map_dimensions, Direction

global is_out_of_bounds


def init_globals(is_out_of_bounds_func):
    global is_out_of_bounds
    is_out_of_bounds = is_out_of_bounds_func


def main(lines: list[str]) -> int:
    height, width = get_map_dimensions(lines)
    is_out_of_bounds = out_of_bounds_calculator(width, height)

    init_globals(is_out_of_bounds)

    non_visited = {Coordinate(x, y) for y in range(height) for x in range(width)}
    fence_cost = 0
    while non_visited:
        next_non_visited = non_visited.pop()
        visited = fill_one_plot(lines, next_non_visited, is_out_of_bounds)
        # print(f"Plot with letter: {lines[next_non_visited.y][next_non_visited.x]}, Coordinates: \n"
        #       f"{visited}")
        non_visited -= visited
        area = len(visited)
        # TODO new calculation for part2
        sides = start_side_search(visited)
        # print(f"Sides: {sides}, Area: {area}")
        fence_cost += area * sides
        # print()

    return fence_cost


def start_side_search(plot_coordinates: set[Coordinate]):
    """
    Examples:
        >>> import map_handling
        >>> init_globals(map_handling.out_of_bounds_calculator(5, 5))
        >>> plot_coordinates = {Coordinate(0, 0)}
        >>> start_side_search(plot_coordinates)
        4
        >>> start_side_search(plot_coordinates)
        Traceback (most recent call last):
        ...
        ValueError: The first coordinate is not in the plot coordinates.
        >>> plot_coordinates = {Coordinate(0, 0), Coordinate(0, 1), Coordinate(0, 2)}
        >>> start_side_search(plot_coordinates)
        4
        >>> start_side_search(plot_coordinates)
        4
        >>> plot_coordinates = {Coordinate(0, 0), Coordinate(0, 1), Coordinate(1, 0), Coordinate(2, 0), Coordinate(2, 1)}
        >>> start_side_search(plot_coordinates)
        8
        >>> plot_coordinates = {Coordinate(0, 0), Coordinate(1, 0), Coordinate(2, 0), Coordinate(3, 0),
        ...     Coordinate(0, 1), Coordinate(1, 1), Coordinate(2, 1), Coordinate(3, 1),
        ...     Coordinate(0, 2), Coordinate(1, 2),
        ...     Coordinate(1, 3)}
        >>> start_side_search(plot_coordinates)
        8
        >>> plot_coordinates = {Coordinate(x, y) for x in range(4) for y in range(4)}
        >>> start_side_search(plot_coordinates)
        4
        >>> start_side_search(plot_coordinates)
        4
        >>> plot_coordinates.add(Coordinate(4, 0))
        >>> start_side_search(plot_coordinates)
        6
    """
    fences_coordinates = get_plot_fences(plot_coordinates)
    return connected_fences(fences_coordinates)


def connected_fences(fences_coordinates: dict[Direction, set[Coordinate]]):
    return sum(
        no_of_connected_fences(direction, dir_coordinates) for direction, dir_coordinates in fences_coordinates.items()
    )


def no_of_connected_fences(direction: Direction, fences_coordinates: set[Coordinate]):
    fences = 0
    while fences_coordinates:
        coordinate = fences_coordinates.pop()
        fences += 1
        connected_fences_left = get_connected_fences(coordinate, fences_coordinates, direction.turn_left())
        fences_coordinates -= connected_fences_left
        connected_fences_right = get_connected_fences(coordinate, fences_coordinates, direction.turn_right())
        fences_coordinates -= connected_fences_right
    return fences


def get_connected_fences(coordinate, fences_coordinates, direction: Direction):
    next_coordinate = coordinate + direction.value
    if next_coordinate not in fences_coordinates:
        return set()
    return get_connected_fences(next_coordinate, fences_coordinates, direction).union({next_coordinate})


def get_plot_fences(plot_coordinates) -> dict[Direction, set[Coordinate]]:
    fence_coordinates = {direction: set() for direction in Direction}
    for coordinate in plot_coordinates:
        fences = get_fences(plot_coordinates, coordinate)
        for direction in fences:
            fence_coordinates[direction].add(coordinate)
    return fence_coordinates


def get_fences(plot_coordinates: set[Coordinate], coordinate: Coordinate):
    """
    Examples:
        >>> import map_handling
        >>> init_globals(map_handling.out_of_bounds_calculator(5, 5))
        >>> plot_coordinates = {Coordinate(0, 0), Coordinate(0, 1), Coordinate(0, 2), Coordinate(1, 0)}
        >>> get_fences(plot_coordinates, Coordinate(0, 0))
        {Direction.UP, Direction.LEFT}
        >>> get_fences(plot_coordinates, Coordinate(0, 1))
        {Direction.RIGHT, Direction.LEFT}
        >>> get_fences(plot_coordinates, Coordinate(0, 2))
        {Direction.RIGHT, Direction.DOWN, Direction.LEFT}
        >>> get_fences(plot_coordinates, Coordinate(1, 0))
        {Direction.UP, Direction.RIGHT, Direction.DOWN}
    """
    # return {direction for direction in Direction if is_fence(coordinate, direction, plot_coordinates)}
    return filter(lambda direction: is_fence(coordinate, direction, plot_coordinates), Direction)


def is_fence(coordinate, direction, plot_coordinates):
    """
    Check if the coordinate is a fence in the given direction.

    Args:
        coordinate (Coordinate): The coordinate to check.
        direction (Direction): The direction to check.
        plot_coordinates (set[Coordinate]): A set of coordinates representing the plot.

    Returns:
        bool: True if the coordinate is a fence in the given direction, False otherwise.

    Examples:
        >>> import map_handling
        >>> init_globals(map_handling.out_of_bounds_calculator(5, 5))
        >>> plot_coordinates = {Coordinate(0, 0), Coordinate(1, 0), Coordinate(2, 0)}
        >>> is_fence(Coordinate(0, 0), Direction.RIGHT, plot_coordinates)
        False
        >>> is_fence(Coordinate(0, 0), Direction.LEFT, plot_coordinates)
        True
        >>> is_fence(Coordinate(2, 0), Direction.RIGHT, plot_coordinates)
        True
        >>> is_fence(Coordinate(0, 0), Direction.UP, plot_coordinates)
        True
    """
    global is_out_of_bounds
    behind_possible_fence = coordinate + direction.value
    return is_out_of_bounds(behind_possible_fence) or behind_possible_fence not in plot_coordinates


if __name__ == '__main__':
    result = main(input_as_lines())
    # result = main(input_as_lines('sample.txt'))
    print(f"Part two result:\n{result}")
    # assert result ==
