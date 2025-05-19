from dataclasses import dataclass
from typing import Callable

import map_handling
from file_handling import get_file_as_lines
from map_handling import Coordinate, out_of_bounds_calculator


@dataclass(frozen=True)
class Plot:
    area: int
    perimeter: int
    coordinates: set[Coordinate]


height: int = 0
width: int = 0
is_out_of_bounds: Callable[[Coordinate], bool] = None


def initialize_globals(lines: list[str]) -> None:
    global is_out_of_bounds, height, width
    height, width = map_handling.get_map_dimensions(lines)
    is_out_of_bounds = out_of_bounds_calculator(width, height)


def main(lines: list[str]) -> int:
    initialize_globals(lines)

    non_visited = {Coordinate(x, y) for y in range(height) for x in range(width)}
    fence_cost = 0
    while non_visited:
        next_non_visited = non_visited.pop()
        visited = fill_one_plot(lines, next_non_visited)
        non_visited -= visited
        area = len(visited)
        perimeter = get_plot_perimeter(visited)
        fence_cost += area * perimeter

    return fence_cost


def fill_one_plot(maze: list[str], start_coordinate: Coordinate) -> set[Coordinate]:
    """
    Recursively fills the plot starting from the given coordinate.

    Args:
        maze (list[str]): The maze represented as a list of strings.
        start_coordinate (Coordinate): The starting coordinate for filling the plot.

    Returns:
        Plot: The filled plot.

    Examples:
        >>> maze = [
        ...     "AAAA",
        ...     "ABCD",
        ...     "ABCC",
        ...     "EEEC"
        ... ]
        >>> initialize_globals(maze)
        >>> result1 = {Coordinate(0, 0), Coordinate(1, 0), Coordinate(2, 0), Coordinate(3, 0), Coordinate(0, 1), Coordinate(0, 2)}
        >>> fill_one_plot(maze, Coordinate(0, 0)) == result1
        True
        >>> result2 = {Coordinate(1, 1), Coordinate(1, 2)}
        >>> fill_one_plot(maze, Coordinate(1, 1)) == result2
        True
        >>> fill_one_plot(maze, Coordinate(3, 1))
        {Coordinate(3, 1)}
    """
    return fill_plot_recursive(maze, start_coordinate, {start_coordinate})


def get_plot_perimeter(visited):
    """
    Calculate the perimeter of the plot based on the visited coordinates.

    Args:
        visited (set[Coordinate]): A set of visited coordinates.

    Returns:
        int: The perimeter of the plot.

    Examples:
        >>> visited = {Coordinate(0, 0), Coordinate(1, 0), Coordinate(2, 0), Coordinate(3, 0), Coordinate(0, 1), Coordinate(0, 2)}
        >>> get_plot_perimeter(visited)
        14
    """
    return sum(get_perimeter_for_coordinate(coordinate, visited) for coordinate in visited)


def get_perimeter_for_coordinate(coordinate, visited):
    """
    Calculate the perimeter contribution of a single coordinate.

    Args:
        coordinate (Coordinate): The coordinate to calculate the perimeter for.
        visited (set[Coordinate]): A set of visited coordinates.

    Returns:
        int: The perimeter contribution of the coordinate.

    Examples:
        >>> visited = {Coordinate(0, 0), Coordinate(1, 0), Coordinate(2, 0), Coordinate(3, 0), Coordinate(0, 1), Coordinate(0, 2)}
        >>> get_perimeter_for_coordinate(Coordinate(0, 0), visited)
        2
        >>> get_perimeter_for_coordinate(Coordinate(1, 0), visited)
        2
        >>> get_perimeter_for_coordinate(Coordinate(3, 0), visited)
        3
        >>> get_perimeter_for_coordinate(Coordinate(0, 2), visited)
        3
    """
    no_of_surrounding_coordinates = sum(1 for surrounding_coordinate in surrounding_coordinates(coordinate) if surrounding_coordinate in visited)
    return 4 - no_of_surrounding_coordinates


def fill_plot_recursive(maze: list[str], coordinate: Coordinate, visited: set[Coordinate]) -> set[Coordinate]:
    same_plot_coordinates = list(filter(
        lambda coord: coord not in visited and coordinates_same_plot(maze, coordinate, coord),
        surrounding_coordinates(coordinate)
    ))
    if not same_plot_coordinates:
        return visited
    visited.update(same_plot_coordinates)
    for new_coordinate in same_plot_coordinates:
        visited.update(fill_plot_recursive(maze, new_coordinate, visited))
    return visited


def surrounding_coordinates(coordinate):
    return (coordinate + direction.value for direction in map_handling.Direction)


def coordinates_same_plot(maze: list[str], plot_coordinate: Coordinate, new_coordinate: Coordinate) -> bool:
    """
    Check if two coordinates are part of the same plot.

    Args:
        maze (list[str]): The maze represented as a list of strings.
        plot_coordinate (Coordinate): The coordinate of the plot.
        new_coordinate (Coordinate): The new coordinate to check.

    Returns:
        bool: True if both coordinates are part of the same plot, False otherwise.

    Examples:
        >>> maze = [
        ...     "AAAA",
        ...     "ABCD",
        ...     "ABCC",
        ...     "EEEC"
        ... ]
        >>> initialize_globals(maze)
        >>> coordinates_same_plot(maze, Coordinate(0, 0), Coordinate(1, 0))
        True
        >>> coordinates_same_plot(maze, Coordinate(0, 0), Coordinate(1, 1))
        False
        >>> coordinates_same_plot(maze, Coordinate(0, 0), Coordinate(-1, -1))
        False
    """
    if is_out_of_bounds(new_coordinate):
        return False
    if get_plot_type(maze, plot_coordinate) != get_plot_type(maze, new_coordinate):
        return False
    return True


def get_plot_type(maze: list[str], coordinate: Coordinate) -> str:
    return maze[coordinate.y][coordinate.x]


if __name__ == '__main__':
    result = main(get_file_as_lines())
    # result = main(get_specific_file_as_lines('sample_input.txt'))
    print(f"Part one result:\n{result}")
    # assert result ==
