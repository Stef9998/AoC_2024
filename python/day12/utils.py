from typing import Callable

from map_handling import Coordinate, out_of_bounds_calculator, Direction


def fill_plot_recursive(maze: list[str], coordinate: Coordinate, visited: set[Coordinate],
                        out_of_bounds: Callable[[Coordinate], bool]) -> set[Coordinate]:
    same_plot_coordinates = list(filter(
        lambda coord: coord not in visited and coordinates_same_plot(maze, coordinate, coord, out_of_bounds),
        surrounding_coordinates(coordinate)
    ))
    if not same_plot_coordinates:
        return visited
    visited.update(same_plot_coordinates)
    for new_coordinate in same_plot_coordinates:
        visited.update(fill_plot_recursive(maze, new_coordinate, visited, out_of_bounds))
    return visited


def surrounding_coordinates(coordinate):
    return (coordinate + direction.value for direction in Direction)


def coordinates_same_plot(maze: list[str], plot_coordinate: Coordinate, new_coordinate: Coordinate,
                          out_of_bounds: Callable[[Coordinate], bool]) -> bool:
    """
    Check if two coordinates are part of the same plot.

    Args:
        maze (list[str]): The maze represented as a list of strings.
        plot_coordinate (Coordinate): The coordinate of the plot.
        new_coordinate (Coordinate): The new coordinate to check.
        out_of_bounds (Callable[[Coordinate], bool]): A function to check if a coordinate is out of bounds.

    Returns:
        bool: True if both coordinates are part of the same plot, False otherwise.

    Examples:
        >>> maze = [
        ...     "AAAA",
        ...     "ABCD",
        ...     "ABCC",
        ...     "EEEC"
        ... ]
        >>> import map_handling
        >>> height, width = map_handling.get_map_dimensions(maze)
        >>> out_of_bounds = out_of_bounds_calculator(width, height)
        >>> coordinates_same_plot(maze, Coordinate(0, 0), Coordinate(1, 0), out_of_bounds)
        True
        >>> coordinates_same_plot(maze, Coordinate(0, 0), Coordinate(1, 1), out_of_bounds)
        False
        >>> coordinates_same_plot(maze, Coordinate(0, 0), Coordinate(-1, -1), out_of_bounds)
        False
    """
    if out_of_bounds(new_coordinate):
        return False
    if get_plot_type(maze, plot_coordinate) != get_plot_type(maze, new_coordinate):
        return False
    return True


def get_plot_type(maze: list[str], coordinate: Coordinate) -> str:
    return maze[coordinate.y][coordinate.x]


def fill_one_plot(maze: list[str], start_coordinate: Coordinate, out_of_bounds: Callable[[Coordinate], bool]) -> set[Coordinate]:
    """
    Recursively fills the plot starting from the given coordinate.

    Args:
        maze (list[str]): The maze represented as a list of strings.
        start_coordinate (Coordinate): The starting coordinate for filling the plot.
        out_of_bounds (Callable[[Coordinate], bool]): A function to check if a coordinate is out of bounds.

    Returns:
        Plot: The filled plot.

    Examples:
        >>> maze = [
        ...     "AAAA",
        ...     "ABCD",
        ...     "ABCC",
        ...     "EEEC"
        ... ]
        >>> import map_handling
        >>> height, width = map_handling.get_map_dimensions(maze)
        >>> out_of_bounds = out_of_bounds_calculator(width, height)
        >>> result1 = {Coordinate(0, 0), Coordinate(1, 0), Coordinate(2, 0), Coordinate(3, 0), Coordinate(0, 1), Coordinate(0, 2)}
        >>> fill_one_plot(maze, Coordinate(0, 0), out_of_bounds) == result1
        True
        >>> result2 = {Coordinate(1, 1), Coordinate(1, 2)}
        >>> fill_one_plot(maze, Coordinate(1, 1), out_of_bounds) == result2
        True
        >>> fill_one_plot(maze, Coordinate(3, 1), out_of_bounds)
        {Coordinate(3, 1)}
    """
    return fill_plot_recursive(maze, start_coordinate, {start_coordinate}, out_of_bounds)
