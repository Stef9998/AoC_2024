from day12.utils import surrounding_coordinates, fill_one_plot
from file_handling import input_as_lines
from map_handling import Coordinate, out_of_bounds_calculator, get_map_dimensions


def main(lines: list[str]) -> int:
    height, width = get_map_dimensions(lines)
    is_out_of_bounds = out_of_bounds_calculator(width, height)

    non_visited = {Coordinate(x, y) for y in range(height) for x in range(width)}
    fence_cost = 0
    while non_visited:
        next_non_visited = non_visited.pop()
        visited = fill_one_plot(lines, next_non_visited, is_out_of_bounds)
        non_visited -= visited
        area = len(visited)
        perimeter = get_plot_perimeter(visited)
        fence_cost += area * perimeter

    return fence_cost


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
    no_of_surrounding_coordinates = sum(
        1 for surrounding_coordinate in surrounding_coordinates(coordinate) if surrounding_coordinate in visited
    )
    return 4 - no_of_surrounding_coordinates


if __name__ == '__main__':
    result = main(input_as_lines())
    # result = main(input_as_lines('sample.txt'))
    print(f"Part one result:\n{result}")
    # assert result ==
