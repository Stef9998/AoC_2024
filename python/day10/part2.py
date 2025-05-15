from file_handling import get_file_as_lines, get_specific_file_as_lines

from map_handling import Coordinate
from utils import get_trail_starts

global height, width


def main(lines: list[str]) -> int:
    global height, width
    height = len(lines)
    width = len(lines[0])

    trail_map = [[int(char) for char in line] for line in lines]

    return score_of_map(trail_map)


def score_of_map(trail_map: list[list[int]]):
    start_coordinates = get_trail_starts(trail_map)
    return sum(score_of_trail(coordinate, 0, trail_map) for coordinate in start_coordinates)


def score_of_trail(coordinate: Coordinate, expected_number: int, trail_map: list[list[int]]):
    """
    Calculate the score of a trail starting from the given coordinate in the trail map.

    A trail is defined as a sequence of numbers starting from 0, where each subsequent number
    is exactly one greater than the previous one. The function recursively calculates the score
    by following the trail until it reaches the number 9 or no valid next step is found.

    Args:
        coordinate (Coordinate): The starting coordinate (row, column) of the trail.
        expected_number (int): The current number in the trail sequence.
        trail_map (list[list[int]]): A 2D list representing the trail map.

    Returns:
        int: The score of the trail.

    Examples:
        >>> init_global_variables_doctest(7,7)
        >>> trail_map_ = [
        ...     [-1,-1,-1, 0,-1,-1,-1],
        ...     [-1,-1,-1, 1,-1,-1,-1],
        ...     [-1,-1,-1, 2,-1,-1,-1],
        ...     [ 6, 5, 4, 3, 4, 5, 6],
        ...     [ 7,-1,-1,-1,-1,-1, 7],
        ...     [ 8,-1,-1,-1,-1,-1, 8],
        ...     [ 9,-1,-1,-1,-1,-1, 9],
        ... ]
        >>> score_of_trail(Coordinate(0, 3), 0, trail_map_)
        2
    """
    y = coordinate[0]
    x = coordinate[1]
    number = trail_map[y][x]
    if number != expected_number:
        return 0
    if number == 9:
        return 1
    return sum(
        score_of_trail(neighbor_coordinate, number + 1, trail_map)
        for neighbor_coordinate in get_surrounding_coordinates(coordinate)
    )


def get_surrounding_coordinates(coordinate: Coordinate) -> list[Coordinate]:
    """
    Get the surrounding coordinates of a given coordinate in a 2D grid.
    Surrounding means only up, down, left and right. Not diagonal.
    When the given coordinate is on the edge of the grid, only the ones inside will be returned

    Args:
        coordinate (Coordinate): The current coordinate (row, column).

    Returns:
        list[Coordinate]: A list of valid surrounding coordinates.

    Examples:
        >>> init_global_variables_doctest(5,5)
        >>> list(get_surrounding_coordinates(Coordinate(0, 0)))
        [Coordinate(1, 0), Coordinate(0, 1)]
        >>> list(get_surrounding_coordinates(Coordinate(4, 4)))
        [Coordinate(3, 4), Coordinate(4, 3)]
        >>> list(get_surrounding_coordinates(Coordinate(2, 2)))
        [Coordinate(1, 2), Coordinate(3, 2), Coordinate(2, 1), Coordinate(2, 3)]
        >>> list(get_surrounding_coordinates(Coordinate(0, 4)))
        [Coordinate(1, 4), Coordinate(0, 3)]
        >>> list(get_surrounding_coordinates(Coordinate(4, 0)))
        [Coordinate(3, 0), Coordinate(4, 1)]
    """
    global height, width
    possible_coordinates = [
        coordinate + Coordinate(-1,0) if coordinate[0] > 0 else None,
        coordinate + Coordinate(1,0) if coordinate[0] < height - 1 else None,
        coordinate + Coordinate(0,-1) if coordinate[1] > 0 else None,
        coordinate + Coordinate(0,1) if coordinate[1] < width - 1 else None,
    ]
    return list(filter(lambda x: x is not None, possible_coordinates))


def init_global_variables_doctest(h,w):
    global height, width
    height, width = h,w


if __name__ == '__main__':
    part_one_result = main(get_file_as_lines())
    # part_one_result = main(get_specific_file_as_lines('sample_input.txt'))
    print(f"Part one result:\n{part_one_result}")
    # assert part_one_result ==
